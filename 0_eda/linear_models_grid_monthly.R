library(broom)
library(dplyr)
library(furrr)
library(ggplot2)
library(stars)
library(tidyr)

source("partials/display.R")

# Set up parallel processing
plan(multisession, workers = 64)
options <- furrr_options(packages = c("dplyr", "broom"))

# Get SiB4 daily data without double-zeros removed
path <- "data/eda/sib4/daily_full"
file_list <- list.files(path, full.names = TRUE)

sib4 <- read_stars(file_list, sub = c("sif", "assim"), along = "time") %>%
    st_set_dimensions(names = c("lon", "lat", "time")) %>%
    st_set_crs(4326)

# Read in the TRANSCOM3 regions, create land mask, and apply to sib4
sib4_grid <- sib4 %>% slice(time, 1)

transcom_path <- "data/TRANSCOM_mask_original_1x1.nc"
transcom_mask <- read_ncdf(transcom_path, var = "mask64") %>%
    st_set_dimensions(names = c("lon", "lat")) %>%
    mutate(regions = as.integer(mask64)) %>%
    select(regions) %>%
    st_warp(sib4_grid)

land_mask <- transcom_mask %in% seq(11)

sib4_land <- sib4
sib4_land[!land_mask] <- NA


# Convert from stars to tibble, drop masked values, and nest by lon, lat, and month
sib4_land_nested <- sib4_land %>%
    units::drop_units() %>%
    as_tibble() %>%
    na.omit() %>%
    mutate(month = clock::get_month(time)) %>%
    group_by(lon, lat, month) %>%
    nest() %>%
    ungroup()

# readr::write_rds(sib4_land_nested, "data/eda/sib4/sib4_land_nested.rds")
# sib4_land_nested <- readr::read_rds("data/eda/sib4/sib4_land_nested.rds")


# Fit linear models to each nested tibble; extract coefficients and std errors

sif_model <- function(df) {
    lm(sif ~ assim, data = df)
}

nested_models <- sib4_land_nested %>%
    mutate(model = future_map(data, sif_model))

# readr::write_rds(nested_models, "data/eda/sib4/lms_grid_month.rds")
# nested_models <- readr::read_rds("data/eda/sib4/lms_grid_month.rds")

nested_summaries <- nested_models %>%
    mutate(
        tidy_out = future_map(model, tidy, .options = options),
        glance_out = future_map(model, glance, .options = options)
    )

# readr::write_rds(nested_summaries, "data/eda/sib4/summaries_grid_month.rds")


# Extract summaries and combine into a single tibble
df_intercept <- nested_summaries %>%
    unnest(cols = c(tidy_out)) %>%
    filter(term == "(Intercept)") %>%
    select(c(lon, lat, month, estimate))

df_slope <- nested_summaries %>%
    unnest(cols = c(tidy_out)) %>%
    filter(term == "assim") %>%
    select(c(lon, lat, month, estimate))

df_sigma <- nested_summaries %>%
    unnest(cols = c(glance_out)) %>%
    select(c(lon, lat, month, sigma))

df_fit_metrics <- list(df_intercept, df_slope, df_sigma) %>%
    purrr::reduce(inner_join, by = c("lon", "lat", "month")) %>% 
    rename(intercept = estimate.x, slope = estimate.y, std_err = sigma)

# Map the results

## INTERCEPT
p <- ggplot() +
    geom_raster(data = df_fit_metrics, aes(x=lon, y=lat, fill = intercept)) +
    coord_sf(
        crs = sf::st_crs(4326), 
        default_crs = sf::st_crs(4326)
    ) +
    scale_fill_distiller(
        "Intercept", 
        type = "div",
        palette = "RdYlBu"
    ) +
    labs(x=NULL, y=NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_intercept.png",
    p, 
    width = 18, 
    height = 12
)

p <- ggplot() +
    geom_raster(data = df_fit_metrics, aes(x=lon, y=lat, fill = intercept)) +
    coord_sf(
        crs = sf::st_crs(4326), 
        default_crs = sf::st_crs(4326)
    ) +
    scale_fill_distiller(
        "Intercept (cut)", 
        type = "div",
        palette = "RdYlBu", 
        limits = c(-0.5, 0.5),
    ) +
    labs(x=NULL, y=NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_intercept_cut.png",
    p, 
    width = 18, 
    height = 12
)

## SLOPE
p <- ggplot() +
    geom_raster(data = df_fit_metrics, aes(x=lon, y=lat, fill = slope)) +
    coord_sf(
        crs = sf::st_crs(4326), 
        default_crs = sf::st_crs(4326)
    ) +
    scale_fill_distiller(
        "Slope (cut +)", 
        type = "seq",
        palette = "YlGnBu",
        direction = 1,
        limits = c(0, 0.5)
    ) +
    labs(x=NULL, y=NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_slope_cut_pos.png",
    p, 
    width = 18, 
    height = 12
)

p <- ggplot() +
    geom_raster(data = df_fit_metrics, aes(x=lon, y=lat, fill = slope)) +
    coord_sf(
        crs = sf::st_crs(4326), 
        default_crs = sf::st_crs(4326)
    ) +
    scale_fill_distiller(
        "Slope (cut)", 
        type = "div",
        palette = "RdYlBu",
        limits = c(-0.5, 0.5)
    ) +
    labs(x=NULL, y=NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_slope_cut.png",
    p, 
    width = 18, 
    height = 12
)

df_slope_neg <- df_fit_metrics %>% 
    filter(slope < 0) %>% 
    mutate(log_slope = log(-slope))

df_slope_pos <- df_fit_metrics %>% 
    filter(slope > 0) %>% 
    mutate(log_slope = log(slope))

p <- ggplot() +
    geom_raster(data = df_slope_neg, aes(x=lon, y=lat, fill = log_slope)) +
    coord_sf(
        crs = sf::st_crs(4326), 
        default_crs = sf::st_crs(4326)
    ) +
    scale_fill_distiller(
        "log(-Slope)", 
        type = "div",
        palette = "RdYlBu",
        limits = c(-10, 10)
    ) +
    labs(x=NULL, y=NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_log-neg-slope.png",
    p, 
    width = 18, 
    height = 12
)

p <- ggplot() +
    geom_raster(data = df_slope_pos, aes(x=lon, y=lat, fill = log_slope)) +
    coord_sf(
        crs = sf::st_crs(4326), 
        default_crs = sf::st_crs(4326)
    ) +
    scale_fill_distiller(
        "log(Slope)", 
        type = "div",
        palette = "RdYlBu",
        limits = c(-30, 30)
    ) +
    labs(x=NULL, y=NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_log-pos-slope.png",
    p, 
    width = 18, 
    height = 12
)

## STD ERR
p <- ggplot() +
    geom_raster(data = df_fit_metrics, aes(x=lon, y=lat, fill = std_err)) +
    coord_sf(
        crs = sf::st_crs(4326), 
        default_crs = sf::st_crs(4326)
    ) +
    scale_fill_distiller(
        "Std. Error", 
        type = "seq",
        palette = "YlGnBu",
        direction = 1,
        limits = c(0, 0.15)
    ) +
    labs(x=NULL, y=NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_stderr.png",
    p, 
    width = 18, 
    height = 12
)
