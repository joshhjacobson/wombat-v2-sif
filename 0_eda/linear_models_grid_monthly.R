library(broom)
library(dplyr)
library(furrr)
library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
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


# Compute the correlation between sif and assim for each nested tibble
sif_assim_cor <- function(df) {
    cor(df$assim, df$sif, method = "pearson")
}

nested_cor <- sib4_land_nested %>%
    mutate(p_cor = future_map(data, sif_assim_cor))

# readr::write_rds(nested_cor, "data/eda/sib4/cor_grid_month.rds")
# nested_cor <- readr::read_rds("data/eda/sib4/cor_grid_month.rds")

df_cor <- nested_cor %>%
    unnest(cols = c(p_cor)) %>%
    select(c(lon, lat, month, p_cor))

## Plot correlation coef. at each location
p <- ggplot() +
    geom_raster(data = df_cor, aes(x = lon, y = lat, fill = p_cor)) +
    coord_sf(
        crs = sf::st_crs(4326),
        default_crs = sf::st_crs(4326)
    ) +
    scale_fill_distiller(
        "Correlation",
        type = "div",
        palette = "RdYlBu",
        direction = -1,
        limits = c(-1, 1)
    ) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/cor_grid_monthly.png",
    p,
    width = 18,
    height = 12
)



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
# nested_summaries <- readr::read_rds("data/eda/sib4/summaries_grid_month.rds")


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

df_rsquared <- nested_summaries %>%
    unnest(cols = c(glance_out)) %>%
    select(c(lon, lat, month, adj.r.squared))

df_fit_metrics <- list(df_intercept, df_slope, df_sigma, df_rsquared) %>%
    purrr::reduce(inner_join, by = c("lon", "lat", "month")) %>%
    rename(intercept = estimate.x, slope = estimate.y, std_err = sigma)

# Map the results

## INTERCEPT
p <- ggplot() +
    geom_raster(data = df_fit_metrics, aes(x = lon, y = lat, fill = intercept)) +
    coord_sf(
        crs = sf::st_crs(4326),
        default_crs = sf::st_crs(4326)
    ) +
    scale_fill_distiller(
        "Intercept",
        type = "div",
        palette = "RdYlBu"
    ) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_intercept.png",
    p,
    width = 18,
    height = 12
)

p <- ggplot() +
    geom_raster(data = df_fit_metrics, aes(x = lon, y = lat, fill = intercept)) +
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
    labs(x = NULL, y = NULL) +
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
    geom_raster(data = df_fit_metrics, aes(x = lon, y = lat, fill = slope)) +
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
    labs(x = NULL, y = NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_slope_cut_pos.png",
    p,
    width = 18,
    height = 12
)

p <- ggplot() +
    geom_raster(data = df_fit_metrics, aes(x = lon, y = lat, fill = slope)) +
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
    labs(x = NULL, y = NULL) +
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
    geom_raster(data = df_slope_neg, aes(x = lon, y = lat, fill = log_slope)) +
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
    labs(x = NULL, y = NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_log-neg-slope.png",
    p,
    width = 18,
    height = 12
)

p <- ggplot() +
    geom_raster(data = df_slope_pos, aes(x = lon, y = lat, fill = log_slope)) +
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
    labs(x = NULL, y = NULL) +
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
    geom_raster(data = df_fit_metrics, aes(x = lon, y = lat, fill = std_err)) +
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
    labs(x = NULL, y = NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_stderr.png",
    p,
    width = 18,
    height = 12
)


## Adj R Squared
p <- ggplot() +
    geom_raster(data = df_fit_metrics, aes(x = lon, y = lat, fill = adj.r.squared)) +
    coord_sf(
        crs = sf::st_crs(4326),
        default_crs = sf::st_crs(4326)
    ) +
    scale_fill_distiller(
        "Adj. R^2",
        type = "seq",
        palette = "YlGnBu",
        direction = 1,
        # limits = c(0, 0.15)
    ) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~month, ncol = 3)
p

ggsave_base(
    "0_eda/figures/lms_grid_monthly_adjrsq.png",
    p,
    width = 18,
    height = 12
)


# Check grid cell scatter plots for different intercept cases

## Collect world coastline data
world <- ne_coastline(scale = 110, returnclass = "sf")

plot_scatter_location <- function(nested_data, i, type_name) {
    row_data <- nested_data[i, ] %>% unnest(cols = c(data))
    p_location <- ggplot(data = world) +
        geom_sf() +
        geom_point(
            data = row_data,
            aes(x = lon, y = lat),
            color = "red",
            size = 3
        ) +
        labs(x = element_blank(), y = element_blank(), title = "Location")

    specs <- paste(
        "Month: ",
        row_data$month,
        " |  Intercept: ",
        round(row_data$intercept, 3)
    )
    df_location <- row_data %>%
        select(data) %>%
        unnest(cols = c(data))
    p_data <- ggplot(data = df_location) +
        geom_point(aes(x = assim, y = sif)) +
        expand_limits(x = 0, y = 0) +
        labs(x = "GPP", y = "SIF", title = specs)

    fname <- paste(
        "0_eda/figures/scatter/scatter_grid_monthly",
        type_name,
        i,
        sep = "_"
    )
    ggsave(
        paste0(fname, ".png"),
        p_data + p_location,
        width = 9,
        height = 3,
    )
}


## Negative intercepts
set.seed(20231023)
metrics_intercept_neg_south <- df_fit_metrics %>%
    filter(intercept < -0.1, between(lat, -30, 0)) %>%
    sample_n(15)
metrics_intercept_neg_north <- df_fit_metrics %>%
    filter(intercept < -0.1, between(lat, 50, 70)) %>%
    sample_n(15)
metrics_intercept_neg <- metrics_intercept_neg_south %>%
    bind_rows(metrics_intercept_neg_north)


p_locations_full <- ggplot(data = world) +
    geom_sf() +
    geom_point(data = metrics_intercept_neg, aes(x = lon, y = lat), color = "red") +
    coord_sf(
        crs = sf::st_crs(4326),
        default_crs = sf::st_crs(4326)
    ) +
    labs(
        x = element_blank(),
        y = element_blank(),
        title = "Sample locations with intercept < -0.1 [SIF Units]"
    )
p_locations_full

ggsave_base(
    "0_eda/figures/locations_neg_intercept.png",
    p_locations_full,
    width = 12,
    height = 6
)

sib4_land_nested_neg <- sib4_land_nested %>%
    inner_join(metrics_intercept_neg, by = c("lon", "lat", "month")) %>%
    group_by(lon, lat, month) %>%
    nest() %>%
    ungroup()

for (i in seq_len(nrow(sib4_land_nested_neg))) {
    plot_scatter_location(sib4_land_nested_neg, i, "neg_intercept")
}


## Positive intercepts
set.seed(20231023)
metrics_intercept_pos_south <- df_fit_metrics %>%
    filter(intercept > 0.1, between(lat, -30, 0)) %>%
    sample_n(15)
metrics_intercept_pos_north <- df_fit_metrics %>%
    filter(intercept > 0.1, between(lat, 50, 70)) %>%
    sample_n(15)
metrics_intercept_pos <- metrics_intercept_pos_south %>%
    bind_rows(metrics_intercept_pos_north)


p_locations_full <- ggplot(data = world) +
    geom_sf() +
    geom_point(data = metrics_intercept_pos, aes(x = lon, y = lat), color = "red") +
    coord_sf(
        crs = sf::st_crs(4326),
        default_crs = sf::st_crs(4326)
    ) +
    labs(
        x = element_blank(),
        y = element_blank(),
        title = "Sample locations with intercept > 0.1 [SIF Units]"
    )
p_locations_full

ggsave_base(
    "0_eda/figures/locations_pos_intercept.png",
    p_locations_full,
    width = 12,
    height = 6
)

sib4_land_nested_pos <- sib4_land_nested %>%
    inner_join(metrics_intercept_pos, by = c("lon", "lat", "month")) %>%
    group_by(lon, lat, month) %>%
    nest() %>%
    ungroup()

for (i in seq_len(nrow(sib4_land_nested_pos))) {
    plot_scatter_location(sib4_land_nested_pos, i, "pos_intercept")
}


## Near-zero intercepts
set.seed(20231026)
metrics_intercept_zero <- df_fit_metrics %>%
    filter(between(intercept, -0.1, 0.1)) %>%
    sample_n(30)

p_locations_full <- ggplot(data = world) +
    geom_sf() +
    geom_point(data = metrics_intercept_zero, aes(x = lon, y = lat), color = "red") +
    coord_sf(
        crs = sf::st_crs(4326),
        default_crs = sf::st_crs(4326)
    ) +
    labs(
        x = element_blank(),
        y = element_blank(),
        title = "Sample locations with intercept in (-0.1, 0.1) [SIF Units]"
    )
p_locations_full

ggsave_base(
    "0_eda/figures/locations_zero_intercept.png",
    p_locations_full,
    width = 12,
    height = 6
)

sib4_land_nested_zero <- sib4_land_nested %>%
    inner_join(metrics_intercept_zero, by = c("lon", "lat", "month")) %>%
    group_by(lon, lat, month) %>%
    nest() %>%
    ungroup()

for (i in seq_len(nrow(sib4_land_nested_zero))) {
    plot_scatter_location(sib4_land_nested_zero, i, "zero_intercept")
}
