library(dplyr)
library(ggplot2)
library(gganimate)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)

source("partials/display.R")

# Read in SiB4 and OCO-2 data, and TransCom mask
oco2_sif <- fst::read.fst("data/eda/oco2/oco2_sif_2014-2020_daily_1deg.fst") %>% 
    as_tibble() %>% 
    select(date, longitude, latitude, value_daily) %>%
    rename(lon = longitude, lat = latitude, oco_sif = value_daily) %>% 
    mutate(month = clock::get_month(date))

sib4_data <- readr::read_rds("data/eda/sib4/sib4_land_nested.rds") %>% 
    unnest(cols = c("data")) %>% 
    mutate(date = as.Date(time))

transcom_mask <- stars::read_ncdf("data/TRANSCOM_mask_original_1x1.nc", var = "mask64") %>%
    mutate(region = as.integer(mask64)) %>% 
    select(region) %>%
    as_tibble() %>%
    rename(lon = longitude, lat = latitude) %>%
    mutate(lon = lon + 0.5)

df_daily <- sib4_data %>% 
    inner_join(oco2_sif, by = c("lon", "lat", "date")) %>%
    select(-c("month.x", "month.y", "time")) %>% 
    left_join(transcom_mask, by = c("lon", "lat"))


df_daily_sub <- df_daily %>% 
    filter(between(date, as.Date("2016-01-01"), as.Date("2018-12-31")))

dates <- clock::date_seq(
    from = as.Date("2016-01-01"),
    to = as.Date("2018-12-31"),
    by = clock::duration_days(1)
)

# Animate global scatter plot
ggplot(data = df_daily_sub) +
    geom_point(aes(x = sif, y = oco_sif), alpha = 0.2, shape = 16) +
    coord_fixed(ratio = 1, xlim = c(0, 5), ylim = c(-1, 5)) +
    transition_time(date) +
    labs(
        x = "SiB4 SIF",
        y = "OCO-2 SIF", 
        title = "Global 1-deg, Daily\nSiB4 vs OCO-2 SIF: {frame_time}"
    ) -> scatter_global

anim <- animate(
    scatter_global,
    nframes = length(dates),
    fps = 10,
    height = 10,
    width = 8,
    res = 300,
    units = "cm"
)
anim_save("scatter_sib4_sif_oco2_sif_global.gif", path = "./0_eda/figures/")

# Repeat the above with a smaller coordinate window
ggplot(data = df_daily_sub) +
    geom_point(aes(x = sif, y = oco_sif), alpha = 0.2, shape = 16) +
    coord_fixed(ratio = 1, xlim = c(0, 2), ylim = c(-0.5, 2)) +
    transition_time(date) +
    labs(
        x = "SiB4 SIF",
        y = "OCO-2 SIF", 
        title = "Global 1-deg, Daily\nSiB4 vs OCO-2 SIF: {frame_time}"
    ) -> scatter_global_cut

anim <- animate(
    scatter_global_cut,
    nframes = length(dates),
    fps = 10,
    height = 10,
    width = 9,
    res = 300,
    units = "cm"
)
anim_save("scatter_sib4_sif_oco2_sif_global_cut.gif", path = "./0_eda/figures/")


# Animate scatter plots by region
df_daily_sub_land_padded <- df_daily_sub %>% 
    filter(region %in% seq(11)) %>% 
    select(-c(lon, lat)) %>%
    mutate(display = 1) %>%
    complete(date, region, fill = list(assim = 0, sif = 0, oco_sif = 0, display = 0)) %>%
    mutate(display = as.factor(display))

ggplot(data = df_daily_sub_land_padded) +
    geom_point(aes(x = sif, y = oco_sif, color = display), alpha = 0.2, shape = 16) +
    coord_fixed(ratio = 1, xlim = c(0, 2), ylim = c(-0.5, 2)) +
    facet_wrap(~region, nrow = 2) +
    transition_time(date) +
    scale_color_manual(values = c("white", "black")) +
    scale_x_continuous(breaks = seq(0, 2, 1)) +
    scale_y_continuous(breaks = seq(0, 2, 1)) +
    labs(
        x = "SiB4 SIF",
        y = "OCO-2 SIF", 
        title = "Global 1-deg, Daily SiB4 vs OCO-2 SIF: {frame_time}"
    ) +
    theme(legend.position = "none") -> scatter_regional

anim <- animate(
    scatter_regional,
    nframes = length(dates),
    fps = 10,
    height = 9,
    width = 16,
    res = 300,
    units = "cm"
)
anim_save("scatter_sib4_sif_oco2_sif_regional.gif", path = "./0_eda/figures/")



# Join SiB4 and OCO-2 data, plot scatter plots by day (global and regional)
df <- nested_summaries %>%
    select(lon, lat, month, data) %>%
    inner_join(oco2_sif, by = c("lon", "lat", "month")) %>% 
    rename(data_sib4 = data.x, data_oco2 = data.y)
df



# Nest OCO-2 data by lon, lat, month
# Inner join with nested SiB4 data
# Plot scatter plots by intercept categories where data is sufficient

nested_summaries <- readr::read_rds("data/eda/sib4/summaries_grid_month.rds")

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

