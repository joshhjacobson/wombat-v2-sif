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
oco2_sif_nested <- oco2_sif %>% 
    group_by(lon, lat, month) %>%
    nest() %>%
    ungroup()

nested_summaries <- readr::read_rds("data/eda/sib4/summaries_grid_month.rds")

nested_data <- nested_summaries %>%
    select(lon, lat, month, data, tidy_out) %>%
    inner_join(oco2_sif_nested, by = c("lon", "lat", "month")) %>% 
    rename(data_sib4 = data.x, data_oco2 = data.y)

# Extract summaries and combine into a single tibble
df_intercept <- nested_data %>%
    unnest(cols = c(tidy_out)) %>%
    filter(term == "(Intercept)") %>%
    select(c(lon, lat, month, estimate)) %>%
    rename(intercept = estimate)


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
    df_sib4 <- row_data %>%
        select(data_sib4) %>%
        unnest(cols = c(data_sib4)) %>% 
        mutate(date = as.Date(time))
    df_oco2 <- row_data %>%
        select(data_oco2) %>%
        unnest(cols = c(data_oco2)) %>%
        left_join(df_sib4, by = c("date"))
    p_data <- ggplot() +
        geom_point(
            data = df_sib4, 
            aes(x = assim, y = sif, color = "SiB4", shape = "SiB4"), 
            alpha = 0.5
        ) +
        geom_point(
            data = df_oco2, 
            aes(x = assim, y = oco_sif, color = "OCO-2", shape = "OCO-2")
        ) +
        expand_limits(x = 0, y = 0) +
        scale_color_manual(
            element_blank(), 
            values = c("SiB4" = "black", "OCO-2" = "red")
        ) +
        scale_shape_manual(
            element_blank(), 
            values = c("SiB4" = 1, "OCO-2" = 16)
        ) +
        labs(x = "GPP", y = "SIF", title = specs) +
        theme(legend.position = "bottom")

    fname <- paste(
        "0_eda/figures/scatter_with_oco2/scatter_grid_monthly",
        type_name,
        i,
        sep = "_"
    )
    ggsave(
        paste0(fname, ".png"),
        p_data + p_location,
        width = 8,
        height = 3,
    )
}



## Negative intercepts (with OCO-2)
set.seed(20231023)
metrics_intercept_neg_south <- df_intercept %>%
    filter(intercept < -0.1, between(lat, -30, 0)) %>%
    sample_n(15)
metrics_intercept_neg_north <- df_intercept %>%
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
    "0_eda/figures/locations_neg_intercept_with_oco2.png",
    p_locations_full,
    width = 12,
    height = 6
)

nested_data_neg <- nested_data %>%
    select(-tidy_out) %>%
    inner_join(metrics_intercept_neg, by = c("lon", "lat", "month")) %>%
    group_by(lon, lat, month) %>%
    nest() %>%
    ungroup()

for (i in seq_len(nrow(nested_data_neg))) {
    plot_scatter_location(nested_data_neg, i, "neg_intercept")
}


## Positive intercepts (with OCO-2)
set.seed(20231023)
metrics_intercept_pos_south <- df_intercept %>%
    filter(intercept > 0.1, between(lat, -30, 0)) %>%
    sample_n(15)
metrics_intercept_pos_north <- df_intercept %>%
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
    "0_eda/figures/locations_pos_intercept_with_oco2.png",
    p_locations_full,
    width = 12,
    height = 6
)

nested_data_pos <- nested_data %>%
    select(-tidy_out) %>%
    inner_join(metrics_intercept_pos, by = c("lon", "lat", "month")) %>%
    group_by(lon, lat, month) %>%
    nest() %>%
    ungroup()

for (i in seq_len(nrow(nested_data_pos))) {
    plot_scatter_location(nested_data_pos, i, "pos_intercept")
}


## Near-zero intercepts (with OCO-2)
set.seed(20231026)
metrics_intercept_zero <- df_intercept %>%
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
    "0_eda/figures/locations_zero_intercept_with_oco2.png",
    p_locations_full,
    width = 12,
    height = 6
)

nested_data_zero <- nested_data %>%
    select(-tidy_out) %>%
    inner_join(metrics_intercept_zero, by = c("lon", "lat", "month")) %>%
    group_by(lon, lat, month) %>%
    nest() %>%
    ungroup()

for (i in seq_len(nrow(nested_data_zero))) {
    plot_scatter_location(nested_data_zero, i, "zero_intercept")
}
