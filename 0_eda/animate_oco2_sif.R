library(dplyr)
library(ggplot2)
library(gganimate)
library(stars)

source("partials/display.R")

plot_raster <- function(
    data,
    variable,
    breaks,
    limits,
    palette,
    show_excess = TRUE,
    label_precision = "0",
    drop_second_labels = FALSE,
    bar_width = 13) {
    base_labels <- sprintf(paste0("%.", label_precision, "f"), breaks)
    labels <- if (show_excess) {
        ifelse(
            abs(breaks) == max(abs(breaks)),
            sprintf(
                paste0("%s%.", label_precision, "f"),
                ifelse(breaks < 0, "<-", ">"),
                max(breaks)
            ),
            base_labels
        )
    } else {
        base_labels
    }
    if (drop_second_labels) {
        labels[seq(1, length(breaks), by = 2)] <- ""
    }
    ggplot() +
        geom_stars(
            data = data,
            mapping = aes(x = lon, y = lat, fill = {{ variable }})
        ) +
        coord_sf(ylim = c(-90, 90), default_crs = sf::st_crs(4326)) +
        geom_segment(
            data = data.frame(y = c(-23, 23, 50)),
            mapping = aes(x = -180, y = y, xend = 180, yend = y),
            colour = "black",
            linetype = "dashed",
            linewidth = 0.4
        ) +
        geom_text(
            data = data.frame(
                x = c(-175, -175, -175),
                y = c(-23, 23, 50),
                label = c("23°S", "23°N", "50°N")
            ),
            mapping = aes(x = x, y = y, label = label),
            nudge_y = 10
        ) +
        scale_fill_gradient2(
            breaks = breaks,
            # type = "div",
            # palette = palette,
            # direction = -1,
            # n_colours = 9,
            low = "blue",
            mid = "white",
            high = "red",
            limits = limits,
            labels = labels,
            guide = guide_coloursteps(
                direction = "horizontal",
                label.position = "bottom",
                title.position = "top",
                axis = FALSE,
                label.theme = element_text(size = 8),
                frame.colour = "#ffffff",
                barwidth = bar_width,
                even.steps = FALSE
            ),
            na.value = "#cccccc"
        ) +
        theme(
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom"
        )
}


lon_labs <- seq(-179.5, 179.5, 1)
lat_labs <- seq(-89.5, 89.5, 1)
date_labs <- clock::date_seq(
    from = as.Date("2015-01-01"),
    to = as.Date("2020-12-01"),
    by = clock::duration_months(1)
)
dims_grid <- tidyr::expand_grid(lon_labs, lat_labs, date_labs) %>% 
    rename(lon = lon_labs, lat = lat_labs, date = date_labs)

oco2_sif <- fst::read.fst("data/eda/oco2/oco2_sif_2014-2020_daily_1deg.fst") %>%
    as_tibble() %>%
    select(date, longitude, latitude, value_daily) %>%
    rename(lon = longitude, lat = latitude, oco_sif = value_daily) %>%
    mutate(
        date = lubridate::floor_date(date, unit = "month")
    ) %>%
    group_by(lon, lat, date) %>%
    summarise(
        oco_sif = mean(oco_sif),
        n_values = n()
    ) %>%
    ungroup() %>%
    right_join(dims_grid, by = c("lon", "lat", "date")) %>% 
    st_as_stars(dims = c("lon", "lat", "date"))


oco2_sif %>%
    # filter(between(year_month, as.Date("2014-10-01"), as.Date("2015-02-01"))) %>%
    # filter(date == as.Date("2017-10-01")) %>%
    plot_raster(
        .,
        oco_sif,
        seq(-0.5, 3, 0.1),
        c(-0.5, 3),
        palette = "RdYlBu",
        show_excess = FALSE,
        label_precision = "1",
        drop_second_labels = TRUE,
        bar_width = 18
    ) +
    transition_time(date) +
    labs(fill = "SIF", title = "OCO-2 SIF (monthly, 1-deg): {frame_time}") +
    theme(plot.title = element_text(hjust = 0.5)) -> p_sif

# p_sif

anim_sif <- animate(
    p_sif,
    nframes = length(date_labs),
    fps = 1,
    height = 12,
    width = 14,
    res = 320,
    units = "cm"
)
anim_save(paste0("oco2_sif_monthly_1deg_2.gif"), path = "./0_eda/figures/sif_animations")
