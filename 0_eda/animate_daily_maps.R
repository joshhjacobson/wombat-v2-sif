library(stars)
library(clock)
library(dplyr)
library(ggplot2)
library(gganimate)

source("partials/display.R")

plot_stars <- function(
    data,
    variable,
    breaks,
    limits,
    palette,
    show_excess = TRUE,
    label_precision = "0",
    drop_second_labels = FALSE,
    symmetric = TRUE,
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
        labels[seq(2, length(breaks), by = 2)] <- ""
    }
    ggplot() +
        geom_stars(
            data = data,
            mapping = aes(fill = {{ variable }})
        ) +
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
        scale_fill_fermenter_n(
            breaks = breaks,
            palette = palette,
            direction = 1,
            n_colours = 9,
            limits = limits,
            labels = labels,
            guide = guide_coloursteps(
                direction = "horizontal",
                label.position = "bottom",
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


TIME_RESOLUTION <- "daily"
months <- date_seq(
    from = as.POSIXct("2016-01-01", "UTC"),
    to = as.POSIXct("2019-01-01", "UTC"),
    by = duration_months(1)
)

# Get grid areas and compute weights
# area <- read_ncdf("data/area-1x1.nc")
# total_area <- sum(area$cell_area)
# area_weights <- st_apply(area, c("lon", "lat"), function(a) a / total_area)
# names(area_weights) <- "area_weight"

# Read in data, filter to 2015-2016, and combine with area weights
path <- paste0("data/eda/sib4/", TIME_RESOLUTION, "/")
file_list <- list.files(path, full.names = TRUE)


sib4 <- read_stars(file_list, sub = c("sif_nonzero", "assim_nonzero"), along = "time") %>%
    st_set_dimensions(names = c("lon", "lat", "time")) %>%
    st_set_crs(4326)
#   st_join(area_weights) %>%
#   mutate(assim_weighted = assim_nonzero * area_weight)

days <- as.Date(st_get_dimension_values(sib4, "time"))
sib4 <- sib4 %>% st_set_dimensions(which = "time", values = days, names = "day")

for (i in seq_along(months)) {
    first_day  <- months[i]
    last_day  <- first_day %>% add_months(1) %>% add_days(-1)
    sib4_month <- sib4 %>% filter(between(day, first_day, last_day))
    nframes <- dim(sib4_month)["day"]
    year_month <- date_format(first_day, format = "%Y-%m")
    print(year_month)

    # Create SIF animation
    sib4_month %>%
        plot_stars(
            .,
            sif_nonzero,
            seq(0, 1.2, 0.1),
            c(0, 1.2),
            palette = "OrRd",
            show_excess = FALSE,
            label_precision = "2",
            drop_second_labels = TRUE,
            symmetric = FALSE,
            bar_width = 13
        ) +
        transition_time(day) +
        labs(fill = "SIF", title = "{frame_time}") +
        theme(plot.title = element_text(hjust = 0.5)) -> p_sif

    anim_sif <- animate(
        p_sif,
        nframes = nframes,
        fps = 2,
        height = 12,
        width = 14,
        res = 320,
        units = "cm"
    )
    anim_save(paste0("sif_daily_", year_month, ".gif"), path = "./0_eda/figures/sif_animations")

    # Create GPP animation
    sib4_month %>%
        plot_stars(
            .,
            assim_nonzero,
            seq(0, 20, 2),
            c(0, 20),
            palette = "YlGn",
            show_excess = FALSE,
            label_precision = "0",
            drop_second_labels = TRUE,
            symmetric = FALSE,
            bar_width = 13
        ) +
        transition_time(day) +
        labs(fill = "GPP", title = "{frame_time}") +
        theme(plot.title = element_text(hjust = 0.5)) -> p_gpp

    anim_gpp <- animate(
        p_gpp,
        nframes = nframes,
        fps = 2,
        height = 12,
        width = 14,
        res = 320,
        units = "cm"
    )
    anim_save(paste0("gpp_daily_", year_month, ".gif"), path = "./0_eda/figures/gpp_animations")

}
