library(colorspace)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(patchwork)

source("partials/display.R")
region_sf <- readRDS('5_results/intermediates/region-sf.rds')

oco2_observations <- fst::read_fst("3_sif/intermediates/observations-sif.fst")
control_sif <- fst::read_fst("3_sif/intermediates/oco2-hourly-sif.fst")

table(control_sif$is_outlier)

control <- oco2_observations %>% 
  inner_join(control_sif, by = "observation_id") %>%
  rename(value = value.x, value_control = value.y) %>%
  mutate(month = lubridate::month(time))
control %>% glimpse()


# How do the SIF values compare by month?
p <- control %>% 
  ggplot(aes(x = value_control, y = value)) +
  geom_hex() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_fill_continuous_sequential(
    palette = "Viridis",
    name = "Count"
  ) +
  labs(x = "Control SIF", y = "OCO-2 SIF") +
  facet_wrap(~month, ncol = 4)

ggsave_base(
  "3_sif/figures/matching/oco2_vs_control_monthly.png", 
  p,
  width = 18, 
  height = 12
)

# Where and when are the outliers?
p <- control %>% 
  filter(is_outlier) %>% 
  ggplot(data = .) +
    geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
    geom_point(
      aes(x = longitude, y = latitude),
      color = "red",
      shape = 1,
      size = 0.25,
      alpha = 0.25
    ) +
    facet_wrap(~month, ncol = 3) +
    labs(x = element_blank(), y = element_blank())

ggsave_base(
  "3_sif/figures/matching/outlier_locations_monthly.png", 
  p,
  width = 18, 
  height = 12
)


# TODO: would be helpful to add the inventory data used in the fit to the
# location scatter plots again (not just the matched control values)

# How do the observations compare to the inventory relationship for 
# a sample of pixel-month cases?
plot_scatter_location <- function(nested_data, i, name) {
  row <- nested_data[i, ] 
  # match_data <- row %>% select(match_data) %>% unnest(cols = c(match_data))
  # fit_data <- row %>% select(fit_data) %>% unnest(cols = c(fit_data))
  data <- row %>% select(data) %>% unnest(cols = c(data))

  if(length(unique(data$intercept)) != 1) {
    print("Intercept is not unique")
  }
  if(length(unique(data$slope)) != 1) {
    print("Slope is not unique")
  }
  intercept <- data$intercept[1]
  slope <- data$slope[1]

  p_location <- ggplot() +
    geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
    geom_point(
      data = row,
      aes(x = model_longitude, y = model_latitude),
      color = "red",
      size = 3
    ) +
    labs(x = element_blank(), y = element_blank(), title = paste0("Month: ", row$month))

  specs <- paste0(
    "Intercept: ",
    round(intercept, 2),
    ", Slope: ",
    formatC(slope, format = "e", digits = 1)
  )
  p_data <- ggplot() +
    geom_hline(
      data = data,
      aes(yintercept = value, color = is_outlier),
      alpha = 0.4
    ) +
    scale_color_manual(
      "OCO-2",
      values = c("FALSE" = "orange", "TRUE" = "red"),
      labels = c("FALSE" = "Non-outlier", "TRUE" = "Outlier"),
      guide = guide_legend(
        order = 2,
        keywidth = 0.6,
        keyheight = 0.2,
        default.unit = "cm"
      )
    ) +
    # geom_point(
    #   data = data,
    #   aes(x = assim, y = value_control, shape = "Full Inventory"),
    #   color = "grey",
    #   alpha = 0.4,
    # ) +
    geom_point(
      data = data,
      aes(x = model_assim, y = value_control, shape = "Matched Control")
    ) +
    scale_shape_manual(
      "SiB4",
      values = c("Full Inventory" = 1, "Matched Control" = 16),
      guide = guide_legend(
        order = 1,
        keywidth = 0.6,
        keyheight = 0.2,
        default.unit = "cm"
      )
    ) +
    geom_abline(
      aes(intercept = intercept, slope = slope),
      color = "blue",
      linetype = "dashed"
    ) + 
    expand_limits(x = 0, y = 0) +
    labs(x = "GPP", y = "SIF", title = specs) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )

  fname <- paste0(
    "3_sif/figures/matching/",
    name,
    "/scatter_overlay_update_",
    i
  )
  ggsave(
    paste0(fname, ".png"),
    p_data + p_location,
    width = 8,
    height = 4,
  )
}

set.seed(42)
control_nested <- control %>% 
  nest(.by = c("model_longitude", "model_latitude", "month")) %>%
  sample_n(30)
  
  # %>%
  # rename(
  #   longitude = longitude.y,
  #   latitude = latitude.y,
  #   match_data = data
  # ) %>%
  # left_join(models, by = c("longitude", "latitude", "month")) %>%
  # rename(
  #   fit_data = data
  # )

for (i in seq_len(nrow(control_nested))) {
  plot_scatter_location(control_nested, i, "no_filter")
}
