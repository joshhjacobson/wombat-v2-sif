library(dplyr, warn.conflicts = FALSE)
library(stars)

source('partials/utils.R')
source('partials/display.R')

args <- list(
  model_sif_assim = '3_sif/intermediates/model-sif-assim.fst',
  output_map = '6_results_sif/figures/slides/sif-gpp-average-slope-map.pdf',
  output_ts = '6_results_sif/figures/slides/sif-gpp-average-slope-ts.pdf'
)

model <- fst::read_fst(args$model_sif_assim)
earth_bbox_sf <- rnaturalearth::ne_download(
  category = 'physical',
  type = 'wgs84_bounding_box',
  returnclass = 'sf'
)

W_to_mW  <- 1e-6
model_slope <- model %>%
  select(
    longitude = model_longitude,
    latitude = model_latitude,
    month,
    value = slope
  ) %>%
  filter(abs(latitude) != 89.5) %>%
  mutate(
    month = factor(month.abb[month], levels = month.abb),
    value = W_to_mW * value
  ) %>%
  # HACK(jhj): Remove 5 outliers in March
  filter(value < 10)


model_stars <- model_slope %>%
  group_by(longitude, latitude) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
  arrange(longitude, latitude) %>%
  st_as_stars(dims = c('longitude', 'latitude')) %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012')

model_season <- model_slope %>%
  group_by(month) %>%
  summarise(
    n = n(),
    value_q25 = quantile(value, 0.25, na.rm = TRUE),
    value_q75 = quantile(value, 0.75, na.rm = TRUE),
    value = mean(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    month_label = sprintf('%s\n(%d)', month, n)
  )

output_map <- plot_map_slides(
  model_stars,
  value,
  seq(0, 4, by = 0.5),
  c(0, 4),
  'batlow',
  show_excess = TRUE,
  label_precision = 1,
  drop_second_labels = TRUE,
  symmetric = FALSE,
  reverse = FALSE,
  bar_width = 13
) +
  labs(fill = expression('Slope [(mW'~m^{-2}~µm^{-1}~sr^{-1}*') / (kgCO'[2]~m^{-2}~s^{-1}*')]')) +
  theme(
    panel.border = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(
      size = 9,
      margin = margin(0, 0, 0.1, 0, unit = 'cm')
    ),
    legend.margin = margin(t = -0.3, r = 0, b = 0, l = 0, unit = 'cm'),
    plot.title = element_blank(),
    plot.margin = margin(t = 0, b = 0, l = 0, r = 0, unit = 'cm')
  )

ggsave_base(
  args$output_map,
  output_map,
  width = 12,
  height = 8
)

output_season <- model_season %>%
  ggplot(aes(x = month)) +
    geom_pointrange(
      aes(
        y = value,
        ymin = value_q25,
        ymax = value_q75
      ),
      colour = 'grey15',
      shape = 15,
      size = 0.3,
      linewidth = 0.6
    ) +
    scale_x_discrete(
      breaks = model_season$month,
      labels = model_season$month_label
    ) +
    labs(x = NULL, y = 'Slope') +
    theme(
      axis.title.y = element_text(size = 9, colour = '#23373b'),
      axis.text.y = element_text(size = 7, colour = '#23373b'),
      axis.text.x = element_text(size = 8, colour = '#23373b'),
      plot.title = element_blank(),
      plot.margin = margin(t = 0.1, b = 0.1, l = 0.05, r = 0.1, unit = 'cm')
    )

ggsave_base(
  args$output_ts,
  output_season,
  width = 14,
  height = 4
)
