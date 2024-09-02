library(dplyr, warn.conflicts = FALSE)
library(patchwork)
library(stars)

source('partials/utils.R')
source('partials/display.R')

args <- list(
  sib4_monthly = '3_sif/intermediates/sib4-monthly-sif-assim-2x25.nc',
  control_emissions = '4_inversion/intermediates/control-emissions.fst',
  output_maps = '6_results_sif/figures/slides/sib4-sif-gpp-maps.pdf',
  output_ts = '6_results_sif/figures/slides/sib4-sif-gpp-ts.pdf'
)

earth_bbox_sf <- rnaturalearth::ne_download(
  category = 'physical',
  type = 'wgs84_bounding_box',
  returnclass = 'sf'
)

cell_area <- fst::read_fst(args$control_emissions) %>%
  distinct(longitude, latitude, area)

fn <- ncdf4::nc_open(args$sib4_monthly)
on.exit(ncdf4::nc_close(fn))
v <- function(...) as.vector(ncdf4::ncvar_get(fn, ...))

get_var <- function(var_name) {
  expand.grid(
    longitude = v('lon'),
    latitude = v('lat'),
    time = ncvar_get_time(fn, 'time'),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      term = var_name,
      value = v(var_name)
    ) %>%
    filter(value != 0)
}

sib4_monthly <- bind_rows(lapply(c('sif', 'assim'), get_var)) %>%
  left_join(cell_area, by = c('longitude', 'latitude'))

spatial_averages <- sib4_monthly %>%
  group_by(longitude, latitude, term) %>%
  summarise(
    value = mean(area * value),
    .groups = 'drop'
  )

spatial_stats <- spatial_averages %>%
  group_by(term) %>%
  summarise(
    term_mean = mean(value),
    term_sd = sd(value),
    .groups = 'drop'
  )

spatial_averages_stars <- spatial_averages %>%
  left_join(spatial_stats, by = 'term') %>%
  mutate(value = (value - term_mean) / term_sd) %>%
  select(longitude, latitude, term, value) %>%
  arrange(longitude, latitude, term) %>%
  st_as_stars(dims = c('longitude', 'latitude', 'term'))

sif_map <- spatial_averages_stars %>%
  filter(term == 'sif') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map_slides(
    value,
    seq(-0.5, 3.5, 0.5),
    c(-1, 4),
    'bamako',
    reverse = TRUE,
    symmetric = FALSE,
    label_precision = 1,
    drop_second_labels = TRUE,
    show_excess = FALSE
  ) +
    labs(fill = NULL)

assim_map <- spatial_averages_stars %>%
  filter(term == 'assim') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map_slides(
    value,
    seq(-0.5, 3.5, 0.5),
    c(-1, 4),
    'bamako',
    reverse = TRUE,
    symmetric = FALSE,
    label_precision = 1,
    drop_second_labels = TRUE,
    show_excess = FALSE
  ) +
    labs(fill = NULL)

output_maps <- wrap_plots(
  sif_map,
  assim_map,
  ncol = 2,
  guides = 'collect'
) &
  theme(
    panel.border = element_blank(),
    plot.title = element_blank(),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm'),
    legend.position = 'bottom',
    legend.margin = margin(t = -0.4, r = 0, b = -0.2, l = 0, unit = 'cm'),
    legend.title = element_blank()
  )

ggsave_base(args$output_maps, output_maps, width = 16, height = 5)


sib4_monthly_ts_values <- sib4_monthly %>%
  filter(between(time, as.Date('2015-01-01'), as.Date('2021-01-01'))) %>%
  group_by(time, term) %>%
  summarise(
    value = mean(area * value),
    .groups = 'drop'
  )

sib4_monthly_ts_stats <- sib4_monthly_ts_values %>%
  group_by(term) %>%
  summarise(
    term_mean = mean(value),
    term_sd = sd(value),
    .groups = 'drop'
  )

sib4_monthly_ts <- sib4_monthly_ts_values %>%
  left_join(sib4_monthly_ts_stats, by = 'term') %>%
  mutate(
    value = (value - term_mean) / term_sd,
    time = as.Date(time),
    term = factor(c(
      'sif' = 'SIF',
      'assim' = 'GPP'
    )[term], levels = c('GPP', 'SIF'))
  )

output_ts <- ggplot(sib4_monthly_ts) +
  geom_line(
    aes(
      x = time,
      y = value,
      linetype = term,
      color = term
    ),
    linewidth = 1
  ) +
  scale_x_date(date_labels = '%Y-%m') +
  scale_color_manual(values = c('GPP' = 'grey70', 'SIF' = 'black')) +
  scale_linetype_manual(values = c('GPP' = 'solid', 'SIF' = '22')) +
  labs(
    x = NULL,
    y = NULL,
    linetype = NULL,
    color = NULL
  ) +
  theme(
    plot.margin = margin(t = 0.1, r = 0.1, b = 0, l = 0.1, unit = 'cm'),
    legend.margin = margin(t = -0.2, r = 0, b = 0, l = 0, unit = 'cm'),
    legend.position = 'bottom'
  )

ggsave_base(args$output_ts, output_ts, width = 14, height = 5)
