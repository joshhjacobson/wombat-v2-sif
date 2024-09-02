library(dplyr, warn.conflicts = FALSE)
library(patchwork)
library(stars)

source('partials/utils.R')
source('partials/display.R')

args <- list(
  six_year_average = '6_results_sif/intermediates/six-year-average.fst',
  output = '6_results_sif/figures/slides/average-maps-wombat.pdf'
)

earth_bbox_sf <- rnaturalearth::ne_download(
  category = 'physical',
  type = 'wgs84_bounding_box',
  returnclass = 'sf'
)

six_year_average_gpp <- fst::read_fst(args$six_year_average) %>%
  filter(
    inventory == 'bio_assim'
  ) %>%
  select(-inventory) %>%
  arrange(longitude, latitude, estimate) %>%
  st_as_stars(dims = c('longitude', 'latitude', 'estimate'))

six_year_average_nee <- fst::read_fst(args$six_year_average) %>%
  filter(
    inventory == 'nee'
  ) %>%
  select(-inventory) %>%
  arrange(longitude, latitude, estimate) %>%
  st_as_stars(dims = c('longitude', 'latitude', 'estimate'))


average_diff_gpp <- six_year_average_gpp %>%
  filter(estimate == 'WOMBAT Difference') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map_slides(
    value,
    round(seq(-6, 6, by = 2), 1),
    c(-8, 8),
    'vik',
    symmetric = TRUE,
    label_precision = 0,
    drop_second_labels = FALSE,
    show_excess = FALSE
  ) +
    labs(
      fill = bquote('Flux difference [kgCO'[2]~m^{-2}~yr^{-1}*']'),
      title = NULL
    )

average_posterior_sd_gpp <- six_year_average_gpp %>%
  filter(estimate == 'LNLGISSIF') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map_slides(
    value_scale,
    round(seq(0, 0.4, by = 0.1), 1),
    c(0, 0.4 + 1.5 * 0.1),
    'devon',
    symmetric = FALSE,
    reverse = TRUE,
    trim_colours = TRUE,
    drop_second_labels = FALSE,
    label_precision = 1,
    show_excess = TRUE
  ) +
    labs(
      fill = bquote('Flux [kgCO'[2]~m^{-2}~yr^{-1}*']'),
      title = NULL
    )

average_diff_nee <- six_year_average_nee %>%
  filter(estimate == 'WOMBAT Difference') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map_slides(
    value,
    round(seq(-2.5, 2.5, by = 0.5), 1),
    c(-3, 3),
    'vik',
    symmetric = TRUE,
    label_precision = 1,
    drop_second_labels = TRUE,
    show_excess = FALSE
  ) +
    labs(
      fill = bquote('Flux difference [kgCO'[2]~m^{-2}~yr^{-1}*']'),
      title = NULL
    )

average_posterior_sd_nee <- six_year_average_nee %>%
  filter(estimate == 'LNLGISSIF') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map_slides(
    value_scale,
    round(seq(0, 0.1, by = 0.025), 3),
    c(0, 0.125),
    'devon',
    symmetric = FALSE,
    reverse = TRUE,
    trim_colours = TRUE,
    drop_second_labels = FALSE,
    label_precision = 3,
    show_excess = TRUE
  ) +
    labs(
      fill = bquote('Flux [kgCO'[2]~m^{-2}~yr^{-1}*']'),
      title = NULL
    )

base_theme <- theme(
  plot.title = element_blank(),
  plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')
)

top_theme <- base_theme +
  theme(
    plot.margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit = 'cm')
  )


output <- wrap_plots(
  wrap_plots(
    average_diff_gpp + top_theme,
    average_diff_nee + top_theme,
    ncol = 2
  ) &
    theme(
      legend.position = 'bottom',
      legend.margin = margin(t = -0.4, r = 0, b = -0.2, l = 0, unit = 'cm')
    ),
  wrap_plots(
    average_posterior_sd_gpp + base_theme,
    average_posterior_sd_nee + base_theme,
    ncol = 2
  ) &
    theme(
      legend.position = 'bottom',
      legend.margin = margin(t = -0.4, r = 0, b = 0, l = 0, unit = 'cm')
    ),
  nrow = 2
) &
  theme(
    panel.border = element_blank(),
    legend.title = element_text(
      size = 8,
      margin = margin(0, 0, 0.05, 0, unit = 'cm')
    )
  )

ggsave_base(
  args$output,
  output,
  width = 16.5,
  height = 11.6
)
