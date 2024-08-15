library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(patchwork)
library(stars)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--six-year-average')
parser$add_argument('--flux-component')
parser$add_argument('--region-sf')
parser$add_argument('--output')
args <- parser$parse_args()

flux_key <- list(
  'gpp' = list(
    name = 'bio_assim',
    label = 'GPP',
    label_precision = 0,
    drop_second_labels = FALSE,
    palette = 'bamako',
    reverse = FALSE,
    symmetric = FALSE,
    mean_breaks = round(seq(-12, 0, by = 2), 1),
    mean_limits = c(-14, 0),
    diff_breaks = round(seq(-8, 8, by = 2), 1),
    diff_limits = c(-10, 10),
    sd_breaks = round(seq(0, 0.25, by = 0.05), 3),
    sd_limits = c(0, 0.25 + 1.5 * 0.05),
    mad_breaks = round(seq(0, 0.6, by = 0.15), 3),
    mad_limits = c(0, 0.75)
  ),
  'resp' = list(
    name = 'bio_resp_tot',
    label = 'respiration',
    label_precision = 0,
    drop_second_labels = FALSE,
    palette = 'turku',
    reverse = TRUE,
    symmetric = FALSE,
    mean_breaks = round(seq(0, 12, by = 2), 1),
    mean_limits = c(0, 14),
    diff_breaks = round(seq(-8, 8, by = 2), 1),
    diff_limits = c(-10, 10),
    sd_breaks = round(seq(0, 0.25, by = 0.05), 3),
    sd_limits = c(0, 0.25 + 1.5 * 0.05),
    mad_breaks = round(seq(0, 0.6, by = 0.15), 3),
    mad_limits = c(0, 0.75)
  ),
  'nee' = list(
    name = 'nee',
    label = 'NEE',
    label_precision = 1,
    drop_second_labels = TRUE,
    palette = 'bam',
    reverse = FALSE,
    symmetric = TRUE,
    mean_breaks = round(seq(-2.5, 2.5, by = 0.5), 1),
    mean_limits = c(-2.5, 2.5),
    diff_breaks = round(seq(-2.5, 2.5, by = 0.5), 1),
    diff_limits = c(-3, 3),
    sd_breaks = round(seq(0, 0.1, by = 0.025), 3),
    sd_limits = c(0, 0.1 + 1.5 * 0.025),
    mad_breaks = round(seq(0, 0.45, by = 0.15), 3),
    mad_limits = c(0, 0.6)
  )
)
if (!(args$flux_component %in% names(flux_key))) {
  stop('Invalid flux component')
}
flux_key <- flux_key[[args$flux_component]]

region_sf <- readRDS(args$region_sf)
earth_bbox_sf <- rnaturalearth::ne_download(
  category = 'physical',
  type = 'wgs84_bounding_box',
  returnclass = 'sf'
)

six_year_average_stars <- fst::read_fst(args$six_year_average) %>%
  filter(
    estimate %in% c('LNLGISSIF', 'FLUXCOM', 'FLUXCOM Difference'),
    inventory == flux_key$name,
    abs(latitude) != 89.5
  ) %>%
  select(-inventory) %>%
  mutate(
    estimate = case_match(
      estimate,
      'FLUXCOM Difference' ~ 'Difference',
      .default = estimate
    )
  ) %>%
  arrange(longitude, latitude, estimate) %>%
  st_as_stars(dims = c('longitude', 'latitude', 'estimate'))

flux_mean_label <- bquote(.(flux_key$label)~'flux [kgCO'[2]~m^{-2}~yr^{-1}*']')
flux_sd_label <- expression('Post. std. dev. [kgCO'[2]~m^{-2}~yr^{-1}*']')
flux_mad_label <- expression('Median abs. dev. [kgCO'[2]~m^{-2}~yr^{-1}*']')

average_mean_fluxcom <- six_year_average_stars %>%
  filter(estimate == 'FLUXCOM') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map(
    value_mean,
    flux_key$mean_breaks,
    flux_key$mean_limits,
    flux_key$palette,
    reverse = flux_key$reverse,
    symmetric = flux_key$symmetric,
    label_precision = flux_key$label_precision,
    drop_second_labels = flux_key$drop_second_labels,
    show_excess = TRUE
  ) +
    labs(fill = flux_mean_label, title = 'FLUXCOM mean')

average_mad_fluxcom <- six_year_average_stars %>%
  filter(estimate == 'FLUXCOM') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map(
    value_scale,
    flux_key$mad_breaks,
    flux_key$mad_limits,
    'lipari',
    show_excess = TRUE,
    drop_second_labels = FALSE,
    label_precision = 2,
    symmetric = FALSE,
    reverse = TRUE
  ) +
    labs(fill = flux_mad_label, title = 'FLUXCOM median abs. dev.')

average_posterior_mean_wombat <- six_year_average_stars %>%
  filter(estimate == 'LNLGISSIF') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map(
    value_mean,
    flux_key$mean_breaks,
    flux_key$mean_limits,
    flux_key$palette,
    reverse = flux_key$reverse,
    symmetric = flux_key$symmetric,
    label_precision = flux_key$label_precision,
    drop_second_labels = flux_key$drop_second_labels,
    show_excess = TRUE
  ) +
    labs(fill = flux_mean_label, title = 'WOMBAT v2.S post. mean')

average_posterior_sd_wombat <- six_year_average_stars %>%
  filter(estimate == 'LNLGISSIF') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map(
    value_scale,
    flux_key$sd_breaks,
    flux_key$sd_limits,
    'lapaz',
    show_excess = TRUE,
    drop_second_labels = FALSE,
    label_precision = 2,
    symmetric = FALSE,
    reverse = TRUE
  ) +
    labs(fill = flux_sd_label, title = 'WOMBAT v2.S post. std. dev.')

average_posterior_mean_diff <- six_year_average_stars %>%
  filter(estimate == 'Difference') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map(
    value_mean,
    flux_key$diff_breaks,
    flux_key$diff_limits,
    'vik',
    symmetric = TRUE,
    label_precision = flux_key$label_precision,
    drop_second_labels = TRUE,
    show_excess = TRUE
  ) +
    labs(
      fill = bquote(.(flux_key$label)~'difference [kgCO'[2]~m^{-2}~yr^{-1}*']'),
      title = 'WOMBAT v2.S - FLUXCOM mean'
    )

average_posterior_sd_diff <- six_year_average_stars %>%
  filter(estimate == 'Difference') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map(
    value_scale,
    flux_key$sd_breaks,
    flux_key$sd_limits,
    'lapaz',
    show_excess = TRUE,
    drop_second_labels = FALSE,
    label_precision = 2,
    symmetric = FALSE,
    reverse = TRUE
  ) +
    labs(fill = flux_sd_label) +
    ggtitle('Post. std. dev. (v2.S - FLUXCOM)')

base_theme <- theme(
  legend.position = 'bottom',
  legend.margin = margin(t = -0.4, l = 0.05, b = -0.2, r = 0.05, unit = 'cm'),
  legend.title = element_text(
    size = 8,
    margin = margin(0, 0, 0.05, 0, unit = 'cm')
  ),
  plot.title = element_text(
    hjust = 0.5,
    size = 9,
    margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')
  ),
  plot.margin = margin(t = 0.4, b = 0.2, l = 0, r = 0, unit = 'cm')
)

top_theme <- base_theme +
  theme(
    plot.margin = margin(t = 0, b = 0.2, l = 0, r = 0, unit = 'cm')
  )

output <- wrap_plots(
  average_mean_fluxcom + top_theme,
  average_mad_fluxcom + top_theme,
  average_posterior_mean_wombat + base_theme,
  average_posterior_sd_wombat + base_theme,
  average_posterior_mean_diff + base_theme,
  average_posterior_sd_diff + base_theme,
  nrow = 3,
  ncol = 2
)

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width,
  height = 15.68
)
