library(dplyr, warn.conflicts = FALSE)
library(stars)

source('partials/utils.R')
source('partials/display.R')

args <- list(
  six_year_average = '6_results_sif/intermediates/six-year-average.fst',
  output_base = '6_results_sif/figures/slides/average-map-gpp'
)


plot_map_poster <- function(
  df,
  variable,
  breaks,
  limits,
  palette,
  show_excess = TRUE,
  label_precision = 0,
  drop_second_labels = FALSE,
  symmetric = FALSE,
  reverse = FALSE,
  trim_colours = FALSE,
  bar_width = 45
) {
  base_labels <- sprintf(paste0('%.', label_precision, 'f'), breaks)
  labels <- if (show_excess) {
    ifelse(
      abs(breaks) == max(abs(breaks)),
      sprintf(
        paste0('%s%.', label_precision, 'f'),
        ifelse(breaks < 0, '< -', '> '),
        max(abs(breaks))
      ),
      base_labels
    )
  } else {
    base_labels
  }
  if (drop_second_labels) {
    labels[seq(2, length(breaks), by = 2)] <- ''
  }
  worldmap <- sf::st_wrap_dateline(sf::st_as_sf(
    rnaturalearth::ne_coastline(110, returnclass = 'sf')
  ))
  ggplot() +
    geom_sf(data = earth_bbox_sf, fill = '#dddddd', colour = '#555555', linewidth = 0.75) +
    geom_stars(
      data = df,
      aes(fill = {{ variable }})
    ) +
    geom_sf(data = worldmap, fill = NA, colour = '#555555', linewidth = 0.45) +
    coord_sf(
      crs = sf::st_crs('ESRI:54012'),
      default_crs = sf::st_crs('WGS84')
    ) +
    scale_fill_binned_custom(
      palette,
      breaks = breaks,
      limits = limits,
      labels = labels,
      symmetric = symmetric,
      reverse = reverse,
      trim_colours = trim_colours,
      guide = guide_coloursteps(
        title.position = 'top',
        title.hjust = 0.5,
        label.theme = element_text(
          size = 28,
          margin = margin(0.75, 0, 0, 0, unit = 'cm')
        ),
        frame.colour = '#999999',
        frame.linewidth = 0.15,
        barwidth = bar_width,
        barheight = 3,
        even.steps = TRUE
      ),
      na.value = NA
    ) +
    labs(x = NULL, y = NULL)
}

earth_bbox_sf <- rnaturalearth::ne_download(
  category = 'physical',
  type = 'wgs84_bounding_box',
  returnclass = 'sf'
)

six_year_average_stars <- fst::read_fst(args$six_year_average) %>%
  filter(
    inventory == 'bio_assim',
    abs(latitude) != 89.5,
		abs(longitude) != 180
  ) %>%
  select(-inventory) %>%
  arrange(longitude, latitude, estimate) %>%
  st_as_stars(dims = c('longitude', 'latitude', 'estimate'))

average_posterior_mean_wombat <- six_year_average_stars %>%
  filter(estimate == 'LNLGISSIF') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map_poster(
    value,
    round(seq(-12, 0, by = 2), 1),
    c(-14, 0),
    'bamako',
    reverse = FALSE,
    symmetric = FALSE,
    label_precision = 0,
    drop_second_labels = FALSE,
    show_excess = TRUE
  ) +
    labs(fill = bquote('GPP flux [kgCO'[2]~m^{-2}~yr^{-1}*']'), title = 'Posterior Mean GPP') +
    theme(
      panel.border = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        size = 32,
        margin = margin(t = 0, r = 0, b = -1.25, l = 0, unit = 'cm')
      ),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm'),
      legend.position = 'bottom',
      legend.margin = margin(t = -0.4, r = 0, b = 0, l = 0, unit = 'cm'),
      legend.title = element_text(
        size = 28,
        margin = margin(0, 0, 0.05, 0, unit = 'cm')
      )
    )

average_posterior_sd_wombat <- six_year_average_stars %>%
  filter(estimate == 'LNLGISSIF') %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012') %>%
  plot_map_poster(
    value_scale,
    round(seq(0, 0.4, by = 0.1), 1),
    c(0, 0.4 + 1.5 * 0.1),
    'lapaz',
    bar_width = 32,
    symmetric = FALSE,
    reverse = TRUE,
    trim_colours = TRUE,
    drop_second_labels = FALSE,
    label_precision = 1,
    show_excess = TRUE
  ) +
    labs(fill = bquote('Flux [kgCO'[2]~m^{-2}~yr^{-1}*']'), title = 'Posterior Standard Deviation') +
    theme(
      panel.border = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        size = 32,
        margin = margin(t = 0, r = 0, b = -0.35, l = 0, unit = 'cm')
      ),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm'),
      legend.position = 'bottom',
      legend.margin = margin(t = -0.4, r = 0, b = 0, l = 0, unit = 'cm'),
      legend.title = element_text(
        size = 28,
        margin = margin(-0.5, 0, 0.05, 0, unit = 'cm')
      )
    )

ggsave_base(
  paste0(args$output_base, '-mean.pdf'),
  average_posterior_mean_wombat,
  width = 62,
  height = 37
)

ggsave_base(
  paste0(args$output_base, '-sd.pdf'),
  average_posterior_sd_wombat,
  width = 26.5,
  height = 19.5
)
