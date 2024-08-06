library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(stars)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--model-sif-assim')
parser$add_argument('--region-sf')
parser$add_argument('--term')
parser$add_argument('--output')
args <- parser$parse_args()

model <- fst::read_fst(args$model_sif_assim)
region_sf <- readRDS(args$region_sf)
earth_bbox_sf <- rnaturalearth::ne_download(
  category = 'physical',
  type = 'wgs84_bounding_box',
  returnclass = 'sf'
)

term_key <- list(
  slope = list(
    name = 'slope',
    scale_factor = 1e-6,
    legend_title = expression('Slope [(mW'~m^{-2}~µm^{-1}~sr^{-1}*') / (kgCO'[2]~m^{-2}~s^{-1}*')]'),
    palette = 'Viridis',
    breaks = seq(0, 4, by = 0.5),
    limits = c(0, 4),
    label_precision = 1,
    symmetric = FALSE,
    drop_second_labels = FALSE,
    show_excess = TRUE
  ),
  intercept = list(
    name = 'intercept',
    scale_factor = 1,
    legend_title = expression('Intercept [W'~m^{-2}~µm^{-1}~sr^{-1}*']'),
    palette = 'PuOr',
    breaks = seq(-0.6, 0.6, by = 0.2),
    limits = c(-0.6, 0.6),
    label_precision = 1,
    symmetric = TRUE,
    drop_second_labels = FALSE,
    show_excess = FALSE
  )
)
stopifnot(args$term %in% names(term_key))
term_key <- term_key[[args$term]]

model_term <- model %>%
  select(
    longitude = model_longitude,
    latitude = model_latitude,
    month,
    value = args$term
  ) %>%
  filter(abs(latitude) != 89.5) %>%
  mutate(
    month = factor(month.abb[month], levels = month.abb),
    value = value * term_key$scale_factor
  )

model_stars <- model_term %>%
  arrange(longitude, latitude, month) %>%
  st_as_stars(dims = c('longitude', 'latitude', 'month')) %>%
  st_set_crs('WGS84') %>%
  st_transform('ESRI:54012')

base_labels <- sprintf(paste0('%.', term_key$label_precision, 'f'), term_key$breaks)
labels <- if (term_key$show_excess) {
  ifelse(
    abs(term_key$breaks) == max(abs(term_key$breaks)),
    sprintf(
      paste0('%s%.', term_key$label_precision, 'f'),
      ifelse(term_key$breaks < 0, '< -', '> '),
      max(abs(term_key$breaks))
    ),
    base_labels
  )
} else {
  base_labels
}
if (term_key$drop_second_labels) {
  labels[seq(2, length(term_key$breaks), by = 2)] <- ''
}

output <- ggplot() +
  geom_sf(data = earth_bbox_sf, fill = 'grey85', colour = NA) +
  geom_stars(
    data = model_stars,
    aes(fill = value)
  ) +
  geom_sf(data = region_sf, fill = NA, colour = 'grey35', linewidth = 0.1) +
  geom_segment(
    data = data.frame(y = c(-23, 23, 50)),
    mapping = aes(x = -180, y = y, xend = 180, yend = y),
    colour = 'black',
    linetype = 'dashed',
    linewidth = 0.2
  ) +
  geom_text(
    data = data.frame(
      x = c(-165, -170, -170),
      y = c(-23, 23, 50),
      label = c('23°S', '23°N', '50°N')
    ),
    mapping = aes(x = x, y = y, label = label),
    size = 6/.pt,
    nudge_y = 8
  ) +
  coord_sf(
    crs = sf::st_crs('ESRI:54012'),
    default_crs = sf::st_crs('WGS84'),
    label_graticule = '',
    expand = FALSE
  ) +
  facet_wrap(~month, nrow = 4) +
  scale_fill_binned_custom(
    term_key$palette,
    breaks = term_key$breaks,
    limits = term_key$limits,
    labels = labels,
    symmetric = term_key$symmetric,
    guide = guide_coloursteps(
      title = term_key$legend_title,
      title.position = 'top',
      title.hjust = 0.5,
      label.theme = element_text(size = 7),
      frame.colour = NA,
      barwidth = 13,
      barheight = 0.55
    ),
    na.value = NA
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = sprintf('SIF-GPP regression %s', term_key$name),
  ) +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.position = 'bottom',
    legend.margin = margin(t = 0, l = 0, b = 0, r = 0, unit = 'cm'),
    legend.title = element_text(size = 9),
    strip.text = element_text(size = 9),
    plot.title = element_text(
      hjust = 0.5,
      size = 12,
      margin = margin(t = 0, r = 0, b = 0.1, l = 0, unit = 'cm')
    ),
    plot.margin = margin(t = 0, b = 0, l = 0.1, r = 0.05, unit = 'cm')
  )

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width,
  height = 14.62
)
