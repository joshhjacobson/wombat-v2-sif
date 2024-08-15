library(ggplot2)

theme_set(theme_bw())
theme_replace(
  legend.background = element_blank(),
  legend.key = element_blank(),
  panel.background = element_blank(),
  strip.background = element_blank(),
  plot.background = element_blank(),
  # panel.border = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.text = element_text(colour = '#23373b'),
  axis.title = element_text(colour = '#23373b'),
  plot.margin = margin(t = 1, r = 0, b = 0, l = 1, unit = 'mm')
)

DISPLAY_SETTINGS <- list(
  full_width = 14.32,
  supplement_full_width = 16.5,
  full_height = 20,
  png_plot_dpi = 320
)

get_legend <- function(plot_object){
  tmp <- ggplot_gtable(ggplot_build(plot_object))
  legend_index <- which(sapply(tmp$grobs, function(x) x$name) == 'guide-box')
  tmp$grobs[[legend_index]]
}

ggsave_base <- function(filename, plot, bg = 'transparent', dpi = DISPLAY_SETTINGS$png_plot_dpi, ...) {
  ggsave(
    filename,
    plot,
    units = 'cm',
    dpi = dpi,
    bg = bg,
    ...
  )
}

ggsave_fullwidth <- function(...) {
  ggsave_base(..., width = DISPLAY_SETTINGS$full_width)
}

scale_fill_fermenter_n <- function(
  ...,
  type = 'seq',
  palette = 1,
  direction = -1,
  n_colours = 10,
  na.value = 'grey50',
  guide = 'coloursteps',
  aesthetics = 'fill'
) {
  palette <- scales::gradient_n_pal(
    scales::brewer_pal(type, palette, direction)(n_colours),
    NULL,
    'Lab'
  )
  binned_scale(
    aesthetics,
    'steps2_uneven',
    function(x) {
      palette(seq(0, 1, length.out = length(x)))
    },
    na.value = na.value,
    guide = guide,
    ...
  )
}

scale_fill_binned_custom <- function(
  palette_name,
  symmetric = FALSE,
  reverse = FALSE,
  na.value = 'grey50',
  guide = 'coloursteps',
  aesthetics = 'fill',
  ...
) {
  palette_colours <- if (symmetric && palette_name %in% c('RdBu', 'PuOr')) {
    scales::brewer_pal('div', palette_name, -1)(10)[2:9]
  # } else if (symmetric) {
  #   if (palette_name %in% row.names(colorspace::divergingx_palettes())) {
  #     colorspace::divergingx_hcl(10, palette = palette_name, rev = reverse)
  #   } else {
  #     colorspace::diverging_hcl(10, palette = palette_name, rev = reverse)
  #   }
  } else {
    # if (reverse) {
    #   colorspace::sequential_hcl(11, palette = palette_name, rev = reverse)[1:9]
    # } else {
    #   colorspace::sequential_hcl(11, palette = palette_name, rev = reverse)[3:11]
    # }
    scico::scico(11, palette = palette_name, direction = ifelse(reverse, -1, 1))
  }
  palette <- scales::gradient_n_pal(palette_colours, NULL, 'Lab')
  binned_scale(
    aesthetics = 'fill',
    palette = function(x) {
      palette(seq(0, 1, length.out = length(x)))
    },
    na.value = na.value,
    guide = guide,
    ...
  )
}

plot_map <- function(
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
  bar_width = 11
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
  ggplot() +
    geom_sf(data = earth_bbox_sf, fill = 'grey85', colour = 'black') +
    geom_stars(
      data = df,
      aes(fill = {{ variable }})
    ) +
    geom_sf(data = region_sf, fill = NA, colour = 'grey35', linewidth = 0.1) +
    geom_segment(
      data = data.frame(y = c(-23, 23, 50)),
      mapping = aes(x = -180, y = y, xend = 180, yend = y),
      colour = 'black',
      linetype = 'dashed',
      linewidth = 0.3
    ) +
    geom_text(
      data = data.frame(
        x = c(-172, -180, -180),
        y = c(-23, 23, 50),
        label = c('23°S', '23°N', '50°N')
      ),
      mapping = aes(x = x, y = y, label = label),
      size = 7/.pt,
      nudge_y = 8
    ) +
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
      guide = guide_coloursteps(
        title.position = 'top',
        title.hjust = 0.5,
        label.theme = element_text(size = 7, margin = margin(0.1, 0, 0, 0, unit = 'cm')),
        frame.colour = NA,
        barwidth = bar_width,
        barheight = 0.5,
        even.steps = TRUE
      ),
      na.value = NA
    ) +
    theme(
      panel.border = element_blank()
    ) +
    labs(x = NULL, y = NULL)
}
