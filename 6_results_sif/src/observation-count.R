library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

geom_world_sf <- function(
  colour = 'black',
  linewidth = 0.05,
  inherit.aes = FALSE,
  ...
) {
  worldmap <- sf::st_wrap_dateline(sf::st_as_sf(
    rnaturalearth::ne_coastline(110, returnclass = 'sf')
  ))

  geom_sf(
    data = worldmap,
    colour = colour,
    linewidth = linewidth,
    inherit.aes = inherit.aes,
    ...
  )
}

match_grid <- function(df, model) {
  latitude_index <- findInterval(
    df$latitude,
    c(model$latitude[1] - model$cell_height[1] / 2, model$latitude + model$cell_height / 2),
    rightmost.closed = TRUE
  )
  longitude_index <- findInterval(
    df$longitude,
    c(model$longitude[1] - model$cell_width[1] / 2, model$longitude + model$cell_width / 2),
    rightmost.closed = TRUE
  )
  longitude_index[longitude_index > length(model$longitude)] <- 1L

  df %>%
    mutate(
      latitude = model$latitude[latitude_index],
      longitude = model$longitude[longitude_index]
    )
}

label_expressions <- function(values) {
  stopifnot(is.expression(values))
  as_labeller(function(x) {
    if (is.null(names(values))) {
      x <- seq_along(x)
      if (length(x) != length(values)) {
        warning(sprintf(
          'Number of labels (%s) does not match number of values (%s)',
          length(values),
          length(x)
        ))
      }
    }
    as.list(values[x])
  }, default = identity)
}


parser <- ArgumentParser()
parser$add_argument('--observations')
parser$add_argument('--output')
args <- parser$parse_args()

observations <- fst::read_fst(args$observations)

observations <- bind_rows(
  observations %>%
    filter(observation_type != 'oco2'),
  observations %>%
    filter(observation_type == 'oco2') %>%
    mutate(
      observation_type = if_else(
        overall_observation_mode %in% c('LN_SIF', 'LG_SIF'),
        'oco2_sif',
        'oco2_xco2'
      )
    )
)


grid_system <- list(
  latitude = seq(-87.5, 87.5, by = 5),
  cell_height = 5,
  longitude = seq(-177.5, 177.5, by = 5),
  cell_width = 5
)
grid_base <- expand.grid(
  latitude = grid_system$latitude,
  longitude = grid_system$longitude
)

count_df <- lapply(c('obspack', 'oco2_xco2', 'oco2_sif'), function(observation_type_i) {
  grid_base %>%
    left_join(
      observations %>%
        filter(
          observation_type == observation_type_i,
          assimilate == 1,
          overall_observation_mode != 'OG'
        ) %>%
        match_grid(grid_system) %>%
        group_by(longitude, latitude) %>%
        summarise(n = n(), .groups = 'drop'),
      by = c('longitude', 'latitude')
    ) %>%
    grid_df_to_sf('n') %>%
    mutate(observation_type = observation_type_i)
}) %>%
  bind_rows() %>%
  mutate(
    observation_type = factor(observation_type, c('obspack', 'oco2_xco2', 'oco2_sif'))
  )

strip_labels <- expression(
  obspack = 'In situ/flask mole-fraction',
  oco2_xco2 = 'OCO-2 XCO'[2],
  oco2_sif = 'OCO-2 SIF'
)

output1 <- ggplot() +
  geom_sf(
    data = count_df,
    mapping = aes(fill = pmax(10, n)),
    colour = NA
  ) +
  geom_path(
    data = data.frame(
      longitude = c(-180, 180, 180, -180, -180),
      latitude = c(-90, -90, 90, 90, -90)
    ),
    mapping = aes(longitude, latitude),
    colour = 'black',
    linewidth = 0.2
  ) +
  geom_world_sf() +
  scale_fill_binned(
    type = 'viridis',
    trans = 'log10',
    breaks = 10 ^ seq(0, 6, by = 0.5),
    labels = function(x) {
      ifelse((x %% 1 == 0) & (x > 10), sprintf('%.0f', x), '')
    },
    na.value = NA
  ) +
  coord_sf(
    crs = sf::st_crs('+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs'),
    default_crs = sf::st_crs('WGS84'),
    label_graticule = '',
    expand = FALSE
  ) +
  labs(x = NULL, y = NULL, title = NULL, fill = 'Obs. count') +
  facet_wrap(
    ~ observation_type,
    labeller = labeller(observation_type = label_expressions(strip_labels))
  ) +
  theme(
    plot.margin = margin(t = 0, r = 1, b = 0, l = 1, unit = 'mm'),
    panel.border = element_blank(),
    strip.text = element_text(size = 8),
    legend.position = 'bottom',
    legend.title = element_text(
      size = 8,
      colour = '#23373b',
      vjust = 1,
      margin = margin(r = 1, unit = 'mm')
    ),
    legend.text = element_text(size = 7, colour = '#23373b'),
    legend.key.height = unit(2, 'mm'),
    legend.key.width = unit(1, 'cm'),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm'),
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm'),
    legend.box.spacing = unit(2, 'mm')
  )

output2 <- observations %>%
  filter(
    assimilate == 1,
    overall_observation_mode != 'OG'
  ) %>%
  mutate(
    month = lubridate::round_date(time, 'month')
  ) %>%
  group_by(observation_type, month) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(
    observation_type = factor(c(
      'obspack' = 'In situ/flask\nmole-fraction',
      'oco2_xco2' = 'OCO-2 XCO2',
      'oco2_sif' = 'OCO-2 SIF'
    )[as.character(observation_type)], c(
      'In situ/flask\nmole-fraction',
      'OCO-2 XCO2',
      'OCO-2 SIF'
    ))
  ) %>%
  ggplot(aes(month, n, linetype = observation_type)) +
    geom_line() +
    scale_y_log10() +
    scale_linetype_discrete(labels = c(
      'In situ/flask mole-fraction',
      expression('OCO-2 XCO'[2]),
      'OCO-2 SIF'
    )) +
    labs(x = NULL, y = 'Obs. count', linetype = NULL) +
    theme(
      plot.margin = margin(t = 3, r = 0, b = 0, l = 0, unit = 'mm'),
      axis.title.y = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 7),
      legend.text = element_text(size = 8),
      legend.position = 'bottom',
      legend.key.size = unit(5, 'mm'),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm'),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm'),
      legend.box.spacing = unit(2, 'mm')
    )

design <- ('
  11111111
  11111111
  11111111
  #222222#
  #222222#
')
output <- wrap_plots(output1, output2, design = design)

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width,
  height = 6.94
)
