library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(fst)
library(patchwork)

# source(Sys.getenv('UTILS_PARTIAL'))
# source(Sys.getenv('DISPLAY_PARTIAL'))
source("partials/display.R")
source("partials/utils.R")

args <- list()
args$control_sif <- "3_sif/intermediates/oco2-hourly-sif.fst"
args$perturbations_augmented <- "5_results/intermediates/perturbations-augmented.fst"
args$samples <- "4_inversion/intermediates/samples-LNLGISSIF.rds"
args$observations <- "4_inversion/intermediates/observations.fst"
args$region_sf <- '5_results/intermediates/region-sf.rds'

observations <- read_fst(args$observations)
control_sif <- read_fst(args$control_sif)
perturbations_base <- read_fst(args$perturbations_augmented)
samples <- readRDS(args$samples)
region_sf <- readRDS(args$region_sf)


output <- observations %>%
  filter(
    observation_group == "3_SIF",
    assimilate == 1
  ) %>%
  select(c(observation_id, longitude, latitude, time, value)) %>%
  left_join(
    control_sif %>%
      select(c(
        observation_id,
        model_longitude,
        model_latitude,
        model_time,
        value_control = value,
        slope
      )),
    by = "observation_id"
  ) %>%
  mutate(
    offset = value - value_control
  )

perturbations_sif <- perturbations_base %>%
  filter(
    inventory == "bio_assim"
  ) %>%
  left_join(
    samples$alpha_df %>%
      select(basis_vector, alpha = value),
    by = "basis_vector"
  ) %>%
  mutate(
    # NOTE: sign of perturbations are switched to match SIF
    value = -value * alpha
  ) %>%
  group_by(longitude, latitude, time) %>%
  summarise(value = sum(value), .groups = "drop")

output <- output %>%
  mutate(
    month = strftime(model_time, "%Y-%m")
  ) %>%
  left_join(
    perturbations_sif %>%
      mutate(month = strftime(time, "%Y-%m")) %>%
      select(c(
        model_longitude = longitude,
        model_latitude = latitude,
        month,
        perturbation = value
      )),
    by = c("model_longitude", "model_latitude", "month")
  ) %>%
  mutate(
    fitted = value_control + slope * perturbation,
    residual = offset - slope * perturbation
  )

p_scatter <- ggplot(output) +
  geom_point(aes(x = fitted, y = residual), shape = 1, alpha = 0.1) +
  labs(
    x = "Fitted SIF",
    y = "Residual SIF",
  )

ggsave_base(
  '6_results_sif/figures/sif-fitted-vs-residual.png',
  p_scatter,
  bg = 'white',
  width = 12,
  height = 12
)

p_hist <- ggplot(output) +
  geom_histogram(aes(x = residual), binwidth = 0.1) +
  labs(
    x = "Residual SIF",
    y = "Count"
  )

ggsave_base(
  '6_results_sif/figures/sif-fitted-vs-residual-with-hist.png',
  (p_scatter + p_hist) + plot_annotation(
    title = 'SIF residuals (2014-09 to 2020-12)'
  ),
  bg = 'white',
  width = 24,
  height = 12
)

output_global_monthly <- output %>%
  filter(
    time >= '2015-01-01',
    time < '2021-01-01'
  ) %>%
  group_by(month) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    value_control = mean(value_control, na.rm = TRUE),
    fitted = mean(fitted, na.rm = TRUE),
    residual = mean(residual, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  tidyr::pivot_longer(
    c(value, value_control, fitted, residual),
    names_to = 'stage',
    values_to = 'value'
  ) %>%
  mutate(
    month = as.Date(paste0(month, '-01')),
    stage = factor(c(
      'value' = 'OCO-2',
      'value_control' = 'Bottom-up',
      'fitted' = 'Fitted',
      'residual' = 'Residual'
    )[stage], levels = c(
      'OCO-2',
      'Bottom-up',
      'Fitted',
      'Residual'
    )),
    plot_group = if_else(
      stage == 'Residual',
      'Residual (observed - fitted)',
      'OCO-2, Bottom-up, and Fitted'
    )
    # plot_group = case_when(
    #   stage %in% c('OCO-2', 'Bottom-up') ~ 'OCO-2 and Bottom-up',
    #   stage == 'Offset' ~ 'Offset',
    #   stage == 'Perturbation' ~ 'Perturbation',
    #   stage == 'Residual' ~ 'Residual (observed - fitted)'
    # )
  )

p <- ggplot(output_global_monthly) +
  geom_line(aes(x = month, y = value, colour = stage, linetype = stage)) +
  facet_wrap(~plot_group, ncol = 1, scales = 'free_y') +
  scale_colour_manual(
    values = c(
      'OCO-2' = 'blue',
      'Bottom-up' = 'black',
      'Fitted' = '#ff4444',
      'Residual' = 'purple'
    )
  ) +
  scale_linetype_manual(
    values = c(
      'OCO-2' = 'dotted',
      'Bottom-up' = 'dashed',
      'Fitted' = 'solid',
      'Residual' = 'dotdash'
    )
  ) +
  scale_x_date(date_breaks = '6 months', date_labels = '%Y-%m') +
  labs(
    x = "Month",
    y = expression('SIF [W' * m^-2 * µm^-1 * sr^-1 * ']'),
    colour = NULL,
    linetype = NULL,
    title = "Monthly SIF: mean across observation locations"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, colour = '#23373b')
  )

ggsave_base(
  '6_results_sif/figures/sif-components-global.pdf',
  p,
  width = 18,
  height = 12
)



p <- ggplot(output) +
  geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
  geom_point(
    aes(x = longitude, y = latitude, colour = residual),
    size = 0.5,
    shape = 1,
    alpha = 0.3
  ) +
  colorspace::scale_colour_binned_divergingx(
    palette = 'RdYlBu',
    rev = TRUE,
    guide = guide_colorbar(
      title.position = 'top',
      title.hjust = 0.5,
      axis = FALSE,
      label.theme = element_text(size = 8),
      frame.colour = '#999999',
      barwidth = 20
    ),
    n.breaks = 7,
  ) +
  coord_sf(
    default_crs = sf::st_crs('WGS84'),
    crs = sf::st_crs('+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')
  ) +
  labs(
    x = NULL,
    y = NULL,
    colour = expression('SIF [W' * m^-2 * µm^-1 * sr^-1 * ']'),
    title = 'SIF residuals (2014-09 to 2020-12)'
  ) +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(size = 13, hjust = 0.5),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

ggsave_base(
  '6_results_sif/figures/sif-residuals-map.png',
  p,
  bg = 'white',
  width = 20,
  height = 14
)


output_seasons <- output %>%
  mutate(
    season = case_when(
      lubridate::month(time) == 12 | lubridate::month(time) < 3 ~ 'DJF',
      between(lubridate::month(time), 3, 5) ~ 'MAM',
      between(lubridate::month(time), 6, 8) ~ 'JJA',
      between(lubridate::month(time), 9, 11) ~ 'SON'
    )
  )

p <- ggplot(output_seasons) +
  geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
  geom_point(
    aes(x = longitude, y = latitude, colour = residual),
    size = 0.5,
    shape = 1,
    alpha = 0.3
  ) +
  colorspace::scale_colour_binned_divergingx(
    palette = 'RdYlBu',
    rev = TRUE,
    guide = guide_colorbar(
      title.position = 'top',
      title.hjust = 0.5,
      axis = FALSE,
      label.theme = element_text(size = 8),
      frame.colour = '#999999',
      barwidth = 20
    ),
    n.breaks = 7,
  ) +
  coord_sf(
    default_crs = sf::st_crs('WGS84'),
    crs = sf::st_crs('+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')
  ) +
  facet_wrap(~season, ncol = 2) +
  labs(
    x = NULL,
    y = NULL,
    colour = expression('SIF [W' * m^-2 * µm^-1 * sr^-1 * ']'),
    title = 'SIF residuals by season (2014-09 to 2020-12)'
  ) +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(size = 13, hjust = 0.5),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

ggsave_base(
  '6_results_sif/figures/sif-residuals-map-seasons.png',
  p,
  bg = 'white',
  width = 30,
  height = 20
)
