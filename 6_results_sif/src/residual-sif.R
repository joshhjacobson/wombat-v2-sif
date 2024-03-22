library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(fst)
library(Matrix)
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

perturbations <- perturbations_base %>%
  filter(
    inventory == "bio_assim"
  ) %>%
  left_join(
    samples$alpha_df %>%
      select(basis_vector, alpha = value),
    by = "basis_vector"
  ) %>%
  mutate(
    value = alpha * value # NOTE: or is it 1 + alpha?
  ) %>%
  group_by(longitude, latitude, time) %>%
  summarise(value = sum(value), .groups = "drop")

output <- output %>%
  mutate(
    month = strftime(model_time, "%Y-%m")
  ) %>%
  left_join(
    perturbations %>%
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
    # NOTE: perturbations multiplied by -1 to match sign of SIF
    fitted = value_control + (-1) * slope * perturbation,
    residual = offset - (-1) * slope * perturbation
  )

p <- ggplot(output) +
  geom_point(aes(x = fitted, y = residual), shape = 1, alpha = 0.1) +
  labs(
    x = "Fitted SIF",
    y = "Residual SIF",
    title = "Fitted vs Residual SIF (2014-09 to 2020-12)"
  )

ggsave_base(
  '6_results_sif/figures/sif-fitted-vs-residual.png',
  p,
  bg = 'white',
  width = 12,
  height = 12
)

ggplot(output) +
  geom_histogram(aes(x = residual), binwidth = 0.1) +
  theme_minimal() +
  labs(
    x = "Residual",
    y = "Count"
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
    offset = mean(offset, na.rm = TRUE),
    perturbation = mean(-slope * perturbation, na.rm = TRUE),
    residual = mean(residual, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  tidyr::pivot_longer(
    c(value, value_control, offset, perturbation, residual),
    names_to = 'stage',
    values_to = 'value'
  ) %>%
  mutate(
    month = as.Date(paste0(month, '-01')),
    stage = factor(c(
      'value' = 'OCO-2',
      'value_control' = 'Bottom-up',
      'offset' = 'Offset',
      'perturbation' = 'Perturbation',
      'residual' = 'Residual'
    )[stage], levels = c(
      'OCO-2',
      'Bottom-up',
      'Offset',
      'Perturbation',
      'Residual'
    )),
    plot_group = case_when(
      stage %in% c('OCO-2', 'Bottom-up') ~ 'OCO-2 and Bottom-up',
      stage == 'Offset' ~ 'Offset',
      stage == 'Perturbation' ~ 'Perturbation',
      stage == 'Residual' ~ 'Residual'
    )
  )

p <- ggplot(output_global_monthly) +
  geom_line(aes(x = month, y = value, colour = stage)) +
  facet_wrap(~ plot_group, ncol = 1, scales = 'free_y') +
  scale_colour_brewer(palette = 'Dark2') +
  labs(
    x = "Month",
    y = "SIF",
    colour = NULL,
    title = "Global monthly mean SIF components"
  )

ggsave_base(
  '6_results_sif/figures/sif-components-global.pdf',
  p,
  width = 16,
  height = 12
)



p <-  ggplot(output) +
  geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
  geom_point(
    aes(x = longitude, y = latitude, colour = residual),
    size = 0.5,
    shape = 1,
    alpha = 0.3
  ) +
  colorspace::scale_color_binned_divergingx(
    palette = 'Roma',
    rev = TRUE,
    guide = guide_colorbar(
      title.position = 'top',
      title.hjust = 0.5,
      axis = FALSE,
      label.theme = element_text(size = 8),
      frame.colour = '#999999',
      barwidth = 13
    )
  ) +
  coord_sf(
    default_crs = sf::st_crs('WGS84'),
    crs = sf::st_crs('+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')
  ) +
  labs(x = NULL, y = NULL, colour = 'SIF', title = 'SIF residuals (2014-09 to 2020-12)') +
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
  colorspace::scale_color_binned_divergingx(
    palette = 'Roma',
    rev = TRUE,
    guide = guide_colorbar(
      title.position = 'top',
      title.hjust = 0.5,
      axis = FALSE,
      label.theme = element_text(size = 8),
      frame.colour = '#999999',
      barwidth = 13
    )
  ) +
  coord_sf(
    default_crs = sf::st_crs('WGS84'),
    crs = sf::st_crs('+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs')
  ) +
  facet_wrap(~ season, ncol = 2) +
  labs(x = NULL, y = NULL, colour = 'SIF', title = 'SIF residuals by season (2014-09 to 2020-12)') +
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
