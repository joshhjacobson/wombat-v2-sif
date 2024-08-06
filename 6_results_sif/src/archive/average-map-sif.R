library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(fst)
library(Matrix)
library(parallel)
library(patchwork)

# source(Sys.getenv('UTILS_PARTIAL'))
# source(Sys.getenv('DISPLAY_PARTIAL'))
source("partials/display.R")
source("partials/utils.R")

args <- list()
args$inventory <- '3_sif/intermediates/sib4-monthly-sif-2x25.nc'
args$model <- '3_sif/intermediates/model-sif-assim.fst'
args$perturbations <- '5_results/intermediates/perturbations-augmented.fst'
args$samples <- '4_inversion/intermediates/samples-LNLGISSIF.rds'
args$region_sf <- '5_results/intermediates/region-sf.rds'

model <- read_fst(args$model)
perturbations <- fst::read_fst(args$perturbations)
samples <- readRDS(args$samples)
region_sf <- readRDS(args$region_sf)


perturbations_base <- perturbations %>%
  filter(
    inventory == 'bio_assim',
    time >= '2015-01-01',
    time < '2021-01-01'
  ) %>%
  mutate(
    # NOTE: sign of perturbations are switched to match SIF (yes confirmed, GPP is damped, thus so is SIF)
    value = -value,
    year_month = factor(time),
    month = lubridate::month(time),
  ) %>%
  inner_join(
    model %>% select(c(
      longitude = model_longitude,
      latitude = model_latitude,
      month,
      slope
    )),
    by = c('longitude', 'latitude', 'month')
  ) %>%
  arrange(time, longitude, latitude) %>%
  mutate(
    value = value * slope,
    month_location = interaction(
      month,
      longitude,
      latitude,
      drop = TRUE
    )
  )

perturbations_sif <- perturbations_base %>%
  group_by(month_location, basis_vector) %>%
  summarise(value = sum(value) / 6, .groups = 'drop') %>%
  left_join(
    perturbations_base %>%
      distinct(month_location, month, longitude, latitude),
    by = 'month_location'
  )

X_alpha_to_six_year_mean_by_month <- with(
  perturbations_sif,
  sparseMatrix(
    i = as.integer(month_location),
    j = as.integer(basis_vector),
    x = value,
    dims = c(nlevels(month_location), nlevels(basis_vector))
  )
)[, as.integer(samples$alpha_df$basis_vector)]

alpha_mean <- samples$alpha_df$value
alpha_samples <- samples$alpha_df$value_samples[
  ,
  floor(seq(1, ncol(samples$alpha_df$value_samples), length.out = 100))
]
value_tilde_mean <- as.vector(X_alpha_to_six_year_mean_by_month %*% alpha_mean)
value_tilde_samples <- as.matrix(X_alpha_to_six_year_mean_by_month %*% alpha_samples)

six_year_average_month <- perturbations_sif %>%
  distinct(month, longitude, latitude) %>%
  mutate(
    output = 'Posterior',
    value_mean = value_tilde_mean,
    value_sd = matrixStats::rowSds(value_tilde_samples)
  ) %>%
  right_join(
    expand.grid(
      month = seq(12),
      longitude = seq(-180, 180, 2.5),
      latitude = seq(-88, 88, 2)
    ),
    by = c('month', 'longitude', 'latitude')
  )


average_posterior_mean <- ggplot() +
  geom_tile(
    data = six_year_average_month,
    mapping = aes(x = longitude, y = latitude, fill = value_mean)
  ) +
  geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
  facet_wrap(~month, ncol = 4) +
  colorspace::scale_fill_binned_divergingx(
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
    na.value = '#cccccc'
  ) +
  labs(
    fill = expression('SIF [W' * m^-2 * µm^-1 * sr^-1 * ']')
  ) +
  ggtitle('Posterior mean (adjustment)')

average_posterior_sd <- ggplot() +
  geom_tile(
    data = six_year_average_month,
    mapping = aes(x = longitude, y = latitude, fill = value_sd)
  ) +
  geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
  facet_wrap(~month, ncol = 4) +
  colorspace::scale_fill_binned_sequential(
    palette = 'Blues 3',
    rev = TRUE,
    guide = guide_colorbar(
      title.position = 'top',
      title.hjust = 0.5,
      axis = FALSE,
      label.theme = element_text(size = 8),
      frame.colour = '#999999',
      barwidth = 20
    ),
    n.breaks = 5,
    na.value = '#cccccc'
  ) +
  labs(
    fill = expression('SIF [W' * m^-2 * µm^-1 * sr^-1 * ']')
  ) +
  ggtitle('Posterior st. dev.')


base_theme <- theme(
  legend.position = 'bottom',
  legend.margin = margin(t = -0.2, l = 0, b = -0.2, r = 0, unit = 'cm'),
  legend.title = element_text(size = 14),
  axis.title = element_blank(),
  plot.title = element_text(
    hjust = 0.5,
    vjust = 1,
    size = 16,
    margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')
  ),
  strip.text = element_text(size = 14),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  panel.spacing = unit(0.05, 'cm'),
  plot.margin = margin(t = 0.4, b = 0.3, l = 0.1, r = 0, unit = 'cm')
)

average_column <- wrap_plots(
  average_posterior_mean + base_theme,
  average_posterior_sd + base_theme,
  ncol = 1
)

output <- wrap_plots(
  wrap_elements(
    panel = grid::textGrob(
      'Month-average SIF from January 2015 to December 2020',
      gp = grid::gpar(fontsize = 20)
    )
  ),
  average_column,
  heights = c(0.08, 1)
)

ggsave_base(
  '6_results_sif/figures/average-map-sif.pdf',
  output,
  width = 40,
  height = 45
)

