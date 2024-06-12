library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(patchwork)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented-zonal')
parser$add_argument('--samples-LNLGIS')
parser$add_argument('--samples-LNLGISSIF')
parser$add_argument('--fluxcom-monthly-2x25')
parser$add_argument('--output')
args <- parser$parse_args()

perturbations_zonal <- fst::read_fst(args$perturbations_augmented_zonal)
samples_LNLGIS <- readRDS(args$samples_LNLGIS)
samples_LNLGISSIF <- readRDS(args$samples_LNLGISSIF)
fluxcom_monthly_2x25_zonal <- fst::read_fst(args$fluxcom_monthly_2x25_zonal)

# TODO: remove once we have full LNLGISSIF samples
samples_LNLGIS$alpha_df$value_samples <- samples_LNLGIS$alpha_df$value_samples[
  , 
  1:ncol(samples_LNLGISSIF$alpha_df$value_samples)
]

perturbations_zonal <- perturbations_zonal %>%
  mutate(
    month = lubridate::month(time),
    inventory_month = interaction(inventory, month, drop = TRUE)
  )

fluxcom_zonal <- fluxcom_monthly_2x25_zonal %>%
  mutate(
    inventory = factor(c(
      'GPP' = 'bio_assim',
      'Respiration' = 'bio_resp_tot',
      'NEE' = 'nee'
    )[inventory], levels = c(
      'bio_assim',
      'bio_resp_tot',
      'nee'
    )),
    month = lubridate::month(time),
    inventory_month = interaction(inventory, month, drop = TRUE)
  )

emissions_zonal <- lapply(REGION_PLOT_SETTINGS, function(zonal_band) {
  fluxcom_zone <- fluxcom_zonal %>%
    filter(
      latitude_bottom >= zonal_band$latitude_lower,
      latitude_bottom < zonal_band$latitude_upper
    ) %>%
    group_by(inventory_month, method) %>%
    summarise(
      # Average over the six-year study period
      value = GC_M2_DAY_TO_PGC_MONTH * sum(area * value) / 6,
      .groups = 'drop'
    ) %>%
    left_join(
      fluxcom_zonal %>%
        distinct(inventory_month, inventory, month),
      by = 'inventory_month'
    ) %>%
    group_by(inventory_month, inventory, month) %>%
    summarise(
      value_mean = mean(value),
      value_q025 = quantile(value, probs = 0.025),
      value_q975 = quantile(value, probs = 0.975),
      .groups = 'drop'
    ) %>%
    rename(value = value_mean) %>%
    select(-inventory_month) %>%
    mutate(estimate = 'FLUXCOM')

  perturbations_zone <- perturbations_zonal %>%
    filter(
      latitude_bottom >= zonal_band$latitude_lower,
      latitude_bottom < zonal_band$latitude_upper
    ) %>%
    group_by(inventory_month, basis_vector) %>%
    summarise(
      # Average over the six-year study period
      value = KG_M2_S_TO_PGC_MONTH * sum(area * value) / 6,
      .groups = 'drop'
    ) %>%
    left_join(
      perturbations_zonal %>%
        distinct(inventory_month, inventory, month),
      by = 'inventory_month'
    )

  X_zone <- with(perturbations_zone, sparseMatrix(
    i = as.integer(inventory_month),
    j = as.integer(basis_vector),
    x = value,
    dims = c(nlevels(inventory_month), nlevels(basis_vector))
  ))

  prior_emissions <- perturbations_zone %>%
    group_by(inventory_month, inventory, month) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    select(-inventory_month) %>%
    mutate(estimate = 'Bottom-up')

  posterior_emissions_LNLGIS <- compute_posterior(prior_emissions, X_zone, samples_LNLGIS, 'Posterior v2')
  posterior_emissions_LNLGISSIF <- compute_posterior(prior_emissions, X_zone, samples_LNLGISSIF, 'Posterior v2S')

  emissions_zone <- bind_rows(
    prior_emissions,
    posterior_emissions_LNLGIS,
    posterior_emissions_LNLGISSIF
  ) %>%
    {
      x <- .

      bind_rows(
        x,
        x %>%
          filter(inventory %in% c('bio_assim', 'bio_resp_tot')) %>%
          group_by(estimate, month) %>%
          summarise(
            value = sum(value),
            value_samples = t(colSums(value_samples)),
            .groups = 'drop'
          ) %>%
          mutate(
            inventory = 'nee',
            value_q025 = matrixStats::rowQuantiles(value_samples, probs = 0.025),
            value_q975 = matrixStats::rowQuantiles(value_samples, probs = 0.975)
          )
      )
    } %>%
    bind_rows(fluxcom_zone) %>%
    mutate(zone = zonal_band$numeric_title)
}) %>%
  bind_rows() %>%
  mutate(
    inventory = factor(c(
      'bio_assim' = 'GPP',
      'bio_resp_tot' = 'Respiration',
      'nee' = 'NEE',
      'ocean' = 'Ocean'
    )[inventory], levels = c(
      'GPP',
      'Respiration',
      'NEE',
      'Ocean'
    )),
    estimate = factor(
      estimate,
      levels = c('FLUXCOM', 'Bottom-up', 'Posterior v2', 'Posterior v2S')
    ),
    zone = factor(
      zone,
      levels = sapply(REGION_PLOT_SETTINGS, function(zonal_band) zonal_band$numeric_title)
    )
  )


colour_key <- c(
  'FLUXCOM' = 'grey70',
  'Bottom-up' = 'black',
  'Posterior v2' = 'grey50',
  'Posterior v2S' = '#ebac23'
)
linetype_key <- c(
  'FLUXCOM' = 'solid',
  'Bottom-up' = '41',
  'Posterior v2' = 'solid',
  'Posterior v2S' = 'solid'
)
shape_key <- c(
  'FLUXCOM' = 4,
  'Bottom-up' = 16,
  'Posterior v2' = 15,
  'Posterior v2S' = 17
)

output <- emissions_zonal %>%
  filter(
    inventory %in% c('GPP', 'Respiration', 'NEE')
  ) %>%
  ggplot(aes(x = month)) +
  geom_ribbon(
    mapping = aes(
      ymin = value_q025,
      ymax = value_q975,
      fill = estimate
    ),
    alpha = 0.3
  ) +
  geom_line(
    mapping = aes(
      y = value,
      colour = estimate,
      linetype = estimate
    ),
    linewidth = 0.4
  ) +
  geom_point(
    mapping = aes(
      y = value,
      colour = estimate,
      shape = estimate
    ),
  ) +
  ggh4x::facet_grid2(zone ~ inventory, scales = 'free_y', independent = 'y') +
  scale_y_continuous(n.breaks = 4) +
  scale_x_continuous(
    breaks = seq(1, 11, 2),
    labels = month.abb[c(TRUE, FALSE)]
  ) +
  scale_colour_manual(values = colour_key) +
  scale_fill_manual(values = colour_key) +
  scale_linetype_manual(values = linetype_key) +
  scale_shape_manual(values = shape_key) +
  guides(fill = 'none', shape = 'none') +
  labs(x = 'Month', y = 'Flux [PgC per month]', colour = NULL, fill = NULL, linetype = NULL, shape = NULL) +
  ggtitle(
    'Average seasonal cycle by zonal band'
  ) +
  theme(
    plot.margin = margin(t = 1, r = 1, b = 0, l = 1, unit = 'mm'),
    plot.title = element_text(size = 13, hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 10),
    strip.text.x = element_text(size = 11),
    strip.text.y = element_text(size = 10),
    legend.position = 'bottom',
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm')
  )

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$supplement_full_width,
  height = DISPLAY_SETTINGS$full_height
)
