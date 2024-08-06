library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(patchwork)

# source(Sys.getenv('UTILS_PARTIAL'))
# source(Sys.getenv('DISPLAY_PARTIAL'))
source('partials/display.R')
source('partials/utils.R')

# parser <- ArgumentParser()
# parser$add_argument('--area-1x1')
# parser$add_argument('--perturbations-augmented')
# parser$add_argument('--samples')
# parser$add_argument('--region')
# parser$add_argument('--output')
# args <- parser$parse_args()

args <- list()
args$perturbations_augmented <- '5_results/intermediates/perturbations-augmented.fst'
args$samples <- '4_inversion/intermediates/samples-LNLGISSIF-FREERESP.rds'
args$output_base <- '6_results_sif/figures'

samples_type <- sub('samples-(.*)\\.rds', '\\1', basename(args$samples))
output_path <- sprintf('%s/fluxes-regional-monthly-%s.pdf', args$output_base, samples_type)
posterior_label <- samples_type

samples <- readRDS(args$samples)
perturbations_base <- fst::read_fst(args$perturbations_augmented)

perturbations_base <- perturbations_base %>%
  filter(
    between(time, as.Date('2015-01-01'), as.Date('2021-01-01')),
    region %in% sprintf('Region%02d', 1:11)
  ) %>%
  mutate(
    inventory_region_time = interaction(
      inventory,
      region,
      time,
      drop = TRUE
    )
  )

perturbations_region <- perturbations_base %>%
  group_by(inventory_region_time, basis_vector) %>%
  summarise(value = KG_M2_S_TO_PGC_MONTH * sum(area * value)) %>%
  left_join(
    perturbations_base %>%
      distinct(inventory_region_time, inventory, region, time),
    by = 'inventory_region_time'
  )

X_region <- with(perturbations_region, sparseMatrix(
  i = as.integer(inventory_region_time),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(inventory_region_time), nlevels(basis_vector))
))

prior_emissions <- perturbations_region %>%
  group_by(inventory_region_time, inventory, region, time) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  select(-inventory_region_time) %>%
  mutate(output = 'Bottom-up')

posterior_emissions <- prior_emissions %>%
  mutate(
    output = 'Posterior',
    value_prior = value,
    value = value_prior + as.vector(
      X_region[, as.integer(samples$alpha_df$basis_vector)]
      %*% samples$alpha_df$value
    ),
    value_samples = value_prior + as.matrix(
      X_region[, as.integer(samples$alpha_df$basis_vector)]
      %*% samples$alpha_df$value_samples
    ),
    value_q025 = matrixStats::rowQuantiles(value_samples, probs = 0.025),
    value_q975 = matrixStats::rowQuantiles(value_samples, probs = 0.975)
  ) %>%
  select(-value_prior)

emissions <- bind_rows(
  prior_emissions,
  posterior_emissions
) %>%
  filter(
    inventory %in% c('bio_assim', 'bio_resp_tot')
  ) %>%
  mutate(
    inventory = factor(c(
      'bio_assim' = 'GPP',
      'bio_resp_tot' = 'Respiration'
    )[inventory], levels = c(
      'GPP',
      'Respiration'
    ))
  )


output <- ggplot(emissions, aes(time)) +
  geom_line(
    mapping = aes(
      y = value,
      colour = output,
      linetype = output
    ),
    linewidth = 0.4
  ) +
  geom_ribbon(
    mapping = aes(
      ymin = value_q025,
      ymax = value_q975,
      fill = output
    ),
    alpha = 0.3
  ) +
  ggh4x::facet_grid2(region ~ inventory, scales = "free_y", independent = "y") +
  scale_colour_manual(values = c('Bottom-up' = 'black', 'Posterior' = '#ff4444')) +
  scale_fill_manual(values = c('Bottom-up' = 'black', 'Posterior' = '#ff4444')) +
  scale_linetype_manual(values = c('Bottom-up' = '41', 'Posterior' = 'solid')) +
  scale_x_date(date_breaks = '6 months', date_labels = '%Y-%m') +
  guides(fill = 'none') +
  labs(x = 'Time', y = 'Flux [PgC/month]', colour = NULL, fill = NULL, linetype = NULL) +
  ggtitle(sprintf('Monthly regional fluxes, %s', posterior_label)) +
  theme(
    legend.position = 'bottom',
    legend.margin = margin(t = 0, r = 5, b = 0, l = 0, unit = 'mm'),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.title.x = element_text(size = 10),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(size = 8),
    plot.title = element_text(size = 13, hjust = 0.5)
  )


ggsave_base(
  output_path,
  output,
  width = 16,
  height = 32
)
