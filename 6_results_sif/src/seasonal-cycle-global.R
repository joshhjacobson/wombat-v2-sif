library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented')
parser$add_argument('--samples-LNLGIS')
parser$add_argument('--samples-LNLGISSIF')
parser$add_argument('--fluxcom-monthly-2x25')
parser$add_argument('--output')
args <- parser$parse_args()

perturbations_base <- fst::read_fst(args$perturbations_augmented)
samples_LNLGIS <- readRDS(args$samples_LNLGIS)
samples_LNLGISSIF <- readRDS(args$samples_LNLGISSIF)
fluxcom_monthly_2x25 <- fst::read_fst(args$fluxcom_monthly_2x25)

# TODO: remove once we have full LNLGISSIF samples
samples_LNLGIS$alpha_df$value_samples <- samples_LNLGIS$alpha_df$value_samples[
  , 
  1:ncol(samples_LNLGISSIF$alpha_df$value_samples)
]

fluxcom_month_averages <- fluxcom_monthly_2x25 %>%
  mutate(
    month = lubridate::month(time)
  ) %>%
  group_by(inventory, month, method) %>%
  summarise(
    # Average over the six-year study period
    value = GC_M2_DAY_TO_PGC_MONTH * sum(area * value) / 6,
    .groups = 'drop'
  ) %>%
  group_by(inventory, month) %>%
  summarise(
    value_mean = mean(value),
    value_low = quantile(value, 0.25),
    value_high = quantile(value, 0.75),
    .groups = 'drop'
  ) %>%
  rename(value = value_mean) %>%
  mutate(estimate = 'FLUXCOM')

perturbations_base <- perturbations_base %>%
  mutate(
    month = lubridate::month(time),
    inventory_month = interaction(inventory, month, drop = TRUE)
  )

perturbations <- perturbations_base %>%
  group_by(inventory_month, basis_vector) %>%
  summarise(
    # Average over the six-year study period
    value = KG_M2_S_TO_PGC_MONTH * sum(area * value) / 6,
    .groups = 'drop'
  ) %>%
  left_join(
    perturbations_base %>%
      distinct(inventory_month, inventory, month),
    by = 'inventory_month'
  )

X_global <- with(perturbations, sparseMatrix(
  i = as.integer(inventory_month),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(inventory_month), nlevels(basis_vector))
))

prior_emissions <- perturbations %>%
  group_by(inventory_month, inventory, month) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  select(-inventory_month) %>%
  mutate(estimate = 'Bottom-up')

posterior_emissions_LNLGIS <- compute_posterior(prior_emissions, X_global, samples_LNLGIS, 'v2.0 posterior') %>%
  rename(
    value_low = value_q025,
    value_high = value_q975
  )
posterior_emissions_LNLGISSIF <- compute_posterior(prior_emissions, X_global, samples_LNLGISSIF, 'v2.S posterior') %>%
  rename(
    value_low = value_q025,
    value_high = value_q975
  )

emissions <- bind_rows(
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
          value_low = matrixStats::rowQuantiles(value_samples, probs = 0.025),
          value_high = matrixStats::rowQuantiles(value_samples, probs = 0.975)
        )
    )
  } %>%
  bind_rows(fluxcom_month_averages) %>%
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
      levels = c('FLUXCOM', 'Bottom-up', 'v2.0 posterior', 'v2.S posterior')
    )
  )

colour_key <- c(
  'FLUXCOM' = 'grey70',
  'Bottom-up' = 'grey30',
  'v2.0 posterior' = 'grey50',
  'v2.S posterior' = '#fb8b00'
)
linetype_key <- c(
  'FLUXCOM' = '12',
  'Bottom-up' = '41',
  'v2.0 posterior' = '1131',
  'v2.S posterior' = 'solid'
)

output <- emissions %>%
  filter(
    inventory %in% c('GPP', 'Respiration', 'NEE')
  ) %>%
  ggplot(aes(x = month)) +
  geom_ribbon(
    mapping = aes(
      ymin = value_low,
      ymax = value_high,
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
  # facet_wrap(vars(inventory), scales = 'free_y', ncol = 1) +
  facet_wrap(vars(inventory), scales = 'free_y', nrow = 1) +
  scale_x_continuous(
    breaks = seq(1, 11, 2),
    labels = month.abb[c(TRUE, FALSE)]
  ) +
  scale_colour_manual(values = colour_key) +
  scale_fill_manual(values = colour_key) +
  scale_linetype_manual(values = linetype_key) +
  guides(
    fill = 'none'
    # colour = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(x = 'Month', y = 'Flux [PgC/month]', colour = NULL, fill = NULL, linetype = NULL) +
  ggtitle('Average seasonal cycle of global fluxes') +
  theme(
    plot.margin = margin(t = 1, r = 1, b = 0, l = 1, unit = 'mm'),
    plot.title = element_text(size = 13, hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(size = 11),
    legend.position = 'bottom',
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm')
  )

# ggsave_base(
#   args$output,
#   output,
#   width = 9,
#   height = 18
# )
ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$supplement_full_width,
  height = 6.9
)
