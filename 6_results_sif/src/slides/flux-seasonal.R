library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(Matrix)

source('partials/utils.R')
source('partials/display.R')

args <- list(
  perturbations_augmented = '6_results_sif/intermediates/perturbations-augmented.fst',
  samples_LNLGIS = '4_inversion/intermediates/samples-LNLGIS.rds',
  samples_LNLGISSIF = '4_inversion/intermediates/samples-LNLGISSIF.rds',
  xbase_monthly_2x25 = '6_results_sif/intermediates/xbase-monthly-2x25.fst',
  output = '6_results_sif/figures/slides/flux-seasonal.pdf'
)


perturbations_base <- fst::read_fst(args$perturbations_augmented)
samples_LNLGIS <- readRDS(args$samples_LNLGIS)
samples_LNLGISSIF <- readRDS(args$samples_LNLGISSIF)
xbase_monthly_2x25 <- fst::read_fst(args$xbase_monthly_2x25)

# TODO: remove once we have full LNLGISSIF samples
samples_LNLGIS$alpha_df$value_samples <- samples_LNLGIS$alpha_df$value_samples[
  , 
  1:ncol(samples_LNLGISSIF$alpha_df$value_samples)
]


xbase_seasonal <- xbase_monthly_2x25 %>%
  mutate(
    month = month(time)
  ) %>%
  group_by(inventory, month) %>%
  summarise(
    # Average over the six-year study period
    value = GC_M2_DAY_TO_PGC_MONTH * sum(area * value) / 6,
    .groups = 'drop'
  ) %>%
  mutate(estimate = 'FLUXCOM')

perturbations_seasonal_base <- perturbations_base %>%
  mutate(
    month = month(time),
    inventory_month = interaction(inventory, month, drop = TRUE)
  )

perturbations_seasonal <- perturbations_seasonal_base %>%
  group_by(inventory_month, basis_vector) %>%
  summarise(
    # Average over the six-year study period
    value = KG_M2_S_TO_PGC_MONTH * sum(area * value) / 6,
    .groups = 'drop'
  ) %>%
  left_join(
    perturbations_seasonal_base %>%
      distinct(inventory_month, inventory, month),
    by = 'inventory_month'
  )

X_global_seasonal <- with(perturbations_seasonal, sparseMatrix(
  i = as.integer(inventory_month),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(inventory_month), nlevels(basis_vector))
))

prior_emissions_seasonal <- perturbations_seasonal %>%
  group_by(inventory_month, inventory, month) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  select(-inventory_month) %>%
  mutate(estimate = 'Bottom-up')

posterior_emissions_seasonal_LNLGIS <- compute_posterior(
  prior_emissions_seasonal,
  X_global_seasonal,
  samples_LNLGIS,
  'v2.0 posterior'
)
posterior_emissions_seasonal_LNLGISSIF <- compute_posterior(
  prior_emissions_seasonal,
  X_global_seasonal,
  samples_LNLGISSIF,
  'v2.S posterior'
)

emissions_seasonal <- bind_rows(
  prior_emissions_seasonal,
  posterior_emissions_seasonal_LNLGIS,
  posterior_emissions_seasonal_LNLGISSIF
) %>%
  filter(inventory %in% c('bio_assim', 'bio_resp_tot')) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
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
  bind_rows(xbase_seasonal) %>%
  mutate(
    inventory = factor(c(
      'bio_assim' = 'GPP',
      'bio_resp_tot' = 'Respiration',
      'nee' = 'NEE'
    )[inventory], levels = c(
      'GPP',
      'Respiration',
      'NEE'
    )),
    estimate = factor(
      estimate,
      levels = c('FLUXCOM', 'Bottom-up', 'v2.0 posterior', 'v2.S posterior')
    )
  )

output <- emissions_seasonal %>%
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
    linewidth = 0.6
  ) +
  facet_wrap(vars(inventory), scales = 'free_y', nrow = 1) +
  scale_x_continuous(
    breaks = seq(1, 11, 2),
    labels = month.abb[c(TRUE, FALSE)]
  ) +
  scale_colour_manual(values = DISPLAY_SETTINGS$colour_key) +
  scale_fill_manual(values = DISPLAY_SETTINGS$colour_key) +
  scale_linetype_manual(values = DISPLAY_SETTINGS$linetype_key) +
  guides(fill = 'none') +
  labs(y = 'Flux [PgC/month]', x = NULL, colour = NULL, fill = NULL, linetype = NULL) +
  theme(
    strip.text = element_text(size = 10),
    plot.margin = margin(t = 0, r = 0.1, b = 0, l = 0.1, unit = 'cm'),
    axis.text.x = element_text(size = 8, colour = '#23373b'),
    axis.text.y = element_text(size = 7, colour = '#23373b'),
    axis.title.y = element_text(size = 10, colour = '#23373b'),
    legend.text = element_text(size = 9),
    legend.position = 'bottom',
    legend.margin = margin(t = -0.2, r = 0, b = 0, l = 0, unit = 'cm')
  )

ggsave_base(
  args$output,
  output,
  width = 16.5,
  height = 4.5
)
