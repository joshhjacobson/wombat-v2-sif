library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--flux-samples-wsif')
parser$add_argument('--flux-samples-wosif')
parser$add_argument('--osse-base-case')
parser$add_argument('--output')
args <- parser$parse_args()

read_flux_samples <- function(filename, estimates = 'Posterior') {
  readRDS(filename) %>% filter(estimate %in% estimates)
}

osse_base_case <- OSSE_PLOT_SETTINGS[[args$osse_base_case]]

flux_aggregates_samples <- bind_rows(
  read_flux_samples(
    args$flux_samples_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      estimate = if_else(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_wosif,
  ) %>%
    mutate(
      estimate = 'Without SIF'
    )
)

osse_fluxes <- flux_aggregates_samples %>%
  filter(estimate != 'Truth') %>%
  left_join(
    flux_aggregates_samples %>%
      filter(estimate == 'Truth') %>%
      select(inventory, region, time, flux_truth = flux_mean),
    by = c('inventory', 'region', 'time')
  ) %>%
  mutate(
    estimate = factor(estimate, levels = c('Without SIF', 'With SIF'))
  )

metrics_monthly <- osse_fluxes %>%
  filter(
    inventory %in% c('GPP', 'Respiration', 'NEE')
  ) %>%
  group_by(inventory, estimate, time) %>%
  summarise(
    rmse = sqrt(mean((flux_mean - flux_truth)^2)),
    mcrps = mean(scoringRules::crps_sample(flux_truth, flux_samples)),
    .groups = 'drop'
  ) %>%
  pivot_longer(
    cols = c(rmse, mcrps),
    names_to = 'metric',
    values_to = 'value'
  ) %>%
  mutate(
    metric = factor(c(
      'rmse' = 'RMSE [PgC/month]',
      'mcrps' = 'CRPS'
    )[metric], c(
      'RMSE [PgC/month]',
      'CRPS'
    ))
  )

output <- ggplot(
  metrics_monthly,
  aes(x = time, order = estimate)
) +
  geom_line(
    aes(
      y = value,
      colour = estimate
    )
  ) +
  ggh4x::facet_grid2(
    inventory ~ metric,
    scales = 'free_y',
    independent = 'y',
  ) +
  ylim(0, NA) +
  # scale_y_continuous(n.breaks = 3, limits = c(0, NA)) +
  scale_x_date(date_labels = '%Y-%m') +
  scale_colour_manual(
    values = c('Without SIF' = 'grey50', 'With SIF' = '#fb8b00')
  ) +
  theme(
    plot.title = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 7),
    strip.text.x = element_text(size = 9),
    strip.text.y = element_text(size = 8),
    legend.position = if (osse_base_case$in_supplement) 'right' else 'bottom',
    legend.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = 'mm'),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
  )

ggsave_base(
  args$output,
  output,
  width = if (osse_base_case$in_supplement) {
    DISPLAY_SETTINGS$supplement_full_width
  } else {
    DISPLAY_SETTINGS$full_width
  },
  height = if (osse_base_case$in_supplement) 5.7 else 6.94
)
