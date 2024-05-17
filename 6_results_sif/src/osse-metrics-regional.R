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
    region = factor(stringr::str_replace(region, 'Region', 'T')),
    estimate = factor(estimate, levels = c('Without SIF', 'With SIF'))
  )

metrics_regional <- osse_fluxes %>%
  filter(
    inventory %in% c('GPP', 'Respiration', 'NEE'),
    region %in% sprintf('T%02d', 1:11)
  ) %>%
  group_by(inventory, estimate, region) %>%
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
      'rmse' = 'RMSE [PgC per month]',
      'mcrps' = 'CRPS'
    )[metric], c(
      'RMSE [PgC per month]',
      'CRPS'
    )),
    region_n = as.numeric(region)
  )

region_labels <- metrics_regional %>%
  distinct(region, region_n)
regions_shaded <- region_labels %>%
  filter(region %in% sprintf('T%02d', seq(2, 10, 2)))

output <- ggplot() +
  geom_rect(
    data = regions_shaded,
    mapping = aes(
      xmin = region_n - 0.5,
      xmax = region_n + 0.5,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = 'grey90',
    alpha = 0.5
  ) +
  geom_point(
    data = metrics_regional,
    mapping = aes(
      x = region_n,
      y = value,
      colour = estimate
    ),
    shape = 16,
    position = position_dodge(width = 0.5)
  ) +
  ggh4x::facet_grid2(
    inventory ~ metric,
    scales = 'free_y',
    independent = 'y',
  ) +
  scale_x_continuous(
    breaks = region_labels$region_n,
    labels = region_labels$region
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    n.breaks = 3
  ) +
  scale_colour_manual(
    values = c('Without SIF' = 'grey50', 'With SIF' = '#fb8b00')
  ) +
  theme(
    plot.title = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 7),
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
