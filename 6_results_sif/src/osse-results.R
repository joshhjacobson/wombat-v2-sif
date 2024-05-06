library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)
library(tidyr, warn.conflicts = FALSE)

# source(Sys.getenv('UTILS_PARTIAL'))
# source(Sys.getenv('DISPLAY_PARTIAL'))
source('partials/display.R')
source('partials/utils.R')

parser <- ArgumentParser()
parser$add_argument('--flux-samples-alpha0-wsif')
parser$add_argument('--flux-samples-alpha0-wosif')
parser$add_argument('--flux-samples-alphav2-wsif')
parser$add_argument('--flux-samples-alphav2-wosif')
parser$add_argument('--flux-samples-alphafree-wsif')
parser$add_argument('--flux-samples-alphafree-wosif')
parser$add_argument('--output')
args <- parser$parse_args()

args <- list(
  flux_samples_alpha0_wsif = '4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WSIF.rds',
  flux_samples_alpha0_wosif = '4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WOSIF.rds',
  flux_samples_alphav2_wsif = '4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WSIF.rds',
  flux_samples_alphav2_wosif = '4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WOSIF.rds',
  flux_samples_alphav2_wsif = '4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAFREE-WSIF.rds',
  flux_samples_alphav2_wosif = '4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAFREE-WOSIF.rds'
)

read_flux_samples <- function(filename, estimates = 'Posterior') {
  readRDS(filename) %>% filter(estimate %in% estimates)
}

log_debug('Loading flux samples')
flux_aggregates_samples <- bind_rows(
  read_flux_samples(
    args$flux_samples_alpha0_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      case = 'ALPHA0',
      estimate = ifelse(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alpha0_wosif,
  ) %>%
    mutate(
      case = 'ALPHA0',
      estimate = 'Without SIF'
    ),
  read_flux_samples(
    args$flux_samples_alphav2_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      case = 'ALPHAV2',
      estimate = ifelse(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alphav2_wosif,
  ) %>%
    mutate(
      case = 'ALPHAV2',
      estimate = 'Without SIF'
    )
  # read_flux_samples(
  #   args$flux_samples_alphafree_wsif,
  #   c('Truth', 'Posterior')
  # ) %>%
  #   mutate(
  #     case = 'ALPHAFREE',
  #     estimate = ifelse(
  #       estimate == 'Posterior',
  #       'With SIF',
  #       estimate
  #     )
  #   ),
  # read_flux_samples(
  #   args$flux_samples_alphafree_wosif,
  # ) %>%
  #   mutate(
  #     case = 'ALPHAFREE',
  #     estimate = 'Without SIF'
  #   )
)

flux_aggregates_samples %>% glimpse

log_debug('Computing metrics')
osse_fluxes <- flux_aggregates_samples %>%
  filter(estimate != 'Truth') %>%
  left_join(
    flux_aggregates_samples %>%
      filter(estimate == 'Truth') %>%
      select(case, inventory, region, time, flux_truth = flux_mean),
    by = c('case', 'inventory', 'region', 'time')
  ) %>%
  mutate(
    region = stringr::str_replace(region, 'Region', 'T'),
    case = factor(case, levels = c('ALPHA0', 'ALPHAV2', 'ALPHAFREE')),
    estimate = factor(estimate, levels = c('Without SIF', 'With SIF')),
    crps = scoringRules::crps_sample(flux_truth, flux_samples)
  )

metrics_region <- bind_rows(
  osse_fluxes %>%
    filter(
      inventory %in% c('GPP', 'Respiration', 'NEE'),
      region %in% sprintf('Region%02d', 1:11)
    ) %>%
    group_by(case, estimate, inventory, region) %>%
    summarise(
      metric = 'rmse',
      value = sqrt(mean((flux_mean - flux_truth)^2)),
      .groups = 'drop'
    ),
  osse_fluxes %>%
    filter(
      inventory %in% c('GPP', 'Respiration', 'NEE'),
      region %in% sprintf('Region%02d', 1:11)
    ) %>%
    group_by(case, estimate, inventory, region) %>%
    summarise(
      metric = 'crps',
      value = mean(crps),
      q025 = quantile(crps, probs = 0.025),
      q975 = quantile(crps, probs = 0.975),
      .groups = 'drop'
    )
)

metrics_time <- bind_rows(
  osse_fluxes %>%
    filter(
      inventory %in% c('GPP', 'Respiration', 'NEE')
    ) %>%
    group_by(case, estimate, inventory, time) %>%
    summarise(
      metric = 'rmse',
      value = sqrt(mean((flux_mean - flux_truth)^2)),
      .groups = 'drop'
    ),
  osse_fluxes %>%
    filter(
      inventory %in% c('GPP', 'Respiration', 'NEE')
    ) %>%
    group_by(case, estimate, inventory, time) %>%
    summarise(
      metric = 'crps',
      value = mean(crps),
      q025 = quantile(crps, probs = 0.025),
      q975 = quantile(crps, probs = 0.975),
      .groups = 'drop'
    )
)

metrics_overall <- osse_fluxes %>%
  group_by(case, estimate, inventory) %>%
  summarise(
    rmse = sqrt(mean((flux_mean - flux_truth)^2)),
    mcrps = mean(crps),
    .groups = 'drop'
  )

estimates <- unique(sort(output$estimate))
inventories <- unique(sort(output$inventory))

rmse_matrix <- output %>%
  select(case, estimate, inventory, rmse) %>%
  pivot_wider(names_from = inventory, values_from = rmse)

mcrps_matrix <- output %>%
  select(case, estimate, inventory, mcrps) %>%
  pivot_wider(names_from = inventory, values_from = mcrps)

  # %>%
  # as.matrix()

# sink(args$output)
# printf(
#   '\\begin{tabular}{l%s}\n\\hline\n',
#   paste0(
#     rep(collapse0(rep('l', length(groups))), 2),
#     collapse = ''
#   )
# )
# cat('& \\multicolumn{2}{c}{RMSE [PgC mo$^{-1}$]} & \\multicolumn{2}{c}{CRPS} \\\\\n')
# printf(
#   'Setup & %s \\\\\n \\hline\n',
#   paste_columns(rep(paste_columns(groups), 2))
# )
# for (i in seq_along(estimates)) {
#   printf(
#     '%s & %s & %s \\\\\n',
#     estimates[i],
#     paste_columns(sprintf('%.03f', rmse_matrix[i, ])),
#     paste_columns(sprintf('%.03f', mcrps_matrix[i, ]))
#   )
# }
# cat('\\hline\n\\end{tabular}\n')
# sink(NULL)


plot_regions <- function(df) {
  ggplot(
    df,
    aes(x = region, colour = estimate, order = estimate)
  ) +
    geom_hline(yintercept = 0, colour = 'black') +
    geom_pointrange(
      aes(
        y = value,
        ymin = q025,
        ymax = q975,
        shape = case
      ),
      fatten = 2,
      position = position_dodge(width = 0.75)
    ) +
    facet_wrap(~ inventory, scales = 'free_y', nrow = 3) +
    scale_colour_manual(
      name = 'Posterior',
      values = c('Without SIF' = 'grey70', 'With SIF' = '#fb8b00')
    ) +
    scale_shape_manual(
      name = 'Truth',
      values = c(
        'ALPHA0' = 'circle small',
        'ALPHAV2' = 'square',
        'ALPHAFREE' = 'triangle'
      ),
      labels = c(
        'Bottom-up',
        'WOMBAT v2',
        'WOMBAT v2, modified'
      )
    )
}

plot_region_rmse <- metrics_region %>%
  filter(metric == 'rmse') %>%
  plot_regions() +
  labs(
    x = element_blank(),
    y = 'RMSE [PgC/month]'
  )

plot_region_crps <- metrics_region %>%
  filter(metric == 'crps') %>%
  plot_regions() +
  labs(
    x = element_blank(),
    y = 'CRPS [unitless]'
  )

plot_region  <- (plot_region_rmse | plot_region_crps) +
  plot_layout(guides = 'collect') &
  theme(
    plot.margin = margin(t = 1, r = 1, b = 0, l = 1, unit = 'mm'),
    plot.title = element_blank(),
    legend.position = 'bottom',
    legend.margin = margin(t = 0, r = 5, b = 0, l = 0, unit = 'mm'),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 9),
    strip.text = element_text(size = 8)
  )

ggsave_base(
  '6_results_sif/figures/osse/metrics-regional.pdf',
  plot_region,
  width = 16.5,
  height = 12
)


plot_months <- function(df) {
  ggplot(
    df,
    aes(x = time, order = estimate)
  ) +
    geom_hline(yintercept = 0, colour = 'black') +
    # geom_ribbon(
    #   aes(
    #     ymin = q025,
    #     ymax = q975,
    #     fill = estimate,
    #     linetype = case
    #   ),
    #   alpha = 0.3
    # ) +
    geom_line(
      aes(
        y = value,
        colour = estimate,
        linetype = case
      ),
      linewidth = 0.5
    ) +
    facet_wrap(~ inventory, scales = 'free_y', nrow = 3) +
    scale_x_date(date_breaks = '6 months', date_labels = '%Y-%m') +
    scale_colour_manual(
      name = 'Posterior',
      values = c('Without SIF' = 'grey70', 'With SIF' = '#fb8b00')
    ) +
    scale_fill_manual(
      name = 'Posterior',
      values = c('Without SIF' = 'grey70', 'With SIF' = '#fb8b00')
    ) +
    scale_linetype_manual(
      name = 'Truth',
      values = c(
        'ALPHA0' = '11',
        'ALPHAV2' = '42',
        'ALPHAFREE' = 'solid'
      ),
      labels = c(
        'Bottom-up',
        'WOMBAT v2',
        'WOMBAT v2, modified'
      )
    )
}

plot_time_rmse <- metrics_time %>%
  filter(metric == 'rmse') %>%
  plot_months() +
  labs(
    x = element_blank(),
    y = 'RMSE [PgC/month]'
  )

plot_time_crps <- metrics_time %>%
  filter(metric == 'crps') %>%
  plot_months() +
  labs(
    x = element_blank(),
    y = 'CRPS [unitless]'
  )

plot_time  <- (plot_time_rmse | plot_time_crps) +
  plot_layout(guides = 'collect') &
  theme(
    plot.margin = margin(t = 1, r = 1, b = 0, l = 1, unit = 'mm'),
    plot.title = element_blank(),
    legend.position = 'bottom',
    legend.margin = margin(t = 0, r = 5, b = 0, l = 0, unit = 'mm'),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 9),
    strip.text = element_text(size = 8)
  )

ggsave_base(
  '6_results_sif/figures/osse/metrics-months.pdf',
  plot_time,
  width = 16.5,
  height = 12
)
