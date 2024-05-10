library(argparse)
library(dplyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--flux-samples-alpha0-wsif')
parser$add_argument('--flux-samples-alpha0-wosif')
parser$add_argument('--flux-samples-alphav2-wsif')
parser$add_argument('--flux-samples-alphav2-wosif')
parser$add_argument('--flux-samples-alphafree-wsif')
parser$add_argument('--flux-samples-alphafree-wosif')
parser$add_argument('--output')
args <- parser$parse_args()

printf <- function(...) cat(sprintf(...))
collapse0 <- function(x) paste0(x, collapse = '')
paste_columns <- function(x) paste0(x, collapse = ' & ')
paste_columns_min_bold <- function(x, digits = 3) {
  x_ <- round(rmse_vector, digits = digits)
  if (length(unique(x_)) == 1) {
    paste_columns(sprintf(paste0('%.', digits, 'f'), x))
  } else {
    min_x <- min(x)
    paste0(
      ifelse(x == min_x, '\\bfseries ', ''),
      sprintf(paste0('%.', digits, 'f'), x),
      collapse = ' & '
    )
  }
}
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
      truth = 'Bottom-up',
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
      truth = 'Bottom-up',
      estimate = 'Without SIF'
    ),
  read_flux_samples(
    args$flux_samples_alphav2_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      truth = 'WOMBAT v2',
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
      truth = 'WOMBAT v2',
      estimate = 'Without SIF'
    ),
  read_flux_samples(
    args$flux_samples_alphafree_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      truth = 'WOMBAT v2, adjusted',
      estimate = ifelse(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alphafree_wosif,
  ) %>%
    mutate(
      truth = 'WOMBAT v2, adjusted',
      estimate = 'Without SIF'
    )
)

log_debug('Computing metrics')
osse_fluxes <- flux_aggregates_samples %>%
  filter(estimate != 'Truth') %>%
  left_join(
    flux_aggregates_samples %>%
      filter(estimate == 'Truth') %>%
      select(truth, inventory, region, time, flux_truth = flux_mean),
    by = c('truth', 'inventory', 'region', 'time')
  ) %>%
  mutate(
    truth = factor(truth, levels = c('Bottom-up', 'WOMBAT v2', 'WOMBAT v2, adjusted')),
    estimate = factor(estimate, levels = c('Without SIF', 'With SIF'))
  )

metrics <- osse_fluxes %>%
  group_by(truth, estimate, inventory) %>%
  summarise(
    rmse = sqrt(mean((flux_mean - flux_truth)^2)),
    mcrps = mean(scoringRules::crps_sample(flux_truth, flux_samples)),
    .groups = 'drop'
  ) %>%
  arrange(inventory, truth, estimate)

inventories <- levels(metrics$inventory)
truth_cases <- levels(metrics$truth)
estimates <- levels(metrics$estimate)

log_debug('Writing table to {args$output}')
sink(args$output)
printf(
  '\\begin{tabular}{@{}ll%s@{}}\n\\toprule\n',
  paste0(
    rep(collapse0(rep('r', length(estimates))), 2),
    collapse = '|'
  )
)
cat('Component & True Flux & \\multicolumn{2}{c|}{RMSE [PgC per month]} & \\multicolumn{2}{c}{CRPS} \\\\\n')
cat('\\cmidrule(lr){3-4} \\cmidrule(lr){5-6}\n')
printf(
  '& & %s \\\\\n\\toprule\n',
  paste_columns(rep(paste_columns(estimates), 2))
)
for (inventory_i in inventories) {
  for (truth_j in truth_cases) {
    rmse_vector <- metrics %>%
      filter(inventory == inventory_i, truth == truth_j) %>%
      pull(rmse)
    mcrps_vector <- metrics %>%
      filter(inventory == inventory_i, truth == truth_j) %>%
      pull(mcrps)
    printf(
      '%s & %s & %s & %s \\\\\n',
      ifelse(truth_j == truth_cases[2], inventory_i, ''),
      truth_j,
      paste_columns_min_bold(rmse_vector),
      paste_columns_min_bold(mcrps_vector)
    )
  }
  if (inventory_i != tail(inventories, 1)) cat('\\midrule\n')
}
cat('\\bottomrule\n\\end{tabular}\n')
sink(NULL)

log_debug('Done')
