library(argparse)
library(dplyr, warn.conflicts = FALSE)
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
parser$add_argument('--output')
args <- parser$parse_args()

args <- list(
  flux_samples_alpha0_wsif = '4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WSIF.rds',
  flux_samples_alpha0_wosif = '4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WOSIF.rds',
  flux_samples_alphav2_wsif = '4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WSIF.rds',
  flux_samples_alphav2_wosif = '4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WOSIF.rds'
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
)

flux_aggregates_samples %>% glimpse

log_debug('Computing metrics')
output <- flux_aggregates_samples %>%
  filter(estimate != 'Truth') %>%
  left_join(
    flux_aggregates_samples %>%
      filter(estimate == 'Truth') %>%
      select(case, inventory, region, time, flux_truth = flux_mean),
    by = c('case', 'inventory', 'region', 'time')
  ) %>%
  group_by(case, estimate, inventory) %>%
  summarise(
    rmse = sqrt(mean((flux_mean - flux_truth)^2)),
    mcrps = mean(scoringRules::crps_sample(
      flux_truth,
      flux_samples
    )),
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
