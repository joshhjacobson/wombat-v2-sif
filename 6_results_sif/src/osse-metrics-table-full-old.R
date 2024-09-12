library(argparse)
library(dplyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--flux-samples-alpha0-fixresp-wsif')
parser$add_argument('--flux-samples-alpha0-fixresp-wosif')
parser$add_argument('--flux-samples-alpha0-freeresp-wsif')
parser$add_argument('--flux-samples-alpha0-freeresp-wosif')
parser$add_argument('--flux-samples-alphav2-fixresp-wsif')
parser$add_argument('--flux-samples-alphav2-fixresp-wosif')
parser$add_argument('--flux-samples-alphav2-freeresp-wsif')
parser$add_argument('--flux-samples-alphav2-freeresp-wosif')
parser$add_argument('--flux-samples-alphap-fixresp-wsif')
parser$add_argument('--flux-samples-alphap-fixresp-wosif')
parser$add_argument('--flux-samples-alphap-freeresp-wsif')
parser$add_argument('--flux-samples-alphap-freeresp-wosif')
parser$add_argument('--flux-samples-alphan-fixresp-wsif')
parser$add_argument('--flux-samples-alphan-fixresp-wosif')
parser$add_argument('--flux-samples-alphan-freeresp-wsif')
parser$add_argument('--flux-samples-alphan-freeresp-wosif')
parser$add_argument('--output')
args <- parser$parse_args()

printf <- function(...) cat(sprintf(...))
collapse0 <- function(x) paste0(x, collapse = '')
paste_columns <- function(x) paste0(x, collapse = ' & ')
paste_columns_min_bold <- function(x, digits = 3) {
  x_ <- round(x, digits = digits)
  if (length(unique(x_)) == 1) {
    paste_columns(sprintf(paste0('%.', digits, 'f'), x))
  } else {
    min_x <- min(x)
    paste0(
      ifelse(x == min_x, '\\textbf{', ''),
      sprintf(paste0('%.', digits, 'f'), x),
      ifelse(x == min_x, '}', ''),
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
    args$flux_samples_alpha0_fixresp_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      truth = 'Bottom-up',
      resp_term = 'Fixed',
      estimate = if_else(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alpha0_fixresp_wosif,
  ) %>%
    mutate(
      truth = 'Bottom-up',
      resp_term = 'Fixed',
      estimate = 'Without SIF'
    ),
  read_flux_samples(
    args$flux_samples_alphav2_fixresp_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      truth = 'v2.0 mean',
      resp_term = 'Fixed',
      estimate = if_else(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alphav2_fixresp_wosif,
  ) %>%
    mutate(
      truth = 'v2.0 mean',
      resp_term = 'Fixed',
      estimate = 'Without SIF'
    ),
  read_flux_samples(
    args$flux_samples_alphap_fixresp_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      truth = 'Positive shift',
      resp_term = 'Fixed',
      estimate = if_else(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alphap_fixresp_wosif,
  ) %>%
    mutate(
      truth = 'Positive shift',
      resp_term = 'Fixed',
      estimate = 'Without SIF'
    ),
  read_flux_samples(
    args$flux_samples_alphan_fixresp_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      truth = 'Negative shift',
      resp_term = 'Fixed',
      estimate = if_else(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alphan_fixresp_wosif,
  ) %>%
    mutate(
      truth = 'Negative shift',
      resp_term = 'Fixed',
      estimate = 'Without SIF'
    ),
  read_flux_samples(
    args$flux_samples_alpha0_freeresp_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      truth = 'Bottom-up',
      resp_term = 'Free',
      estimate = if_else(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alpha0_freeresp_wosif,
  ) %>%
    mutate(
      truth = 'Bottom-up',
      resp_term = 'Free',
      estimate = 'Without SIF'
    ),
  read_flux_samples(
    args$flux_samples_alphav2_freeresp_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      truth = 'v2.0 mean',
      resp_term = 'Free',
      estimate = if_else(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alphav2_freeresp_wosif,
  ) %>%
    mutate(
      truth = 'v2.0 mean',
      resp_term = 'Free',
      estimate = 'Without SIF'
    ),
  read_flux_samples(
    args$flux_samples_alphap_freeresp_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      truth = 'Positive shift',
      resp_term = 'Free',
      estimate = if_else(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alphap_freeresp_wosif,
  ) %>%
    mutate(
      truth = 'Positive shift',
      resp_term = 'Free',
      estimate = 'Without SIF'
    ),
  read_flux_samples(
    args$flux_samples_alphan_freeresp_wsif,
    c('Truth', 'Posterior')
  ) %>%
    mutate(
      truth = 'Negative shift',
      resp_term = 'Free',
      estimate = if_else(
        estimate == 'Posterior',
        'With SIF',
        estimate
      )
    ),
  read_flux_samples(
    args$flux_samples_alphan_freeresp_wosif,
  ) %>%
    mutate(
      truth = 'Negative shift',
      resp_term = 'Free',
      estimate = 'Without SIF'
    )
) %>%
  filter(inventory != 'Total')

log_debug('Computing metrics')
osse_fluxes <- flux_aggregates_samples %>%
  filter(estimate != 'Truth') %>%
  left_join(
    flux_aggregates_samples %>%
      filter(estimate == 'Truth') %>%
      distinct(inventory, truth, region, time, flux_truth = flux_mean),
    by = c('inventory', 'truth', 'region', 'time')
  ) %>%
  mutate(
    resp_term = factor(resp_term, levels = c('Fixed', 'Free')),
    truth = factor(truth, levels = c('Bottom-up', 'v2.0 mean', 'Positive shift', 'Negative shift')),
    estimate = factor(estimate, levels = c('Without SIF', 'With SIF'))
  )

metrics <- osse_fluxes %>%
  group_by(resp_term, inventory, truth, estimate) %>%
  summarise(
    rmse = sqrt(mean((flux_mean - flux_truth)^2)),
    mcrps = mean(scoringRules::crps_sample(flux_truth, flux_samples)),
    .groups = 'drop'
  ) %>%
  arrange(resp_term, inventory, truth, estimate)

resp_terms <- levels(metrics$resp_term)
inventories <-  c('GPP', 'Respiration', 'NEE', 'Ocean')
truth_cases <- levels(metrics$truth)
estimates <- levels(metrics$estimate)

log_debug('Writing table to {args$output}')
# '\\begin{tabular}{@{}lll%s@{}}\n\\toprule\n',
sink(args$output)
printf(
  '\\begin{tabularx}{0.95\\textwidth}{lll*{%d}{R}}\n\\toprule\n',
  2 * length(estimates)
)
cat('RLT & Component & True Flux & \\multicolumn{2}{c}{RMSE [PgC/month]} & \\multicolumn{2}{c}{CRPS} \\\\\n')
cat('\\cmidrule(lr){4-5} \\cmidrule(l){6-7}\n')
printf(
  '& & & %s \\\\\n\\midrule\n',
  paste_columns(rep(paste_columns(estimates), 2))
)
for (resp_term_k in resp_terms) {
  for (inventory_i in inventories) {
    for (truth_j in truth_cases) {
      metrics_ijk <- metrics %>%
        filter(
          resp_term == resp_term_k,
          inventory == inventory_i,
          truth == truth_j
        )
      rmse_vector <- metrics_ijk %>% pull(rmse)
      mcrps_vector <- metrics_ijk %>% pull(mcrps)
      printf(
        '%s & %s & %s & %s & %s \\\\\n',
        ifelse(
          inventory_i == inventories[1] & truth_j == truth_cases[1],
          resp_term_k,
          ''
        ),
        ifelse(truth_j == truth_cases[1], inventory_i, ''),
        truth_j,
        paste_columns_min_bold(rmse_vector),
        paste_columns_min_bold(mcrps_vector)
      )
    }
  }
  if (resp_term_k != tail(resp_terms, 1)) cat('\\midrule\n')
}
cat('\\bottomrule\n\\end{tabularx}\n')
sink(NULL)

log_debug('Done')
