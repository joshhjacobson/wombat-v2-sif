library(argparse)
library(Matrix)
library(dplyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--case')
parser$add_argument('--samples')
parser$add_argument('--flux-aggregators')
parser$add_argument('--wombat-v2-alpha')
parser$add_argument('--output')
args <- parser$parse_args()

base_case <- strsplit(args$case, '-', fixed = TRUE)[[1]][1]
stopifnot(base_case %in% c('ALPHA0', 'ALPHAV2'))

samples <- readRDS(args$samples)
flux_aggregators <- fst::read_fst(args$flux_aggregators)
wombat_v2_alpha <- fst::read_fst(args$wombat_v2_alpha)


X_aggregators <- with(flux_aggregators, sparseMatrix(
  i = as.integer(inventory_region_time),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(inventory_region_time), nlevels(basis_vector))
))

prior_emissions <- flux_aggregators %>%
  group_by(inventory_region_time, inventory, region, time) %>%
  summarise(flux_mean = sum(value), .groups = 'drop') %>%
  select(-inventory_region_time) %>%
  mutate(estimate = 'Bottom-up')

true_emissions <- prior_emissions %>%
  mutate(estimate = 'Truth')

if (base_case == 'ALPHAV2') {
  true_emissions <- true_emissions %>%
    mutate(
      flux_mean = flux_mean + as.vector(
        X_aggregators[, as.integer(wombat_v2_alpha$basis_vector)]
        %*% wombat_v2_alpha$value
      )
    )
}

posterior_emissions <- prior_emissions %>%
  mutate(
    estimate = 'Posterior',
    flux_prior = flux_mean,
    flux_mean = flux_prior + as.vector(
      X_aggregators[, as.integer(samples$alpha_df$basis_vector)]
      %*% samples$alpha_df$value
    ),
    flux_samples = flux_prior + as.matrix(
      X_aggregators[, as.integer(samples$alpha_df$basis_vector)]
      %*% samples$alpha_df$value_samples
    )
  ) %>%
  select(-flux_prior)

flux_aggregates_samples <- bind_rows(
  true_emissions,
  prior_emissions,
  posterior_emissions
) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        filter(inventory %in% c('bio_assim', 'bio_resp_tot')) %>%
        group_by(estimate, region, time) %>%
        summarise(
          flux_mean = sum(flux_mean),
          flux_samples = t(colSums(flux_samples)),
          .groups = 'drop'
        ) %>%
        mutate(inventory = 'nee'),
      x %>%
        group_by(estimate, region, time) %>%
        summarise(
          flux_mean = sum(flux_mean),
          flux_samples = t(colSums(flux_samples)),
          .groups = 'drop'
        ) %>%
        mutate(inventory = 'total')
    )
  } %>%
  mutate(
    inventory = factor(c(
      'bio_assim' = 'GPP',
      'bio_resp_tot' = 'Respiration',
      'nee' = 'NEE',
      'ocean' = 'Ocean',
      'total' = 'Total'
    )[inventory], levels = c(
      'GPP',
      'Respiration',
      'NEE',
      'Ocean',
      'Total'
    ))
  )

saveRDS(flux_aggregates_samples, args$output)
