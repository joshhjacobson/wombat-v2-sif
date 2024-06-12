library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented')
parser$add_argument('--samples-LNLGIS')
parser$add_argument('--samples-LNLGISSIF')
parser$add_argument('--output')
args <- parser$parse_args()

compute_alpha_posterior <- function(samples, perturbations) {
  X_alpha_to_six_year_mean <- with(perturbations, sparseMatrix(
    i = as.integer(inventory_location),
    j = as.integer(basis_vector),
    x = value,
    dims = c(nlevels(inventory_location), nlevels(basis_vector))
  ))[, as.integer(samples$alpha_df$basis_vector)]

  alpha_mean <- samples$alpha_df$value
  alpha_samples <- samples$alpha_df$value_samples[
    ,
    floor(seq(1, ncol(samples$alpha_df$value_samples), length.out = 100))
  ]
  list(
    value_tilde_mean = as.vector(X_alpha_to_six_year_mean %*% alpha_mean),
    value_tilde_samples = as.matrix(X_alpha_to_six_year_mean %*% alpha_samples)
  )
}

perturbations_augmented <- fst::read_fst(args$perturbations_augmented)
samples_LNLGIS <- readRDS(args$samples_LNLGIS)
samples_LNLGISSIF <- readRDS(args$samples_LNLGISSIF)

perturbations_augmented <- perturbations_augmented %>%
  filter(
    inventory %in% c('bio_assim', 'bio_resp_tot')
  ) %>%
  mutate(
    inventory_location = interaction(
      inventory,
      longitude,
      latitude,
      drop = TRUE
    )
  )

perturbations <- perturbations_augmented %>%
  group_by(inventory_location, basis_vector) %>%
  summarise(
    value = PER_SECONDS_TO_PER_YEAR * sum(value) / 72,
    .groups = 'drop'
  ) %>%
  left_join(
    perturbations_augmented %>%
      distinct(inventory_location, inventory, longitude, latitude),
    by = 'inventory_location'
  )

six_year_average_bottom_up <- perturbations %>%
  group_by(inventory_location, inventory, longitude, latitude) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  select(-inventory_location)

alpha_LNLGIS <- compute_alpha_posterior(samples_LNLGIS, perturbations)
alpha_LNLGISSIF <- compute_alpha_posterior(samples_LNLGISSIF, perturbations)
alpha_difference <- list(
  value_tilde_mean = alpha_LNLGISSIF$value_tilde_mean - alpha_LNLGIS$value_tilde_mean,
  value_tilde_samples = alpha_LNLGISSIF$value_tilde_samples - alpha_LNLGIS$value_tilde_samples
)

output <- bind_rows(
  six_year_average_bottom_up %>%
    mutate(estimate = 'Bottom-up'),
  six_year_average_bottom_up %>%
    mutate(
      estimate = 'LNLGIS',
      value = value + alpha_LNLGIS$value_tilde_mean,
      value_samples = value + alpha_LNLGIS$value_tilde_samples,
      value_sd = matrixStats::rowSds(value_samples)
    ),
  six_year_average_bottom_up %>%
    mutate(
      estimate = 'LNLGISSIF',
      value = value + alpha_LNLGISSIF$value_tilde_mean,
      value_samples = value + alpha_LNLGISSIF$value_tilde_samples,
      value_sd = matrixStats::rowSds(value_samples)
    ),
  six_year_average_bottom_up %>%
    mutate(
      estimate = 'Difference',
      value = alpha_difference$value_tilde_mean,
      value_samples = alpha_difference$value_tilde_samples,
      value_sd = matrixStats::rowSds(value_samples)
    )
) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        group_by(estimate, longitude, latitude) %>%
        summarise(
          value = sum(value),
          value_samples = t(colSums(value_samples)),
          .groups = 'drop'
        ) %>%
        mutate(
          inventory = 'nee',
          value_sd = matrixStats::rowSds(value_samples)
        )
    )
  } %>%
  select(-value_samples) %>%
  rename(value_mean = value)

fst::write_fst(output, args$output)
