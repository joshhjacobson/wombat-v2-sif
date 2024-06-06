library(argparse)
library(Matrix)
library(dplyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--true-alpha')
parser$add_argument('--samples')
parser$add_argument('--perturbations-augmented-zonal')
parser$add_argument('--output')
args <- parser$parse_known_args()[[1]]

perturbations_augmented_zonal <- fst::read_fst(args$perturbations_augmented_zonal)
samples <- readRDS(args$samples)
true_alpha <- if (!is.null(args$true_alpha)) fst::read_fst(args$true_alpha) else NULL

perturbations_augmented_zonal <- perturbations_augmented_zonal %>%
  mutate(
    inventory_time = interaction(
      inventory,
      time,
      drop = TRUE
    )
  )

flux_aggregates_samples_zonal <- lapply(REGION_PLOT_SETTINGS, function(zonal_band) {
  perturbations_zone <- perturbations_augmented_zonal %>%
    filter(
      latitude_bottom >= zonal_band$latitude_lower,
      latitude_bottom < zonal_band$latitude_upper
    ) %>%
    group_by(inventory_time, basis_vector) %>%
    summarise(
      value = KG_M2_S_TO_PGC_MONTH * sum(area * value),
      .groups = 'drop'
    ) %>%
    left_join(
      perturbations_augmented_zonal %>%
        distinct(inventory_time, inventory, time),
      by = 'inventory_time'
    )

  X_aggregators_zone <- with(perturbations_zone, sparseMatrix(
    i = as.integer(inventory_time),
    j = as.integer(basis_vector),
    x = value,
    dims = c(nlevels(inventory_time), nlevels(basis_vector))
  ))

  prior_emissions <- perturbations_zone %>%
    group_by(inventory_time, inventory, time) %>%
    summarise(flux_mean = sum(value), .groups = 'drop') %>%
    select(-inventory_time) %>%
    mutate(estimate = 'Bottom-up')

  true_emissions <- prior_emissions %>%
    mutate(estimate = 'Truth')

  if (!is.null(true_alpha)) {
    true_emissions <- true_emissions %>%
      mutate(
        flux_mean = flux_mean + as.vector(
          X_aggregators_zone[, as.integer(true_alpha$basis_vector)]
          %*% true_alpha$value
        )
      )
  }

  posterior_emissions <- prior_emissions %>%
    mutate(
      estimate = 'Posterior',
      flux_prior = flux_mean,
      flux_mean = flux_prior + as.vector(
        X_aggregators_zone[, as.integer(samples$alpha_df$basis_vector)]
        %*% samples$alpha_df$value
      ),
      flux_samples = flux_prior + as.matrix(
        X_aggregators_zone[, as.integer(samples$alpha_df$basis_vector)]
        %*% samples$alpha_df$value_samples
      )
    ) %>%
    select(-flux_prior)

  emissions_zone <- bind_rows(
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
          group_by(estimate, time) %>%
          summarise(
            flux_mean = sum(flux_mean),
            flux_samples = t(colSums(flux_samples)),
            .groups = 'drop'
          ) %>%
          mutate(inventory = 'nee')
      )
    } %>%
    mutate(zone = zonal_band$numeric_title)
}) %>%
  bind_rows() %>%
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
    zone = factor(
      zone,
      levels = sapply(REGION_PLOT_SETTINGS, function(zonal_band) zonal_band$numeric_title)
    )
  )

saveRDS(flux_aggregates_samples_zonal, args$output)