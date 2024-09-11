library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(Rcpp)

rcpp_cache_dir <- Sys.getenv('RCPP_CACHE_DIR')
options(rcpp.cache.dir = if (rcpp_cache_dir == '') tempdir() else rcpp_cache_dir)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('ALPHA_PRECISION_PARTIAL'))
sourceCpp(Sys.getenv('UTILS_CPP_PARTIAL'))
sourceCpp(Sys.getenv('HMC_EXACT_CPP_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--fix-resp-linear', nargs = '+', default = sprintf('Region%02d', 1:11))
parser$add_argument('--prior-base')
parser$add_argument('--constraints')
parser$add_argument('--basis-vectors')
parser$add_argument('--control-emissions')
parser$add_argument('--perturbations')
parser$add_argument('--output')
args <- parser$parse_args()

set.seed(20240820)

prior_base <- readRDS(args$prior_base)
constraints <- readRDS(args$constraints)
basis_vectors <- fst::read_fst(args$basis_vectors)
control_emissions <- fst::read_fst(args$control_emissions)
perturbations_base <- fst::read_fst(args$perturbations)

cell_area <- control_emissions %>%
  distinct(longitude, latitude, cell_height, area) %>%
  mutate(
    latitude_bottom = latitude - cell_height / 2
  ) %>%
  select(-cell_height)

perturbations <- perturbations_base %>%
  add_basis_vector(basis_vectors) %>%
  mutate(
    minor_component = factor(
      case_when(
        component == 'residual' ~ 'residual',
        component %in% c('intercept', 'trend') ~ 'linear',
        TRUE ~ 'periodic'
      ),
      c('linear', 'periodic', 'residual')
    ),
    inventory_minor_component_time = interaction(
      inventory,
      minor_component,
      time,
      drop = TRUE
    )
  ) %>%
  left_join(cell_area, by = c('longitude', 'latitude'))

# NOTE(mgnb): values of alpha fixed to zero are given infinite precision
alpha_to_include <- is.finite(diag(prior_base$precision)) & with(
  basis_vectors,
  !(
    inventory == 'bio_resp_tot' &
      component %in% c('intercept', 'trend') &
      region %in% args$fix_resp_linear
  )
)

alpha_prior_mean <- prior_base$mean[alpha_to_include]
alpha_prior_precision_base <- prior_base$precision[alpha_to_include, alpha_to_include]
n_alpha <- nrow(alpha_prior_precision_base)

bio_regions <- sort(unique(basis_vectors$region))[1 : 11]
resp_bio_all_fixed <- identical(args$fix_resp_linear, as.character(bio_regions))
resp_bio_all_free <- any(tolower(args$fix_resp_linear) %in% c('null', 'none', 'na'))

bio_indices <- get_bio_indices(basis_vectors[alpha_to_include, ])

F_constraint <- rbind(
  constraints$F_sign, 
  constraints$F_residual
)[, alpha_to_include]
g_constraint <- c(constraints$g_sign, constraints$g_residual)

get_emissions_parts <- function(region_name) {
  perturbations_region <- if (!missing(region_name)) {
    perturbations %>% filter(region == region_name)
  } else {
    perturbations
  }
  perturbations_region <- perturbations_region %>%
    group_by(inventory_minor_component_time, basis_vector) %>%
    summarise(
      value = KG_M2_S_TO_PGC_MONTH * sum(area * value),
      .groups = 'drop'
    ) %>%
    left_join(
      perturbations %>%
        distinct(inventory_minor_component_time, inventory, minor_component, time),
      by = 'inventory_minor_component_time'
    )

  X_region <- with(perturbations_region, sparseMatrix(
    i = as.integer(inventory_minor_component_time),
    j = as.integer(basis_vector),
    x = value,
    dims = c(nlevels(inventory_minor_component_time), nlevels(basis_vector))
  ))[, alpha_to_include]

  prior_emissions <- perturbations_region %>%
    group_by(inventory_minor_component_time, inventory, minor_component, time) %>%
    summarise(value = sum(value), .groups = 'drop') %>%
    select(-inventory_minor_component_time) %>%
    mutate(output = 'Bottom-up')

  list(
    X_region = X_region,
    prior_emissions = prior_emissions
  )
}

sample_from_region <- function(
  region_name,
  Q,
  n_samples,
  bio_intercept_mean
) {
  bio_inventories <- c('bio_assim', 'bio_resp_tot')
  indices_subset <- with(
    basis_vectors[alpha_to_include, ],
    region == region_name & inventory %in% bio_inventories
  )
  F_constraint_subset <- F_constraint[, indices_subset]
  keep_F <- rowSums(abs(F_constraint_subset)) != 0
  F_constraint_subset <- F_constraint_subset[keep_F, ]
  g_constraint_subset <- g_constraint[keep_F]
  Q_subset <- Q[indices_subset, indices_subset]

  if (region_name %in% args$fix_resp_linear) {
    bio_inventories <- bio_inventories[1]
    bio_intercept_mean <- as.vector(bio_intercept_mean)[1]
  }

  bio_intercept_indices <- which(
    (basis_vectors[alpha_to_include, ][indices_subset, ]$inventory %in% bio_inventories)
    & (basis_vectors[alpha_to_include, ][indices_subset, ]$component == 'intercept')
  )
  prior_mean <- rep(0, ncol(Q_subset))
  prior_mean[bio_intercept_indices] <- bio_intercept_mean

  list(
    indices = indices_subset,
    samples = sampleHmcConstrained(
      rep(0, ncol(Q_subset)),
      prior_mean,
      R = chol(Q_subset),
      F = F_constraint_subset,
      g = g_constraint_subset,
      totalTime = pi / 2,
      debug = FALSE,
      nSamples = n_samples
    )
  )
}

Q <- get_alpha_prior_precision(
  0,  # rho clim
  c(1, 1),  # w clim
  0,  # kappa resid
  0,  # rho resid
  c(1, 1),  # w resid
  bio_indices,
  alpha_prior_precision_base
)
n_samples <- 10000

get_intercept_mean <- function(region_name) {
  log_trace('Computing offsets for bio intercepts in {region_name}')
  parts <- get_emissions_parts(region_name)
  middle_bio_assim <- which(
    parts$prior_emissions$inventory == 'bio_assim'
    & parts$prior_emissions$minor_component == 'linear'
    & parts$prior_emissions$time == median(parts$prior_emissions$time)
  )
  middle_bio_resp_tot <- which(
    parts$prior_emissions$inventory == 'bio_resp_tot'
    & parts$prior_emissions$minor_component == 'linear'
    & parts$prior_emissions$time == median(parts$prior_emissions$time)
  )
  # Get the region total flux for the middle month within 0.01 PgC of zero
  epsilon <- 1e-2
  get_target <- function(proposed_mean) {
    region_samples <- sample_from_region(
      region_name,
      Q,
      n_samples,
      bio_intercept_mean = proposed_mean
    )
    fluxes <- t(
      parts$X_region[c(middle_bio_assim, middle_bio_resp_tot), region_samples$indices]
      %*% t(region_samples$samples)
    )

    colMeans(fluxes)
  }

  bio_assim_root <- uniroot(function(x) {
    get_target(c(x, 0))[1]
  }, c(-12, 12), tol = epsilon, trace = 0)$root
  bio_resp_tot_root <- if (region_name %in% args$fix_resp_linear) {
    0
  } else {
    uniroot(function(x) {
      get_target(c(0, x))[2]
    }, c(-10, 10), tol = epsilon, trace = 0)$root
  }
  c(
    bio_assim_root,
    bio_resp_tot_root
  )
}

intercept_regional <- sapply(as.character(bio_regions), get_intercept_mean)
rownames(intercept_regional) <- c('bio_assim', 'bio_resp_tot')

get_region_intercept_indices <- function(region_name) {
  bio_inventories_region <- if (region_name %in% args$fix_resp_linear) {
    'bio_assim'
  } else {
    c('bio_assim', 'bio_resp_tot')
  }
  with(
    basis_vectors[alpha_to_include, ],
    which(
      region == region_name
      & inventory %in% bio_inventories_region
      & component == 'intercept'
    )
  )
}

log_debug('Updating prior mean for bio intercepts')
for (region_i in seq_len(ncol(intercept_regional))) {
  region_name <- colnames(intercept_regional)[region_i]
  bio_intercept_indices_i <- get_region_intercept_indices(region_name)

  bio_intercept_i <- intercept_regional[, region_i]
  if (region_name %in% args$fix_resp_linear) {
    bio_intercept_i <- bio_intercept_i[1]
  }

  stopifnot(length(bio_intercept_indices_i) == length(bio_intercept_i))
  alpha_prior_mean[bio_intercept_indices_i] <- bio_intercept_i
}

alpha_prior_mean_final <- prior_base$mean
alpha_prior_mean_final[alpha_to_include] <- alpha_prior_mean

saveRDS(list(
  mean = alpha_prior_mean_final,
  precision = prior_base$precision
), args$output)
