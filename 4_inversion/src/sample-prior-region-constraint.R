library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(Rcpp)

rcpp_cache_dir <- Sys.getenv('RCPP_CACHE_DIR')
options(rcpp.cache.dir = if (rcpp_cache_dir == '') tempdir() else rcpp_cache_dir)

source(Sys.getenv('UTILS_PARTIAL'))
sourceCpp(Sys.getenv('HMC_EXACT_CPP_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--n-samples', type = 'integer')
parser$add_argument('--fix-resp-linear', nargs = '+', default = sprintf('Region%02d', 1:11))
parser$add_argument('--prior')
parser$add_argument('--constraints')
parser$add_argument('--basis-vectors')
parser$add_argument('--perturbations')
parser$add_argument('--output')
args <- parser$parse_args()

set.seed(20240820)

constraints <- readRDS(args$constraints)
basis_vectors <- fst::read_fst(args$basis_vectors)
prior <- readRDS(args$prior)
perturbations <- fst::read_fst(args$perturbations)

# NOTE(mgnb): values of alpha fixed to zero are given infinite precision
alpha_to_include <- is.finite(diag(prior$precision)) & with(
  basis_vectors,
  !(
    inventory == 'bio_resp_tot' &
      component %in% c('intercept', 'trend') &
      region %in% args$fix_resp_linear
  )
)

alpha_prior_precision_base <- prior$precision[alpha_to_include, alpha_to_include]
n_alpha <- sum(alpha_to_include)

bio_regions <- sort(unique(basis_vectors$region))[1 : 11]
resp_bio_all_fixed <- identical(args$fix_resp_linear, as.character(bio_regions))
resp_bio_all_free <- any(tolower(args$fix_resp_linear) %in% c('null', 'none', 'na'))

F_constraint <- rbind(constraints$F_sign, constraints$F_residual)[, alpha_to_include]
g_constraint <- c(constraints$g_sign, constraints$g_residual)

get_diagonal_pairs <- function(indices_list) {
  cbind(
    # NOTE(mgnb): if resp is present, the rbind cause these to alternate between assim/resp
    as.vector(do.call(rbind, indices_list)),
    as.vector(do.call(rbind, indices_list))
  )
}
get_off_diagonal_pairs <- function(indices_list) {
  rbind(
    cbind(indices_list$bio_assim, indices_list$bio_resp_tot),
    cbind(indices_list$bio_resp_tot, indices_list$bio_assim)
  )
}

if (resp_bio_all_fixed) {
  clim_bio_assim_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
    bio_assim = which(inventory == 'bio_assim' & component %in% c('intercept', 'trend'))
  ))
  clim_bio_assim_linear_diagonals <- get_diagonal_pairs(clim_bio_assim_linear_indices)
} else if (resp_bio_all_free) {
  clim_bio_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
    bio_assim = which(inventory == 'bio_assim' & component %in% c('intercept', 'trend')),
    bio_resp_tot = which(inventory == 'bio_resp_tot' & component %in% c('intercept', 'trend'))
  ))
  clim_bio_linear_diagonals <- get_diagonal_pairs(clim_bio_linear_indices)
  clim_bio_linear_off_diagonals <- get_off_diagonal_pairs(clim_bio_linear_indices)
} else {
  clim_bio_assim_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
    bio_assim = which(
      inventory == 'bio_assim' &
        component %in% c('intercept', 'trend') &
        region %in% args$fix_resp_linear
    )
  ))
  clim_bio_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
    bio_assim = which(
      inventory == 'bio_assim' &
        component %in% c('intercept', 'trend') &
        !(region %in% args$fix_resp_linear)
    ),
    bio_resp_tot = which(
      inventory == 'bio_resp_tot' &
        component %in% c('intercept', 'trend') &
        !(region %in% args$fix_resp_linear)
    )
  ))
  clim_bio_assim_linear_diagonals <- get_diagonal_pairs(clim_bio_assim_linear_indices)
  clim_bio_linear_diagonals <- get_diagonal_pairs(clim_bio_linear_indices)
  clim_bio_linear_off_diagonals <- get_off_diagonal_pairs(clim_bio_linear_indices)
}

clim_bio_non_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
  bio_assim = which(inventory == 'bio_assim' & !(component %in% c('intercept', 'trend', 'residual'))),
  bio_resp_tot = which(inventory == 'bio_resp_tot' & !(component %in% c('intercept', 'trend', 'residual')))
))
clim_bio_non_linear_diagonals <- get_diagonal_pairs(clim_bio_non_linear_indices)
clim_bio_non_linear_off_diagonals <- get_off_diagonal_pairs(clim_bio_non_linear_indices)

residual_bio_indices_i <- lapply(bio_regions, function(region_i) {
  which(with(basis_vectors[alpha_to_include, ], {
    region == region_i & inventory != 'ocean' & component == 'residual'
  }))
})

n_times <- length(residual_bio_indices_i[[1]]) / 2

get_alpha_prior_precision <- function(
  rho_bio_clim, w_bio_clim,
  kappa_bio_resid, rho_bio_resid, w_bio_resid
) {
  output <- alpha_prior_precision_base

  # Bio climatology process
  Q_pair <- solve(rbind(
    c(1 / w_bio_clim[1], rho_bio_clim / sqrt(prod(w_bio_clim))),
    c(rho_bio_clim / sqrt(prod(w_bio_clim)), 1 / w_bio_clim[2])
  ))
  if (resp_bio_all_fixed) {
    output[clim_bio_assim_linear_diagonals] <- w_bio_clim[1]
  } else if (resp_bio_all_free) {
    output[clim_bio_linear_diagonals] <- diag(Q_pair)
    output[clim_bio_linear_off_diagonals] <- Q_pair[1, 2]
  } else {
    output[clim_bio_assim_linear_diagonals] <- w_bio_clim[1]
    output[clim_bio_linear_diagonals] <- diag(Q_pair)
    output[clim_bio_linear_off_diagonals] <- Q_pair[1, 2]
  }
  output[clim_bio_non_linear_diagonals] <- diag(Q_pair)
  output[clim_bio_non_linear_off_diagonals] <- Q_pair[1, 2]

  # Residual process
  Q_ar <- ar1_Q(n_times, kappa_bio_resid, sparse = FALSE)
  Q_cross <- solve(rbind(
    c(1 / w_bio_resid[1], rho_bio_resid / sqrt(prod(w_bio_resid))),
    c(rho_bio_resid / sqrt(prod(w_bio_resid)), 1 / w_bio_resid[2])
  ))
  Q <- kronecker(Q_ar, Q_cross)
  for (i in seq_along(bio_regions)) {
    indices <- residual_bio_indices_i[[i]]
    output[indices, indices] <- Q
  }

  output
}

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
  bio_intercept_mean,
  ignore_ocean = FALSE
) {
  indices_subset <- with(
    basis_vectors[alpha_to_include, ],
    if (ignore_ocean) {
      region == region_name & inventory != 'ocean'
    } else {
      region == region_name
    }
  )
  F_constraint_subset <- F_constraint[, indices_subset]
  keep_F <- rowSums(abs(F_constraint_subset)) != 0
  F_constraint_subset <- F_constraint_subset[keep_F, ]
  g_constraint_subset <- g_constraint[keep_F]
  Q_subset <- Q[indices_subset, indices_subset]

  bio_inventories <- c('bio_assim', 'bio_resp_tot')
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
  c(1, 1)  # w resid
)
n_samples <- args$n_samples

get_intercept_mean <- function(region_name) {
  log_trace('Getting intercept for {region_name}')
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
  # Get the global total flux for the middle month within 0.1 PgC of zero
  epsilon <- 1e-1
  get_target <- function(proposed_mean) {
    region_samples <- sample_from_region(
      region_name,
      Q,
      n_samples,
      bio_intercept_mean = proposed_mean,
      ignore_ocean = TRUE
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
    NA
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

alpha_samples <- matrix(0, nrow = n_samples, ncol = sum(alpha_to_include))
for (region_name in levels(basis_vectors$region)) {
  log_trace('Sampling alpha for {region_name}')
  bio_intercept <- if (region_name %in% colnames(intercept_regional)) {
    intercept_regional[, colnames(intercept_regional) == region_name]
  } else {
    c(0, 0)
  }
  if (region_name %in% args$fix_resp_linear) {
    bio_intercept <- bio_intercept[1]
  }
  region_samples <- sample_from_region(
    region_name,
    Q,
    n_samples,
    bio_intercept
  )
  alpha_samples[, region_samples$indices] <- region_samples$samples
}

alpha_df <- cbind(
  basis_vectors[alpha_to_include, ],
  data.frame(value = colMeans(alpha_samples))
)
alpha_df$value_samples <- t(alpha_samples)

output <- list(
  alpha = alpha_samples,
  alpha_df = alpha_df,
  intercepts = intercept_regional,
  n_samples = args$n_samples
)

log_trace('Saving output')
saveRDS(output, args$output)

log_trace('Done')
