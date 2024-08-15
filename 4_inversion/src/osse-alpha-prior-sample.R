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
parser$add_argument('--constrain-residual', type = 'logical', default = TRUE)
parser$add_argument('--fuzz-factor', type = 'numeric', default = 1e-10)
parser$add_argument('--basis-vectors')
parser$add_argument('--samples-wombat-v2')
parser$add_argument('--output')
args <- parser$parse_args()

set.seed(20240811)

basis_vectors <- fst::read_fst(args$basis_vectors)
prior <- readRDS(args$prior)
samples <- readRDS(args$samples_wombat_v2)

n_all_alpha <- nrow(basis_vectors)
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

constraints <- readRDS(args$constraints)
if (args$constrain_residual) {
  log_trace('Constraining residual')
  F_constraint <- rbind(constraints$F_sign, constraints$F_residual)[, alpha_to_include]
  g_constraint <- c(pmax(args$fuzz_factor, constraints$g_sign), constraints$g_residual)
} else {
  log_trace('Not constraining residual')
  F_constraint <- constraints$F_sign[, alpha_to_include]
  g_constraint <- pmax(args$fuzz_factor, constraints$g_sign)
}

hist(constraints$g_sign)

all_regions_fixed <- identical(args$fix_resp_linear, sprintf('Region%02d', 1:11))
all_regions_free <- any(args$fix_resp_linear %in% c('NULL', 'null', 'None', 'none', 'NA', 'na'))
bio_regions <- sort(unique(basis_vectors$region))[1 : 11]

get_diagonal_pairs <- function(indices_list) {
  cbind(
    # NOTE(mgnb): if resp is present, the rbind cause these to alternate between assim/resp
    as.vector(do.call(rbind, indices_list)),
    as.vector(do.call(rbind, indices_list))
  )
}
get_off_diagonal_pairs <- function(indices_list) {
  rbind(
    cbind(
      indices_list$bio_assim,
      indices_list$bio_resp_tot
    ),
    cbind(
      indices_list$bio_resp_tot,
      indices_list$bio_assim
    )
  )
}

if (all_regions_fixed) {
  climatology_bio_assim_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
    bio_assim = which(inventory == 'bio_assim' & component %in% c('intercept', 'trend'))
  ))
  climatology_bio_assim_linear_diagonals <- get_diagonal_pairs(climatology_bio_assim_linear_indices)
} else if (all_regions_free) {
  climatology_bio_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
    bio_assim = which(inventory == 'bio_assim' & component %in% c('intercept', 'trend')),
    bio_resp_tot = which(inventory == 'bio_resp_tot' & component %in% c('intercept', 'trend'))
  ))
  climatology_bio_linear_diagonals <- get_diagonal_pairs(climatology_bio_linear_indices)
  climatology_bio_linear_off_diagonals <- get_off_diagonal_pairs(climatology_bio_linear_indices)
} else {
  climatology_bio_assim_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
    bio_assim = which(
      inventory == 'bio_assim' &
        component %in% c('intercept', 'trend') &
        region %in% args$fix_resp_linear
    )
  ))
  climatology_bio_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
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
  climatology_bio_assim_linear_diagonals <- get_diagonal_pairs(climatology_bio_assim_linear_indices)
  climatology_bio_linear_diagonals <- get_diagonal_pairs(climatology_bio_linear_indices)
  climatology_bio_linear_off_diagonals <- get_off_diagonal_pairs(climatology_bio_linear_indices)
}

climatology_bio_non_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
  bio_assim = which(inventory == 'bio_assim' & !(component %in% c('intercept', 'trend', 'residual'))),
  bio_resp_tot = which(inventory == 'bio_resp_tot' & !(component %in% c('intercept', 'trend', 'residual')))
))
climatology_bio_non_linear_diagonals <- get_diagonal_pairs(climatology_bio_non_linear_indices)
climatology_bio_non_linear_off_diagonals <- get_off_diagonal_pairs(climatology_bio_non_linear_indices)

residual_bio_indices_i <- lapply(bio_regions, function(region_i) {
  which(with(basis_vectors[alpha_to_include, ], {
    region == region_i & inventory != 'ocean' & component == 'residual'
  }))
})

n_times <- length(residual_bio_indices_i[[1]]) / 2

rho_bio_clim <- mean(samples$rho_bio_clim)
# w_bio_linear <- c(100, 200)
w_bio_linear <- colMeans(samples$w_bio_clim)
w_bio_season <- colMeans(samples$w_bio_clim)
kappa_bio_resid <- mean(samples$kappa_bio_resid)
rho_bio_resid <- mean(samples$rho_bio_resid)
w_bio_resid <- colMeans(samples$w_bio_resid)

get_alpha_prior_precision <- function(
  rho_bio_clim, w_bio_linear, w_bio_season,
  kappa_bio_resid, rho_bio_resid, w_bio_resid
) {
  output <- alpha_prior_precision_base

  # Bio linear process
  Q_pair_linear <- solve(rbind(
    c(1 / w_bio_linear[1], rho_bio_clim / sqrt(prod(w_bio_linear))),
    c(rho_bio_clim / sqrt(prod(w_bio_linear)), 1 / w_bio_linear[2])
  ))
  if (all_regions_fixed) {
    output[climatology_bio_assim_linear_diagonals] <- w_bio_linear[1]
  } else if (all_regions_free) {
    output[climatology_bio_linear_diagonals] <- diag(Q_pair_linear)
    output[climatology_bio_linear_off_diagonals] <- Q_pair_linear[1, 2]
  } else {
    output[climatology_bio_assim_linear_diagonals] <- w_bio_linear[1]
    output[climatology_bio_linear_diagonals] <- diag(Q_pair_linear)
    output[climatology_bio_linear_off_diagonals] <- Q_pair_linear[1, 2]
  }

  # Bio seasonal process
  Q_pair_season <- solve(rbind(
    c(1 / w_bio_season[1], rho_bio_clim / sqrt(prod(w_bio_season))),
    c(rho_bio_clim / sqrt(prod(w_bio_season)), 1 / w_bio_season[2])
  ))
  output[climatology_bio_non_linear_diagonals] <- diag(Q_pair_season)
  output[climatology_bio_non_linear_off_diagonals] <- Q_pair_season[1, 2]

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

sample_from_region <- function(region_name, Q, n_samples) {

  indices_subset <- with(
    basis_vectors[alpha_to_include, ],
    inventory %in% c('bio_assim', 'bio_resp_tot') & region == region_name
  )

  F_constraint_subset <- F_constraint[, indices_subset]
  keep_F <- rowSums(abs(F_constraint_subset)) != 0
  F_constraint_subset <- F_constraint_subset[keep_F, ]
  g_constraint_subset <- g_constraint[keep_F]
  Q_subset <- Q[indices_subset, indices_subset]

  list(
    indices = indices_subset,
    samples = sampleHmcConstrained(
      rep(0, ncol(Q_subset)),
      rep(0, ncol(Q_subset)),
      R = chol(Q_subset),
      F = F_constraint_subset,
      g = g_constraint_subset,
      totalTime = pi / 2,
      debug = FALSE,
      nSamples = n_samples
    )
  )
}

alpha_prior_precision <- get_alpha_prior_precision(
  rho_bio_clim,
  w_bio_linear,
  w_bio_season,
  kappa_bio_resid,
  rho_bio_resid,
  w_bio_resid
)
# chol_alpha_prior_precision <- chol(alpha_prior_precision)


log_debug('Simulating alpha')
# alpha_samples <- sampleHmcConstrained(
#   rep(0, n_alpha),
#   rep(0, n_alpha),
#   chol_alpha_prior_precision,
#   F_constraint,
#   g_constraint,
#   pi / 2,
#   nSamples = args$n_samples,
#   debug = TRUE,
#   bounceLimit = 100000
# )
alpha_samples <- matrix(NA, nrow = args$n_samples, ncol = sum(alpha_to_include))
for (region_name in levels(basis_vectors$region)) {
  log_trace('Sampling for {region_name}')
  region_samples <- sample_from_region(region_name, alpha_prior_precision, args$n_samples)
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
  n_samples = args$n_samples
)

log_debug('Saving to {args$output}')
saveRDS(output, args$output)

log_debug('Done')
