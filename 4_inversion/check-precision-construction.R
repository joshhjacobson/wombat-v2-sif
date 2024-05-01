library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(fastsparse)
library(WoodburyMatrix)
library(Matrix)
library(fst)

source(Sys.getenv('UTILS_PARTIAL'))

dmvnorm <- function(x, mean, covariance, precision, log = FALSE) {
  if (missing(mean)) mean <- 0

  z <- x - mean
  output <- if (!missing(covariance)) {
    as.numeric(
      -0.5 * (length(x) * log(2 * pi))
        - 0.5 * determinant(covariance, logarithm = TRUE)$modulus
        - 0.5 * crossprod(z, solve(covariance, z))
    )
  } else {
    if (is.matrix(precision)) {
      chol_precision <- chol(precision)
      as.numeric(
        -0.5 * (length(x) * log(2 * pi))
          + sum(log(diag(chol_precision)))
          - 0.5 * crossprod(chol_precision %*% z)
      )
    } else {
      as.numeric(
        -0.5 * (length(x) * log(2 * pi))
          + 0.5 * determinant(precision, logarithm = TRUE)$modulus
          - 0.5 * crossprod(z, precision %*% z)
      )
    }
  }

  if (log) output else exp(output)
}

rho_bio <- 0.7
w_bio_clim <- c(50, 10)

basis_vectors <- read_fst(args$basis_vectors)
prior <- readRDS(args$prior)

n_all_alpha <- nrow(basis_vectors)

#### RESP FIXED #####

args$fix_resp_linear <- sprintf('Region%02d', 1:11)
all_regions_fixed <- identical(args$fix_resp_linear, sprintf('Region%02d', 1:11))
all_regions_free <- args$fix_resp_linear %in% c('NULL', 'null', 'None', 'none', 'NA', 'na')
stopifnot(all_regions_fixed == !all_regions_free)

alpha_to_include <- is.finite(diag(prior$precision)) & with(
  basis_vectors,
  !(
    inventory == 'bio_resp_tot' &
      component %in% c('intercept', 'trend') &
      region %in% args$fix_resp_linear
  ) &
    component != 'residual'
)

alpha_prior_mean <- prior$mean[alpha_to_include]
alpha_prior_precision_base <- prior$precision[alpha_to_include, alpha_to_include]

bio_regions <- sort(unique(basis_vectors$region))[1:11]
bio_inventories <- c('bio_assim', 'bio_resp_tot')

n_alpha <- length(alpha_prior_mean)
n_bio_regions <- length(bio_regions)

alpha_current <- rep(0, n_alpha)

#### OLD CODE #####
climatology_bio_assim_linear_indices <- with(
  basis_vectors[alpha_to_include, ],
  which(inventory == 'bio_assim' & component %in% c('intercept', 'trend'))
)
climatology_bio_assim_linear_diagonals <- cbind(
  climatology_bio_assim_linear_indices,
  climatology_bio_assim_linear_indices
)
climatology_bio_non_linear_indices <- with(basis_vectors[alpha_to_include, ], list(
  bio_assim = which(inventory == 'bio_assim' & !(component %in% c('intercept', 'trend', 'residual'))),
  bio_resp_tot = which(inventory == 'bio_resp_tot' & !(component %in% c('intercept', 'trend', 'residual')))
))
climatology_bio_non_linear_diagonals <- cbind(
  # NOTE(mgnb): the rbind cause these to alternate between assim/resp
  as.vector(do.call(rbind, climatology_bio_non_linear_indices)),
  as.vector(do.call(rbind, climatology_bio_non_linear_indices))
)
climatology_bio_non_linear_off_diagonals <- rbind(
  cbind(
    climatology_bio_non_linear_indices$bio_assim,
    climatology_bio_non_linear_indices$bio_resp_tot
  ),
  cbind(
    climatology_bio_non_linear_indices$bio_resp_tot,
    climatology_bio_non_linear_indices$bio_assim
  )
)

get_alpha_prior_precision_old <- function(
    rho_bio_clim, w_bio_clim,
    kappa_bio_resid, rho_bio_resid, w_bio_resid) {
  output <- alpha_prior_precision_base

  # Bio climatology process
  output[climatology_bio_assim_linear_diagonals] <- w_bio_clim[1]
  Q_pair <- solve(rbind(
    c(1 / w_bio_clim[1], rho_bio_clim / sqrt(prod(w_bio_clim))),
    c(rho_bio_clim / sqrt(prod(w_bio_clim)), 1 / w_bio_clim[2])
  ))
  output[climatology_bio_non_linear_diagonals] <- diag(Q_pair)
  output[climatology_bio_non_linear_off_diagonals] <- Q_pair[1, 2]

  output
}

log_pdf_climatology_bio_old <- function(rho, w) {
  if (rho < -1 || rho > 1) {
    return(-Inf)
  }
  if (any(w < 0)) {
    return(-Inf)
  }
  Q_pair <- solve(rbind(
    c(1 / w[1], rho / sqrt(prod(w))),
    c(rho / sqrt(prod(w)), 1 / w[2])
  ))
  indices <- as.vector(do.call(rbind, climatology_bio_non_linear_indices))
  Q <- kronecker(
    diag(length(indices) / 2),
    Q_pair
  )
  sum(dnorm(
    alpha_current[climatology_bio_assim_linear_indices],
    alpha_prior_mean[climatology_bio_assim_linear_indices],
    sd = 1 / sqrt(w[1]),
    log = TRUE
  )) + dmvnorm(
    alpha_current[indices],
    alpha_prior_mean[indices],
    precision = Q,
    log = TRUE
  )
  # + sum(dgamma(
  #   w,
  #   shape = W_PRIOR$shape,
  #   rate = W_PRIOR$rate,
  #   log = TRUE
  # ))
}


Q_old <- get_alpha_prior_precision_old(
  rho_bio_clim = rho_bio, w_bio_clim = w_bio_clim,
  kappa_bio_resid = 0, rho_bio_resid = 0, w_bio_resid = c(1, 1)
)
image(Q_old)

pdf_manual_old <- log_pdf_climatology_bio_old(rho_bio, w_bio_clim)
pdf_auto_old <- dmvnorm(alpha_current, alpha_prior_mean, precision = Q_old, log = TRUE)
# 234.8207


#### NEW CODE #####

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

get_alpha_prior_precision <- function(
    rho_bio_clim, w_bio_clim,
    kappa_bio_resid, rho_bio_resid, w_bio_resid) {
  output <- alpha_prior_precision_base

  # Bio climatology process
  Q_pair <- solve(rbind(
    c(1 / w_bio_clim[1], rho_bio_clim / sqrt(prod(w_bio_clim))),
    c(rho_bio_clim / sqrt(prod(w_bio_clim)), 1 / w_bio_clim[2])
  ))
  if (all_regions_fixed) {
    output[climatology_bio_assim_linear_diagonals] <- w_bio_clim[1]
  } else if (all_regions_free){
    output[climatology_bio_linear_diagonals] <- diag(Q_pair)
    output[climatology_bio_linear_off_diagonals] <- Q_pair[1, 2]
  } else {
    output[climatology_bio_assim_linear_diagonals] <- w_bio_clim[1]
    output[climatology_bio_linear_diagonals] <- diag(Q_pair)
    output[climatology_bio_linear_off_diagonals] <- Q_pair[1, 2]
  }
  output[climatology_bio_non_linear_diagonals] <- diag(Q_pair)
  output[climatology_bio_non_linear_off_diagonals] <- Q_pair[1, 2]

  output
}

log_pdf_climatology_bio <- function(rho, w) {
  if (rho < -1 || rho > 1) return(-Inf)
  if (any(w < 0)) return(-Inf)
  Q_pair <- solve(rbind(
    c(1 / w[1], rho / sqrt(prod(w))),
    c(rho / sqrt(prod(w)), 1 / w[2])
  ))
  
  if (all_regions_fixed) {
    log_pdf_linear <- sum(dnorm(
      alpha_current[climatology_bio_assim_linear_indices$bio_assim],
      alpha_prior_mean[climatology_bio_assim_linear_indices$bio_assim],
      sd = 1 / sqrt(w[1]),
      log = TRUE
    ))
  } else {
    linear_indices <- as.vector(do.call(rbind, climatology_bio_linear_indices))
    Q_linear <- kronecker(
      diag(length(linear_indices) / 2),
      Q_pair
    )
    log_pdf_linear <- dmvnorm(
      alpha_current[linear_indices],
      alpha_prior_mean[linear_indices],
      precision = Q_linear,
      log = TRUE
    )
  }
  if (!all_regions_fixed & !all_regions_free) {
    log_pdf_linear <- log_pdf_linear + sum(dnorm(
      alpha_current[climatology_bio_assim_linear_indices$bio_assim],
      alpha_prior_mean[climatology_bio_assim_linear_indices$bio_assim],
      sd = 1 / sqrt(w[1]),
      log = TRUE
    ))
  }

  non_linear_indices <- as.vector(do.call(rbind, climatology_bio_non_linear_indices))
  Q_non_linear <- kronecker(
    diag(length(non_linear_indices) / 2),
    Q_pair
  )
  log_pdf_linear + dmvnorm(
    alpha_current[non_linear_indices],
    alpha_prior_mean[non_linear_indices],
    precision = Q_non_linear,
    log = TRUE
  )
}

Q <- get_alpha_prior_precision(
  rho_bio_clim = rho_bio, w_bio_clim = w_bio_clim,
  kappa_bio_resid = 0, rho_bio_resid = 0, w_bio_resid = c(1, 1)
)
image(Q)

pdf_manual <- log_pdf_climatology_bio(rho_bio, w_bio_clim)
pdf_auto <- dmvnorm(alpha_current, alpha_prior_mean, precision = Q, log = TRUE)
# 234.8207


#### RESP FREE #####
args$fix_resp_linear <- NULL
all_regions_fixed <- identical(args$fix_resp_linear, sprintf('Region%02d', 1:11))
all_regions_free <- is.null(args$fix_resp_linear)

alpha_to_include <- is.finite(diag(prior$precision)) & with(
  basis_vectors,
  !(
    inventory == 'bio_resp_tot' &
      component %in% c('intercept', 'trend') &
      region %in% args$fix_resp_linear
  ) &
    component != 'residual'
)


alpha_prior_mean <- prior$mean[alpha_to_include]
alpha_prior_precision_base <- prior$precision[alpha_to_include, alpha_to_include]

n_alpha <- length(alpha_prior_mean)
alpha_current <- rep(0, n_alpha)

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

Q <- get_alpha_prior_precision(
  rho_bio_clim = rho_bio, w_bio_clim = w_bio_clim,
  kappa_bio_resid = 0, rho_bio_resid = 0, w_bio_resid = c(1, 1)
)
image(Q)

pdf_manual <- log_pdf_climatology_bio(rho_bio, w_bio_clim)
pdf_auto <- dmvnorm(alpha_current, alpha_prior_mean, precision = Q, log = TRUE)
# Both 247.3393 when no region fixed
# Both 246.2012 when only Region03 fixed
