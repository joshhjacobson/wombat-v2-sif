library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(fastsparse)
library(lubridate, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(WoodburyMatrix)

# source(Sys.getenv('UTILS_PARTIAL'))
source('partials/utils.R')

args <- list()
args$observations <- '4_inversion/intermediates/observations.fst'
args$overall_observation_mode <- c('LN', 'LG', 'IS', 'LN_SIF', 'LG_SIF')
args$case <- 'zero'

SEED <- 20240403 + as.integer(
  factor(
    c('zero' = 'zero', 'wombatv2' = 'wombatv2')[args$case],
    levels = c('zero', 'wombatv2')
  )
)
set.seed(SEED)

simulate_epsilon <- function(
  gamma,
  rho,
  ell,
  time_since,
  error
) {
  epsilon <- list()
  for (j in seq_along(rho)) {
    for (i in seq_along(error[[j]])) {
      diff_time_since <- diff(time_since[[j]][[i]])
      # HACK(mgnb): some IS sites have repeated times; separate them minimally
      diff_time_since[diff_time_since == 0] <- 1 / 24
      stopifnot(all(diff_time_since != 0))
      precisions <- 1 / error[[j]][[i]] ^ 2
      cross_precisions <- head(sqrt(precisions), -1) * tail(sqrt(precisions), -1)

      A <- FastDiagonal(
        x = (gamma / (1 - rho[j])) * precisions
      )
      B <- ou_precision(
        diff_time_since,
        1 / ell[j],
        precisions * gamma / rho[j],
        cross_precisions * gamma / rho[j]
      )
      O <- TridiagonalMatrix(
        A@x + B@major,
        B@minor
      )
      Sigma_epsilon <- WoodburyMatrix(
        A,
        B,
        O = O,
        symmetric = TRUE
      )

      epsilon <- append(epsilon, rwnorm(n = 1, covariance = Sigma_epsilon))
    }
  }
  unlist(epsilon)
}

produce_group_epsilons <- function(group) {
  # NOTE(mgnb): each hyperparameter_group has its own rho, ell, and is nested
  # within a fit_group, which has its own gamma
  fit_group <- group$fit_group[[1]]
  log_trace('Simulating epsilons for {fit_group} group')
  is_oco2 <- fit_group %in% c('1_LNLG', '2_OG', '3_SIF')

  unit <- if (is_oco2) 'mins' else 'days'

  # NOTE(mgnb): split first by hyperparameter group, then obs group
  group_parts <- group %>%
    mutate(
      time_since = as.double(time - min(time), unit = unit),
    ) %>%
    group_by(hyperparameter_group) %>%
    group_map(~ {
      group %>%
        group_by(observation_group) %>%
        group_map(~ group)
    })

  n_groups <- length(group_parts)
  residual_parts <- lapply(group_parts, lapply, getElement, 'residual')
  time_since_parts <- lapply(group_parts, lapply, getElement, 'time_since')
  error_parts <- lapply(group_parts, lapply, getElement, 'error')

  if(is_oco2) {
    # Setup hyperparameters
    simulate_epsilon(
      gamma = 1,
      rho = c(0.5, 0.5),
      ell = c(1, 1),
      time_since = time_since_parts,
      error = error_parts
    )
  } else {
    # Setup hyperparameters
    simulate_epsilon(
      gamma = 1,
      rho = c(0.5, 0.5),
      ell = c(1, 1),
      time_since = time_since_parts,
      error = error_parts
    )
  }
}

observations <- fst::read_fst(args$observations, columns = c(
  'observation_id',
  'overall_observation_mode',
  'observation_group',
  'hyperparameter_group',
  'time',
  'error'
)) %>%
  filter(
    overall_observation_mode %in% args$overall_observation_mode,
  )

output  <-  observations %>% 
  arrange(time) %>%
  mutate(
    fit_group = if_else(
      overall_observation_mode == 'IS',
      '4_IS',
      as.character(observation_group)
    )
  ) %>%
  group_by(fit_group) %>%
  mutate(epsilon = produce_group_epsilons(.)) %>%
  ungroup() %>%
  select(c(observation_id, epsilon))
