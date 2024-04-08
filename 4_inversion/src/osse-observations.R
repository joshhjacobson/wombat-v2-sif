library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--case')
parser$add_argument('--observations')
parser$add_argument('--basis-vectors')
parser$add_argument('--prior')
parser$add_argument('--wombat-v2-alphas')
parser$add_argument('--errors')
parser$add_argument('--overall-observation-mode', nargs = '+')
parser$add_argument('--control', nargs = '+')
parser$add_argument('--component-name', nargs = '+')
parser$add_argument('--component-parts', nargs = '+')
parser$add_argument('--component-transport-matrix', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

log_debug('Loading control')
control <- bind_rows(lapply(args$control, fst::read_fst)) %>%
  mutate(observation_id = droplevels(observation_id))

log_debug('Loading errors')
errors <- fst::read_fst(args$errors) %>%
  mutate(observation_id = droplevels(observation_id))

log_debug('Loading observations')
observations <- fst::read_fst(args$observations) %>%
  filter(
    assimilate == 1,
    overall_observation_mode %in% args$overall_observation_mode,
    observation_id %in% control$observation_id
  ) %>%
  arrange(observation_group, time) %>%
  left_join(
    control %>%
      select(observation_id, value_control = value),
    by = 'observation_id'
  ) %>%
  left_join(
    errors %>%
      select(observation_id, epsilon),
    by = 'observation_id'
  ) %>%
  mutate(
    observation_id = factor(
      as.character(observation_id),
      as.character(observation_id)
    )
  )

control <- control %>%
  filter(observation_id %in% observations$observation_id) %>%
  mutate(
    observation_id = factor(
      as.character(observation_id),
      levels(observations$observation_id)
    )
  )

stopifnot(all(
  levels(observations$observation_id) == levels(control$observation_id)
))
stopifnot(nlevels(observations$observation_id) == nrow(observations))

observations <- observations %>%
  mutate(
    value = value_control + epsilon
  ) %>%
  select(-c(value_control, epsilon))

if (args$case == 'zero') {
  log_debug('No peturbation for OSSE case `{args$case}`')
} else if (args$case == 'wombat-v2') {
  log_debug('Computing peturbation for OSSE case `{args$case}`')
  alpha <- fst::read_fst(args$wombat_v2_alphas)
  basis_vectors <- fst::read_fst(args$basis_vectors)
  prior <- readRDS(args$prior)

  n_alpha <- nrow(basis_vectors)
  # NOTE(mgnb): values of alpha fixed to zero are given infinite precision
  alpha_to_include <- is.finite(diag(prior$precision))

  stopifnot(length(alpha) == length(alpha_to_include))

  part_indices <- seq_along(args$component_name)
  observations$component_name <- ''
  for (part_i in part_indices) {
    observations$component_name <- ifelse(
      observations$overall_observation_mode %in% strsplit(
        args$component_parts[part_i],
        '|',
        fixed = TRUE
      )[[1]],
      args$component_name[part_i],
      observations$component_name
    )
  }

  log_trace('Loading transport matrices')
  H_parts <- lapply(part_indices, function(part_i) {
    log_trace('Loading {args$component_transport_matrix[part_i]}')
    name_i <- args$component_name[part_i]
    observations_i <- observations %>%
      filter(component_name == name_i)
    n_observations_i <- observations_i %>%
      nrow() %>%
      as.double()

    n_i <- n_observations_i * n_alpha
    log_trace('Total size to load is {n_i} = {n_observations_i} x {n_alpha}')

    fn <- pipe(sprintf('lz4 -v %s -', args$component_transport_matrix[part_i]), 'rb')
    H_vec <- readBin(fn, 'double', n_i)
    close(fn)
    output <- matrix(H_vec, nrow = n_observations_i)
    gc()
    output[, alpha_to_include]
  })
  gc()

  log_trace('Adding perturbations to observations')
  observations <- bind_rows(lapply(part_indices, function(i) {
    observations %>%
      filter(
        component_name == args$component_name[i]
      ) %>%
      mutate(
        value = value + as.vector(H_parts[[i]] %*% alpha)
      )
  }))
} else {
  stop('OSSE case {args$case} not supported')
}

log_debug('Saving to {args$output}')
fst::write_fst(observations, args$output)

log_debug('Done')
