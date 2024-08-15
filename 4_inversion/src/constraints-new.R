library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(Rcpp)
library(logger)

source(Sys.getenv('UTILS_PARTIAL'))
sourceCpp(Sys.getenv('UTILS_CPP_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--basis-vectors')
parser$add_argument('--perturbations')
parser$add_argument('--output')
args <- parser$parse_args()

basis_vectors <- fst::read_fst(args$basis_vectors)
perturbations <- fst::read_fst(args$perturbations) %>%
  filter(inventory %in% c('bio_assim', 'bio_resp_tot')) %>%
  add_basis_vector(basis_vectors)

# Construct constraint for the cell total
perturbations_region <- perturbations %>%
  mutate(
    value = ifelse(inventory == 'bio_assim', -value, value)
  )
baseline_region_cell <- perturbations_region %>%
  group_by(region, inventory, longitude, latitude, time) %>%
  summarise(value = sum(value), .groups = 'drop')

F_cell <- with(
  perturbations_region %>%
    left_join(
      baseline_region_cell %>%
        select(-value) %>%
        mutate(cell_index = 1 : n()),
      by = c('inventory', 'longitude', 'latitude', 'time')
    ),
  sparseMatrix(
    i = cell_index,
    j = as.integer(basis_vector),
    x = value,
    dims = c(nrow(baseline_region_cell), nrow(basis_vectors))
  )
)
g_cell <- baseline_region_cell$value

# Construct constraint for residual
with(
  basis_vectors %>%
    filter(
      inventory %in% c('bio_assim', 'bio_resp_tot'),
      component == 'residual'
    ),
  {
    baseline_residual <<- data.frame(
      region = region,
      inventory = inventory,
      longitude = NA,
      latitude = NA,
      time = NA,
      value = NA
    )
    F_residual <<- sparseMatrix(
      i = seq_along(basis_vector),
      j = as.integer(basis_vector),
      dims = c(length(basis_vector), nlevels(basis_vector))
    )
    g_residual <<- rep(1, length(basis_vector))
  }
)


log_info('Saving to {args$output}')
output <- list(
  F_sign = F_cell,
  g_sign = pmax(1e-10, g_cell),
  baseline_sign = baseline_region_cell,
  F_residual = F_residual,
  g_residual = g_residual
)
saveRDS(output, args$output)
log_info('Done')
