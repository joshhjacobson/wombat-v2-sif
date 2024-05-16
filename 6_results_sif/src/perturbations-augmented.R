library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Rcpp)

source(Sys.getenv('UTILS_PARTIAL'))
sourceCpp(Sys.getenv('UTILS_CPP_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--basis-vectors')
parser$add_argument('--control-emissions')
parser$add_argument('--perturbations')
parser$add_argument('--output')
args <- parser$parse_args()

basis_vectors <- fst::read_fst(args$basis_vectors)
control_emissions <- fst::read_fst(args$control_emissions)
perturbations <- fst::read_fst(args$perturbations)
cell_area <- control_emissions %>%
  distinct(longitude, latitude, cell_height, area) %>%
  mutate(
    latitude_bottom = latitude - cell_height / 2
  ) %>%
  select(-cell_height)

perturbations_augmented <- perturbations %>%
  add_basis_vector(basis_vectors) %>%
  mutate(
    inventory_time = interaction(
      inventory,
      time,
      drop = TRUE
    )
  ) %>%
  left_join(cell_area, by = c('longitude', 'latitude'))

fst::write_fst(perturbations_augmented, args$output)
