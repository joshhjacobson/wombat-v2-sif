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

flux_aggregator_base <- perturbations %>%
  add_basis_vector(basis_vectors) %>%
  mutate(
    inventory_region_time = interaction(
      inventory,
      region,
      time,
      drop = TRUE
    )
  ) %>%
  left_join(cell_area, by = c('longitude', 'latitude'))

flux_aggregator <- flux_aggregator_base %>%
  group_by(inventory_region_time, basis_vector) %>%
  summarise(value = KG_M2_S_TO_PGC_MONTH * sum(area * value)) %>%
  left_join(
    flux_aggregator_base %>%
      distinct(inventory_region_time, inventory, region, time),
    by = 'inventory_region_time'
  )

fst::write_fst(flux_aggregator, args$output)
