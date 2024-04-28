library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Rcpp)

source(Sys.getenv('UTILS_PARTIAL'))
sourceCpp(Sys.getenv('UTILS_CPP_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--region-mask')
parser$add_argument('--sib4-climatology-assim')
parser$add_argument('--sib4-climatology-resp-tot')
parser$add_argument('--basis-vectors')
parser$add_argument('--control-emissions')
parser$add_argument('--alpha-wombat-v2')
parser$add_argument('--output')
args <- parser$parse_args()

# args <- list(
#   region_mask = '1_transport/intermediates/region-mask.rds',
#   sib4_climatology_assim = '5_results/intermediates/sib4-climatology-assim-2x25.nc',
#   sib4_climatology_resp_tot = '5_results/intermediates/sib4-climatology-resp-tot-2x25.nc',
#   basis_vectors = '4_inversion/intermediates/basis-vectors.fst',
#   control_emissions = '4_inversion/intermediates/control-emissions.fst',
#   alpha_wombat_v2 = 'data/wombat-v2-alpha-LNLGIS.fst',
#   output = '4_inversion/intermediates/osse-alpha.fst'
# )

set.seed(20240425)

basis_vectors <- fst::read_fst(args$basis_vectors)
control_emissions <- fst::read_fst(args$control_emissions)
alpha <- fst::read_fst(args$alpha_wombat_v2)

region_list <- readRDS(args$region_mask)
region_values <- region_list[[1]]
for (i in 2:length(region_list)) {
  x <- region_list[[i]]
  region_values[x == 1] <- i
}
region_values[region_values == 0] <- NA

region_grid <- expand.grid(
  longitude = attr(region_list$Region01, 'longitude'),
  latitude = attr(region_list$Region01, 'latitude')
) %>%
  mutate(
    region = as.vector(region_values)
  ) %>%
  filter(!is.na(region)) %>%
  mutate(
    region = factor(names(region_list)[region])
  )

climatology_linear <- bind_rows(
  read_climatology(args$sib4_climatology_assim) %>%
    mutate(
      value = -value,
      inventory = factor('bio_assim')
    ),
  read_climatology(args$sib4_climatology_resp_tot) %>%
    mutate(inventory = factor('bio_resp_tot'))
) %>%
  rename(component = variable) %>%
  filter(component %in% c('intercept', 'trend')) %>%
  left_join(
    region_grid,
    by = c('longitude', 'latitude')
  ) %>%
  filter(region %in% sprintf('Region%02d', 1:11)) %>%
  left_join(
    control_emissions %>% distinct(longitude, latitude, area),
    by = c('longitude', 'latitude')
  ) %>%
  mutate(month = factor(NA)) %>%
  add_basis_vector(basis_vectors)

perturbations <- climatology_linear %>%
  group_by(basis_vector) %>%
  summarise(value = sum(area * value)) %>%
  left_join(
    climatology_linear %>%
      distinct(basis_vector, inventory, component, region),
    by = 'basis_vector'
  )

log_debug('Simulating adjusted linear component for bio_assim')
alpha_bio_assim_linear <- alpha %>%
  filter(
    inventory == 'bio_assim',
    component %in% c('intercept', 'trend')
  ) %>%
  mutate(
    # value = (1 + rnorm(nrow(.), sd = 0.2)) * value
    value = (1 + 5) * value,
    alpha_adjusted = 5 * value
  ) %>%
  select(c(basis_vector, inventory, component, region, value, alpha_adjusted))

log_debug('Computing adjusted linear component for bio_resp_tot')
# NOTE(jhj): This construction ensures that the implied linear component for NEE is unchanged
alpha_bio_resp_tot_linear <- perturbations %>%
  left_join(
    alpha_bio_assim_linear %>% select(
      c(basis_vector, alpha_adjusted)
    ),
    by = 'basis_vector'
  ) %>%
  mutate(
    value = if_else(inventory == 'bio_assim', alpha_adjusted * value, value)
  ) %>%
  select(-c(basis_vector, alpha_adjusted)) %>%
  tidyr::pivot_wider(
    names_from = inventory,
    values_from = value
  ) %>%
  mutate(
    value = -bio_assim / bio_resp_tot,
    inventory = 'bio_resp_tot'
  ) %>%
  select(-c(bio_assim, bio_resp_tot)) %>%
  left_join(
    perturbations %>%
      distinct(basis_vector, inventory, component, region),
    by = c('inventory', 'component', 'region')
  )

alpha_bio_linear <- bind_rows(
  alpha_bio_assim_linear %>% select(-alpha_adjusted),
  alpha_bio_resp_tot_linear
) %>%
  mutate(
    basis_vector_str = as.character(basis_vector),
    month = NA
  )

output <- bind_rows(
  alpha_bio_linear,
  alpha %>%
    filter(
      !(inventory == 'bio_assim' & component %in% c('intercept', 'trend'))
    )
) %>%
  mutate(
    basis_vector = factor(
      basis_vector_str,
      levels = levels(basis_vectors$basis_vector)
    )
  )

log_debug('Writing to {args$output}')
fst::write_fst(output, args$output)

log_debug('Done')
