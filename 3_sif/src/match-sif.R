library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(ncdf4)
library(parallel)

source(Sys.getenv('UTILS_PARTIAL'))

read_inventory <- function(inventory_filename) {
  log_trace('Opening {inventory_filename}')
  inventory_fn <- nc_open(inventory_filename)

  time <- ncvar_get_time(inventory_fn, 'time')
  time_width <- diff(time)[1]
  stopifnot(all(diff(time) == time_width))
  latitude <- as.vector(ncvar_get(inventory_fn, 'lat'))
  cell_height <- get_cell_height(latitude)
  longitude <- as.vector(ncvar_get(inventory_fn, 'lon'))
  cell_width <- get_cell_width(longitude)
  sif <- ncvar_get(inventory_fn, 'sif')

  nc_close(inventory_fn)

  list(
    time = time,
    time_width = time_width,
    latitude = latitude,
    cell_height = cell_height,
    longitude = longitude,
    cell_width = cell_width,
    sif = sif
  )
}

match_observations <- function(observations, inventory) {
  period_start <- min(inventory$time) - inventory$time_width / 2
  period_end <- max(inventory$time) + inventory$time_width / 2

  log_trace('Subsetting SIF observations to between {period_start} and {period_end}')
  observations_part <- observations %>%
    filter(
      time >= period_start,
      time < period_end
    )
  if (nrow(observations_part) == 0) {
    return(tidyr::tibble(
      observation_type = factor(),
      observation_id = factor(),
      time = POSIXct(),
      longitude = numeric(),
      latitude = numeric(),
      species = factor(),
      value = numeric()
    ))
  }

  log_trace('Computing match indices for {nrow(observations_part)} observations')
  indices <- match_grid(observations_part$longitude, observations_part$latitude, inventory)
  indices$time <- match_time(observations_part$time, inventory)

  log_trace('Subsetting SIF inventory')
  sif_match <- inventory$sif[cbind(indices$longitude, indices$latitude, indices$time)]
  
  observations_part %>%
    select(
      observation_type,
      observation_id
    ) %>%
    mutate(
      observation_id = factor(observation_id),
      time = inventory$time[indices$time],
      longitude = inventory$longitude[indices$longitude],
      latitude = inventory$latitude[indices$latitude],
      species = factor('SIF'),
      value = sif_match
    )
}

parser <- ArgumentParser()
parser$add_argument('--oco2-observations-sif')
parser$add_argument('--inventory-list')
parser$add_argument('--linear-models')
parser$add_argument('--output')
args <- parser$parse_args()

inventory_filenames <- strsplit(args$inventory_list, " ")[[1]]

log_info('Loading OCO-2 SIF observations from {args$oco2_observations}')
oco2_observations <- fst::read_fst(args$oco2_observations_sif)

sif_control <- bind_rows(mclapply(inventory_filenames, function(filename) {
  sib4_inventory <- read_inventory(filename)
  match_observations(oco2_observations, sib4_inventory)
}, mc.cores = get_cores()))

log_info('Loading fitted SIF-GPP models from {args$linear_models}')
linear_models <- fst::read_fst(args$linear_models)

sif_control <- sif_control %>%
  mutate(
    month = lubridate::month(time)
  ) %>% 
  inner_join(
    linear_models,
    by = c('longitude', 'latitude', 'month')
  ) %>% 
  mutate(
    outlier = value < lower_fence | value > upper_fence
  ) %>%
  select(
    observation_type,
    observation_id,
    time,
    longitude,
    latitude,
    species,
    value,
    slope,
    lower_fence,
    upper_fence,
    outlier
  )

log_info('Saving matched SIF observations to {args$output}')
fst::write_fst(sif_control, args$output)

log_info('Done')