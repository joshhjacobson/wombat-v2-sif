library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(parallel)

source(Sys.getenv('UTILS_PARTIAL'))

read_basis <- function(basis_filename) {
  log_trace('Opening {basis_filename}')
  with_nc_file(list(fn = basis_filename), {
    v <- function(...) ncdf4::ncvar_get(fn, ...)
    longitude <- as.vector(v('lon'))
    latitude <- as.vector(v('lat'))
    time <- ncvar_get_time(fn, 'time')
    year <- lubridate::year(time[1])
    component_names <- names(fn$var)
    components <- lapply(component_names, v)
  })
  list(
    year = year,
    time = time,
    latitude = latitude,
    longitude = longitude,
    component_names = component_names,
    components = components
  )
}

parser <- ArgumentParser()
parser$add_argument('--region-mask')
parser$add_argument('--control-sif')
parser$add_argument('--basis-climatology', nargs = '+')
parser$add_argument('--basis-residual', nargs = '+')
parser$add_argument('--output')
args <- parser$parse_args()

log_info('Constructing region grid using {args$region_mask}')
region_list <- readRDS(args$region_mask)

region_values <- region_list[[1]]
for (i in 2 : length(region_list)) {
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

log_info('Reading SIF control from {args$control_sif}')
control <- fst::read_fst(
  args$control_sif,
  columns = c('observation_id', 'longitude', 'latitude', 'time', 'slope')
)

log_info('Constructing SIF sensitivities')
output <- bind_rows(mclapply(seq_along(args$basis_climatology), function(year_index) {
  basis_climatology <- read_basis(args$basis_climatology[year_index])
  basis_residual <- if_else(
    year_index <= length(args$basis_residual),
    read_basis(args$basis_residual[year_index]),
    read_basis(args$basis_residual[year_index-1])
  )

  same_year <- basis_climatology$year == basis_residual$year
  recycled_residual <- (
    (i > length(args$basis_residual)) &
    (basis_climatology$year == basis_residual$year + 1)
  )
  stopifnot(same_year | recycled_residual)

  control_year <- control %>% 
    filter(lubridate::year(time) == basis_climatology$year)

  match_indices <- cbind(
    match(control_year$longitude, basis_climatology$longitude),
    match(control_year$latitude, basis_climatology$latitude),
    match(control_year$time, basis_climatology$time)
  )

  sapply(basis_climatology$components, function(component) {
    component[match_indices]
  }) %>%
    as_tibble() %>%
    rename_with(~ basis_climatology$component_names) %>%
    mutate(
      residual = basis_residual$components[[1]][match_indices]
    ) %>%
    mutate(
      across(
        .cols = everything(),
        .fns = ~ . * control_year$slope
      )
    ) %>%
    bind_cols(
      control_year %>% select(observation_id, longitude, latitude, time)
    ) %>%
    left_join(
      region_grid,
      by = c('longitude', 'latitude')
    ) %>%
    mutate(
      month = factor(format(time, '%Y-%m'))
    ) %>%
    select(
      -c(longitude, latitude, time)
    ) %>%
    tidyr::pivot_longer(
      cols = -c(observation_id, region, month),
      names_to = 'component',
      values_to = 'value'
    ) %>%
    mutate(
      component = factor(component),
      resolution = factor('hourly'),
      inventory = factor('bio_assim')
    ) %>%
    select(
      observation_id, resolution, region, month, inventory, component, value
    )

}, mc.cores = get_cores()))

log_info('Writing SIF sensitivities to {args$output}')
fst::write_fst(output, args$output)

log_info('Done')