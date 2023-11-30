library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(ncdf4)
library(lubridate, warn.conflicts = FALSE)
library(parallel)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--oco2-sif-directory')
parser$add_argument('--start-date')
parser$add_argument('--end-date')
parser$add_argument('--output')
args <- parser$parse_args()


sif_lite_get_date <- function(filename){
    paste0('20', stringr::str_extract(filename, '\\d{6}'))
}


log_info('Loading OCO-2 SIF observations from {args$oco2_observations_sif}')
sif_paths <- list.files(
    args$oco2_sif_directory,
    pattern = '.nc4$',
    full.names = TRUE,
    recursive = TRUE
)
sif_times <- strptime(sif_lite_get_date(basename(sif_paths)), '%Y%m%d', tz = 'UTC')
sif_paths <- sif_paths[
  sif_times >= args$start_date & sif_times < args$end_date
]
sif_soundings <- bind_rows(mclapply(sif_paths, function(filename) {
  log_trace('Loading {filename}')
  with_nc_file(list(fn = filename), {
    v <- function(...) as.vector(ncvar_get(fn, ...))
    oco2_soundings <- tibble(
        time = ncvar_get_time(fn, 'Delta_Time'),
        longitude = v('Longitude'),
        latitude = v('Latitude'),
        value = v('SIF_740nm'),
        measurement_error = v('SIF_Uncertainty_740nm'),
        oco2_operation_mode = v('Metadata/MeasurementMode'),
        flag = v('Quality_Flag'),
      ) %>%
      filter(
        oco2_operation_mode < 2, # nadir and glint
        flag < 2,
        value + 3 * measurement_error > 0
      ) %>%
      select(-flag)
  })
}, mc.cores = get_cores()))


log_info('Computing 10s averages for OCO-2 SIF observations')
observations_sif <- sif_soundings %>%
  mutate(
    time_bin = cut(time, seq(
      from = as.POSIXct(args$start_date, tz = 'UTC'),
      to = as.POSIXct(args$end_date, tz = 'UTC'),
      by = '10 sec'
    ))
  ) %>%
  select(-time) %>%
  group_by(time_bin, oco2_operation_mode) %>%
  summarise(
    count = n(),
    longitude = mean(longitude),
    latitude = mean(latitude),
    value = mean(value),
    measurement_error = sqrt(sum(measurement_error^2) / count^2)
  ) %>%
  ungroup() %>%
  filter(count >= 30) %>%
  mutate(
    time = as.POSIXct(as.character(time_bin), tz = 'UTC') %m+% seconds(5),
    observation_id = stringr::str_replace_all(time, "[^[:digit:]]", ""),
    oco2_operation_mode = as.factor(oco2_operation_mode),
    observation_type = as.factor('oco2')
  ) %>%
  select(-c(time_bin, count)) %>%
  select(observation_id, observation_type, time, everything())

stopifnot(!any(is.na(observations_sif$value)))

log_info('Saving')
fst::write_fst(observations_sif, args$output)

log_info('Done')
