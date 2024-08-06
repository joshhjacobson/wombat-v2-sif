library(argparse)
library(dplyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--input-files', nargs = '+')
parser$add_argument('--control-emissions')
parser$add_argument('--output')
args <- parser$parse_args()

flux_key <- c(
  'GPP' = 'GPP',
  'GPP_HB' = 'GPP',
  'TER' = 'TER',
  'TER_HB' = 'TER',
  'NEE' = 'NEE'
)

read_fluxcom <- function(filename) {
  fn <- ncdf4::nc_open(filename)
  on.exit(ncdf4::nc_close(fn))
  v <- function(...) ncdf4::ncvar_get(fn, ...)
  fields <- names(fn$var)
  stopifnot(length(fields) == 1 & fields[1] %in% names(flux_key))
  field_name <- fields[1]
  flux_name <- flux_key[field_name]
  expand.grid(
    longitude = as.vector(v('lon')),
    latitude = as.vector(v('lat')),
    time = ncvar_get_time(fn, 'time'),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      inventory = flux_name,
      method = paste(field_name, ncdf4::ncatt_get(fn, 0)$method, sep = '.'),
      value = as.vector(v(field_name))
    )
}

cell_area <- fst::read_fst(args$control_emissions) %>%
  distinct(longitude, latitude, cell_height, area) %>%
  mutate(
    latitude_bottom = latitude - cell_height / 2
  ) %>%
  select(-cell_height)

log_debug('Reading aggregated FLUXCOM data')
output <- lapply(args$input_files, read_fluxcom) %>%
  bind_rows() %>%
  mutate(
    inventory = factor(c(
      'GPP' = 'bio_assim',
      'TER' = 'bio_resp_tot',
      'NEE' = 'nee'
    )[inventory], levels = c(
      'bio_assim',
      'bio_resp_tot',
      'nee'
    )),
    method = factor(method),
    value = if_else(inventory == 'bio_assim', -value, value)
  ) %>%
  filter(complete.cases(value)) %>%
  left_join(cell_area, by = c('longitude', 'latitude'))

log_debug('Saving to {args$output}')
fst::write_fst(output, args$output)

log_debug('Done')
