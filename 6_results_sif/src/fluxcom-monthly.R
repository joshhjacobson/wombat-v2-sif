library(argparse)
library(dplyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--fluxcom-gpp-monthly-2x25')
parser$add_argument('--fluxcom-ter-monthly-2x25')
parser$add_argument('--fluxcom-nee-monthly-2x25')
parser$add_argument('--output')
args <- parser$parse_args()

read_fluxcom <- function(filename, field_name) {
  fn <- ncdf4::nc_open(filename)
  on.exit(ncdf4::nc_close(fn))
  v <- function(...) ncdf4::ncvar_get(fn, ...)
  expand.grid(
    longitude = as.vector(v('lon')),
    latitude = as.vector(v('lat')),
    time = ncvar_get_time(fn, 'time'),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      inventory = field_name,
      value = as.vector(v(field_name)),
      value_mad = as.vector(v(paste0(field_name, '_mad')))
    )
}

log_debug('Reading aggregated FLUXCOM data')
output <- bind_rows(
  read_fluxcom(args$fluxcom_gpp_monthly_2x25, 'GPP'),
  read_fluxcom(args$fluxcom_ter_monthly_2x25, 'TER'),
  read_fluxcom(args$fluxcom_nee_monthly_2x25, 'NEE')
) %>%
  mutate(
    inventory = factor(c(
      'GPP' = 'GPP',
      'TER' = 'Respiration',
      'NEE' = 'NEE'
    )[inventory], levels = c(
      'GPP',
      'Respiration',
      'NEE'
    )),
    value = if_else(inventory == 'GPP', -value, value)
  )

log_debug('Writing output to {args$output}')
fst::write_fst(output, args$output)

log_debug('Done')
