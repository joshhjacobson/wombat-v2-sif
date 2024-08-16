library(argparse)
library(dplyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--land-fraction')
parser$add_argument('--output')
args <- parser$parse_args()

land_fraction <- read_gridded_data(args$land_fraction, 'land_fraction', include_time = FALSE)

output <- expand.grid(
  longitude = land_fraction$longitude,
  latitude = land_fraction$latitude,
  stringsAsFactors = FALSE
) %>%
  mutate(
    land_fraction = as.vector(land_fraction$value)
  )

fst::write_fst(output, args$output)
