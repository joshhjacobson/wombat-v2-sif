library(argparse)
library(dplyr, warn.conflicts = FALSE)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--fluxcom-monthly-2x25')
parser$add_argument('--control-emissions')
parser$add_argument('--area-1x1')
parser$add_argument('--output')
args <- parser$parse_args()

fluxcom_monthly_2x25 <- fst::read_fst(args$fluxcom_monthly_2x25)
cell_area <- fst::read_fst(args$control_emissions) %>%
  distinct(longitude, latitude, cell_height, area) %>%
  mutate(
    latitude_bottom = latitude - cell_height / 2
  ) %>%
  select(-cell_height)

fluxcom_monthly_2x25_base <- fluxcom_monthly_2x25 %>%
  left_join(cell_area, by = c('longitude', 'latitude'))

with_nc_file(list(fn = args$area_1x1), {
  longitude_area <- as.vector(ncdf4::ncvar_get(fn, 'lon'))
  latitude_area <- as.vector(ncdf4::ncvar_get(fn, 'lat'))
  area <- ncdf4::ncvar_get(fn, 'cell_area')
  area_1x1 <- expand.grid(
    longitude = longitude_area,
    latitude = latitude_area
  ) %>%
    mutate(area = as.vector(area))
})

area_495 <- (area_1x1 %>% filter(latitude == 49.5) %>% pull(area))[1]
area_505 <- (area_1x1 %>% filter(latitude == 50.5) %>% pull(area))[1]
area_both <- area_495 + area_505

# Splits grid cells that cross boundaries
fluxcom_monthly_2x25_zonal <- bind_rows(
  fluxcom_monthly_2x25_base %>%
    filter(latitude != 0 & latitude != 50),
  fluxcom_monthly_2x25_base %>%
    filter(latitude == 0) %>%
    mutate(
      latitude = -0.5,
      latitude_bottom = -1,
      area = area / 2
    ),
  fluxcom_monthly_2x25_base %>%
    filter(latitude == 0) %>%
    mutate(
      latitude = 0.5,
      latitude_bottom = 0,
      area = area / 2
    ),
  fluxcom_monthly_2x25_base %>%
    filter(latitude == 50) %>%
    mutate(
      latitude = 49.5,
      latitude_bottom = 49,
      area = area * area_495 / area_both
    ),
  fluxcom_monthly_2x25_base %>%
    filter(latitude == 50) %>%
    mutate(
      latitude = 50.5,
      latitude_bottom = 50,
      area = area * area_505 / area_both
    )
)

fst::write_fst(fluxcom_monthly_2x25_zonal, args$output)
