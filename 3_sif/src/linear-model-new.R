library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(parallel)

source(Sys.getenv('UTILS_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--input-list')
parser$add_argument('--output')
args <- parser$parse_args()


source('partials/utils.R')
lon <- seq(-180, 177.5, 2.5)
lat <- c(-89.5, seq(-88, 88, 2), 89.5)
start <- as.POSIXct('2014-01-01 12:00:00', tz = 'UTC')
end <- as.POSIXct('2020-12-31 12:00:00', tz = 'UTC')
time <- seq(start, end, by = 'day')

grid <- expand.grid(
  longitude = lon,
  latitude = lat
)

solar_table_new <- data.frame(
  mclapply(seq_len(nrow(grid)), function(i, time) {
    Reduce(c, suntools::solarnoon(as.matrix(grid[i, ], nrow = 1), time, POSIXct.out = TRUE)$time)
  }, time, mc.cores = get_cores())
)
colnames(solar_table_new) <- NULL
rownames(solar_table_new) <- as.Date(time)

# NOTE: is there a way to preserve the POSIXct class? (use names/values_transform or type)
df <- cbind(grid, t(solar_table_new)) %>% 
  tidyr::pivot_longer(-c(longitude, latitude), names_to = 'time', values_to = 'solarnoon')





args <- list(input_list = "3_sif/intermediates/sib4-hourly-sif-gpp-2x25-2014.nc 3_sif/intermediates/sib4-hourly-sif-gpp-2x25-2015.nc 3_sif/intermediates/sib4-hourly-sif-gpp-2x25-2016.nc 3_sif/intermediates/sib4-hourly-sif-gpp-2x25-2017.nc 3_sif/intermediates/sib4-hourly-sif-gpp-2x25-2018.nc 3_sif/intermediates/sib4-hourly-sif-gpp-2x25-2019.nc 3_sif/intermediates/sib4-hourly-sif-gpp-2x25-2020.nc")


library(ncdf4)
source('partials/utils.R')

input_paths <- strsplit(args$input_list, " ")[[1]]

sib4_inventory <- bind_rows(lapply(input_paths, function(path) {
  # log_trace('Opening {path}')
  print('Opening {path}')
  fn <- nc_open(path)
  on.exit(nc_close(fn))
  v <- function(...) ncdf4::ncvar_get(fn, ...)
  gridded <- expand.grid(
    longitude = as.vector(v('lon')),
    latitude = as.vector(v('lat')),
    time = ncvar_get_time(fn, 'time'),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      sif = as.vector(v('sif')),
      assim = as.vector(v('assim'))
    )
  gridded$solarnoon <- suntools::solarnoon(
    as.matrix(gridded[,1:2]),
    gridded$time,
    POSIXct.out = TRUE
  )$time
  gridded %>% filter(between(time, solarnoon - minutes(90), solarnoon + minutes(90)))
})) %>% as_tibble()


demo_df <- sib4_inventory %>% sample_n(100)


demo_df$solarnoon <- suntools::solarnoon(as.matrix(demo_df[,1:2]), demo_df$time, POSIXct.out=TRUE)$time
demo_df %>% dplyr::filter(between(time, solarnoon - lubridate::minutes(90), solarnoon + lubridate::minutes(90)))



# # NOTE maybe switch to compute the solar time in parallel (or manual by rough approx.)
# # May need to filter down to non-zero values first too
# solar_time  <- photobiology::solar_time(
#   gridded$time,
#   expand.grid(lon = gridded$longitude, lat = gridded$latitude)
# )

# temp <- photobiology::solar_time(
#   df$time[sample_index],
#   tibble(lon = df$longitude[sample_index], lat = df$latitude[sample_index])
# )
# length(temp)


# # Construct design matrix X as (1, assim)
# # Set y as sif
# # beta_hat <- as.vector(solve(crossprod(X), crossprod(X, y)))
