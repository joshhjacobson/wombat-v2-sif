library(dplyr)
library(ggplot2)
library(ncmeta)
library(ncdf4)
library(purrr)
library(sf)

source("partials/display.R")
source("partials/utils.R")


sif_path <- "/data/OCO-2/OCO2_L2_Lite_SIF.10r/2020/oco2_LtSIF_200701_B10206r_210920045256s.nc4"
sif_file <- nc_open(sif_path)
units <- sif_file$var[["Delta_Time"]]$units

nc_dims(sif_path)
nc_vars(sif_path)
nc_atts(sif_path)

sif <- st_read(sif_path, driver = "netCDF")

temp <- sif["Daily_SIF_740nm"] %>% sample_n(100)
ggplot() +
    geom_sf(data = temp, aes(fill = Daily_SIF_740nm)) +
    scale_fill_viridis_c()


sif <- sif %>% 
    filter(
        Quality_Flag < 2, 
        SIF_740nm + 3 * SIF_Uncertainty_740nm > 0
    )


sounding_id <- ncvar_get(sif_file, "Metadata/SoundingId")
sounding_id


# Collect list of files from SIF subdirectory for 2014 through 2020
sif_dir <- "/data/OCO-2/OCO2_L2_Lite_SIF.10r/"
sif_files <- c()
for (year in 2019:2020) {
    year_dir <- paste0(sif_dir, year)
    sif_files <- append(sif_files, list.files(year_dir, full.names = TRUE))
}

# Read in SIF files and apply pre-processing
read_oco2_sif_lite_file <- function(filename) {
    with_nc_file(list(fn = filename), {
        v <- function(...) ncvar_get(fn, ...)
        oco2_soundings <- tibble(
            sounding_id = v('Metadata/SoundingId'),
            time = ncvar_get_time(fn, 'Delta_Time'),
            longitude = as.vector(v('Longitude')),
            latitude = as.vector(v('Latitude')),
            value = as.vector(v('SIF_740nm')),
            value_daily = as.vector(v('Daily_SIF_740nm')),
            measurement_error = as.vector(v('SIF_Uncertainty_740nm')),
            flag = as.vector(v('Quality_Flag'))
        ) %>%
            filter(
                flag < 2, 
                value + 3 * measurement_error > 0
            ) %>% 
            mutate(
                oco2_operation_mode = c(
                    'LN', 'LG', 'LT', 'LTT', 'ON', 'OG', 'OT', 'OTT'
                )[sounding_id %% 10]
            ) %>%
            mutate(
                oco2_operation_mode = factor(oco2_operation_mode)
            ) %>% 
            filter(oco2_operation_mode %in% c('LN', 'LG'))
    })
}


out <-  map(c(1, 2), function(x) {tibble(value = rep(x^2, 10))})
out
bind_rows(out)
