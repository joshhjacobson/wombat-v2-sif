library(dplyr)
library(furrr)
library(ncdf4)

source("partials/utils.R")

# Collect list of files from SIF subdirectory for 2014 through 2020
sif_dir <- "/data/OCO-2/OCO2_L2_Lite_SIF.10r/"
sif_files <- c()
for (year in 2014:2020) {
    year_dir <- paste0(sif_dir, year)
    sif_files <- append(sif_files, list.files(year_dir, full.names = TRUE))
}

# Read a SIF file and apply pre-processing
read_oco2_sif_lite_file <- function(filename) {
    with_nc_file(list(fn = filename), {
        v <- function(...) as.vector(ncvar_get(fn, ...))
        oco2_soundings <- tibble(
            sounding_id = v("Metadata/SoundingId"),
            operation_mode = v("Metadata/MeasurementMode"),
            time = ncvar_get_time(fn, "Delta_Time"),
            longitude = v("Longitude"),
            latitude = v("Latitude"),
            value = v("SIF_740nm"),
            value_daily = v("Daily_SIF_740nm"),
            measurement_error = v("SIF_Uncertainty_740nm"),
            flag = v("Quality_Flag"),
        ) %>%
            filter(
                operation_mode < 2,
                flag < 2,
                value + 3 * measurement_error > 0
            ) %>%
            mutate(operation_mode = factor(operation_mode))
    })
}

# Read in all files in parallel
plan(multisession, workers = 64)
options <- furrr_options(packages = c("dplyr", "ncdf4"))

oco_sif <- future_map(sif_files, read_oco2_sif_lite_file) %>% bind_rows()
oco_sif

# Write out to fst file
fst::write.fst(oco_sif, "data/eda/oco2/oco2_sif_lite_2014-2020.fst")
