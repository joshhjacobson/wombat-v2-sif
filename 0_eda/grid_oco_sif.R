library(dplyr)
library(fst)

# Read OCO-2 SIF data
oco_sif <- read.fst("data/eda/oco2/oco2_sif_lite_2014-2020.fst") %>% as_tibble()

lon_breaks <- seq(-180, 180, 1)
lon_labs <- seq(-179.5, 179.5, 1)
lat_breaks <- seq(-90, 90, 1)
lat_labs <- seq(-89.5, 89.5, 1)

# Grid the irregularly spaced OCO-2 data to daily 1x1 degree grid
oco_sif_grid <- oco_sif %>%
    mutate(
        date = as.Date(time),
        lon_bin = cut(longitude, breaks = lon_breaks, right = FALSE, labels = FALSE),
        lat_bin = cut(latitude, breaks = lat_breaks, right = FALSE, labels = FALSE)
    ) %>% 
    mutate(
        lon_bin = lon_labs[lon_bin],
        lat_bin = lat_labs[lat_bin]
    ) %>%
    select(date, lon_bin, lat_bin, value, value_daily, measurement_error) %>%
    group_by(date, lon_bin, lat_bin) %>%
    summarise(
        value = mean(value),
        value_daily = mean(value_daily),
        measurement_error = mean(measurement_error)
    ) %>%
    ungroup() %>%
    rename(
        longitude = lon_bin,
        latitude = lat_bin
    )

head(oco_sif_grid)

# Write out to fst file
write.fst(oco_sif_grid, "data/eda/oco2/oco2_sif_2014-2020_daily_1deg.fst")
