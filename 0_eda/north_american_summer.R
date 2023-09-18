library(stars)
library(dplyr)
library(ggplot2)

source("partials/display.R")

TIME_RESOLUTION <- "daily"

# Read in data
path <- paste0("data/eda/sib4/", TIME_RESOLUTION, "/")
file_list <- list.files(path, full.names = TRUE)

sif <- read_stars(file_list, sub = "sif_nonzero", along = "time") %>%
    st_set_dimensions(names = c("lon", "lat", "time")) %>%
    st_set_crs(4326)

assim <- read_stars(file_list, sub = "assim_nonzero", along = "time") %>%
    st_set_dimensions(names = c("lon", "lat", "time")) %>%
    st_set_crs(4326)


# Read in the TRANSCOM3 mask and align with sib4 grid
sib4_grid <- sif %>% slice("time", 1)

transcom_path <- "data/TRANSCOM_mask_original_1x1.nc"

transcom_mask <- read_ncdf(transcom_path, var = "mask64") %>%
    st_set_dimensions(names = c("lon", "lat")) %>%
    mutate(regions = as.integer(mask64)) %>%
    select(regions) %>%
    st_warp(sib4_grid)

# Filter data to summer months and North America
sif_north_america_summer <- sif %>%
    st_join(transcom_mask) %>%
    as_tibble() %>%
    mutate(
        year = clock::get_year(time),
        month = clock::get_month(time),
    ) %>%
    select(-time) %>%
    filter(month %in% c(6, 7, 8)) %>%
    filter(regions %in% c(1, 2))

assim_north_america_summer <- assim %>%
    st_join(transcom_mask) %>%
    as_tibble() %>%
    mutate(
        year = clock::get_year(time),
        month = clock::get_month(time),
    ) %>%
    select(-time) %>%
    filter(month %in% c(6, 7, 8)) %>%
    filter(regions %in% c(1, 2))

# Join sif and assim data
sib4_north_america_summer <- full_join(
    sif_north_america_summer, assim_north_america_summer,
    by = c("lon", "lat", "year", "month", "regions")
)

sib4_us_summer_2016 <- sib4_north_america_summer %>%
    filter(regions == 2 & year == 2016)

# Plot
p <- ggplot(data = sib4_us_summer_2016) +
    geom_hex(aes(x = sif_nonzero, y = assim_nonzero))

ggsave_base(
  paste0("0_eda/figures/sif_gpp_tc2_summer_2016.png"),
  p,
  width = 12,
  height = 12,
)
