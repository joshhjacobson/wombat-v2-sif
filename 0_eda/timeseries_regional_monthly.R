library(ncdf4)
library(stars)
library(xts)
library(dplyr)
library(ggplot2)

source("partials/display.R")

TIME_RESOLUTION <- "monthly"

# Get grid areas and compute weights
area <- read_ncdf("data/area-1x1.nc")
total_area <- sum(area$cell_area)
area_weights <- st_apply(area, c("lon", "lat"), function(a) a / total_area)
names(area_weights) <- "area_weight"

# Read in data and combine with area weights
path <- paste0("data/eda/sib4/", TIME_RESOLUTION, "/")
file_list <- list.files(path, full.names = TRUE)

sif <- read_stars(file_list, sub = "sif_nonzero", along = "time") %>%
  st_set_dimensions(names = c("lon", "lat", "time")) %>%
  st_set_crs(4326) %>%
  st_join(area_weights) %>%
  mutate(sif_weighted = sif_nonzero * area_weight)

assim <- read_stars(file_list, sub = "assim_nonzero", along = "time") %>%
  st_set_dimensions(names = c("lon", "lat", "time")) %>%
  st_set_crs(4326) %>%
  st_join(area_weights) %>%
  mutate(assim_weighted = assim_nonzero * area_weight)

ggplot() +
  geom_stars(data = sif %>% slice("time", 1:6) %>% select("sif_nonzero")) +
  facet_wrap(~time) +
  theme_void()

ggplot() +
  geom_stars(data = assim %>% slice("time", 1:6) %>% select("assim_nonzero")) +
  facet_wrap(~time) +
  theme_void()

# Read in the TRANSCOM3 mask, align with sib4 grid, and apply mask
sib4_grid <- sif %>% slice("time", 1)

transcom_path <- "data/TRANSCOM_mask_original_1x1.nc"

transcom_mask <- read_ncdf(transcom_path, var = "mask64") %>%
  st_set_dimensions(names = c("lon", "lat")) %>%
  mutate(regions = as.integer(mask64)) %>%
  select(regions) %>%
  st_warp(sib4_grid)

sif <- sif %>% st_join(transcom_mask)
assim <- assim %>% st_join(transcom_mask)


# Compute area-weighted mean over time by region
sif_weighted_mean <- sif %>%
  select(sif_weighted, regions) %>%
  as_tibble() %>%
  group_by(regions, time) %>%
  summarise(sif_weighted_mean = sum(sif_weighted, na.rm = TRUE))

assim_weighted_mean <- assim %>%
  select(assim_weighted, regions) %>%
  as_tibble() %>%
  group_by(regions, time) %>%
  summarise(assim_weighted_mean = sum(assim_weighted, na.rm = TRUE))


# Combine time series and standardize
time_series <- merge(sif_weighted_mean, assim_weighted_mean) %>%
  as_tibble() %>%
  group_by(regions) %>%
  mutate(
    sif_std = (sif_weighted_mean - mean(sif_weighted_mean)) / sd(sif_weighted_mean),
    assim_std = (assim_weighted_mean - mean(assim_weighted_mean)) / sd(assim_weighted_mean),
  ) %>%
  select(regions, time, sif_std, assim_std)

# Plot bivariate time series
p <- ggplot(time_series) +
  facet_wrap(~regions, ncol = 2) +
  geom_point(aes(x = time, y = sif_std, color = "SIF")) +
  geom_line(aes(x = time, y = sif_std, color = "SIF")) +
  geom_point(aes(x = time, y = assim_std, color = "GPP")) +
  geom_line(aes(x = time, y = assim_std, color = "GPP")) +
  labs(x = NULL, y = "Standardized Process") +
  theme(legend.title = element_blank())

ggsave_base(
  paste0("0_eda/figures/time_series_regional_", TIME_RESOLUTION, ".png"),
  p,
  width = 24,
  height = 33,
)
