library(ncdf4)
library(stars)
library(dplyr)
library(ggplot2)

source("partials/display.R")

# Read in the TRANSCOM3 grid
transcom_path <- "data/TRANSCOM_mask_original_1x1.nc"

transcom_mask <- read_ncdf(transcom_path, var = "mask64") %>%
  st_set_dimensions(names = c("lon", "lat")) %>%
  mutate(regions = as.integer(mask64))

ggplot() +
  geom_stars(data = transcom_mask)

p <- ggplot() +
  geom_stars(data = transcom_mask) +
  facet_wrap(~regions) + 
  theme(legend.position="none")

ggsave_base(
  paste0("0_eda/figures/transcom_regions.png"),
  p,
  width = 18,
  height = 20,
)


test <- transcom_mask %>% as_tibble() # %>% group_by(mask64)
test_mask %>% count(mask64)


# Repeat with 2x2.5-degree grid
transcom_path <- '1_transport/intermediates/TRANSCOM_mask_GEOS_Chem_2x2.5.nc'
transcom_mask <- read_ncdf(transcom_path) %>%
  st_redimension(name="mask_level") %>%
  st_set_dimensions(which="mask_level", values=seq(0,22)) %>% 
  st_apply(c("lon", "lat"), function(x) which(x == 1)-1) %>% 
  rename(regions = everything())

mask_sf <- transcom_mask %>%  st_as_sf()

ggplot() +
  geom_sf(data = mask_sf, mapping=aes(fill=regions)) +
  facet_wrap(~regions)
