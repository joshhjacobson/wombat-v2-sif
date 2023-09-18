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
test %>% count(mask64)
