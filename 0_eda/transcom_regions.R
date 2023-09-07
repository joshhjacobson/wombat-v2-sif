library(ncmeta)
library(ncdf4)
library(stars)
library(dplyr)
library(ggplot2)

transcom_path <- 'data/TRANSCOM_mask_original_1x1.nc'

nc_vars(transcom_path)

transcom_mask <- read_ncdf(transcom_path, var = 'mask64')  %>% 
    st_as_sf()  %>% 
    mutate(mask64 = as.factor(as.int(mask64)))
ggplot() + 
  geom_sf(data = transcom_mask) +
  scale_color_brewer(palette = 'tab20') +
  theme_void()
