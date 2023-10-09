library(ncmeta)
library(ncdf4)
library(stars)
library(dplyr)
library(ggplot2)

sib4_path <- '/data/2021-06-10_SiB4/'
early_file <- paste0(sib4_path, 'sib4-hourly-2018-01-01.nc')
late_file <- paste0(sib4_path, 'sib4-hourly-2020-12-31.nc')

nc_inq(early_file)
nc_dims(early_file)
nc_vars(early_file)
nc_atts(early_file)

nc <- nc_open(early_file)

# Plot the total pft_area at each location
pft_area <- ncvar_get(nc, "pft_area")  %>% st_as_stars()

# compute the sum over the pft dimension
pft_area_sum <- pft_area %>% 
  st_apply(c("X1", "X2"), sum, na.rm = TRUE)
dim(pft_area_sum)
ggplot() + 
  geom_stars(data = pft_area_sum) +
  theme_void()


# Read in late_file, compute sum over pft dimension
nc2 <- nc_open(late_file)
pft_area2 <- ncvar_get(nc2, "pft_area")  %>% st_as_stars()
pft_area_sum2 <- pft_area2 %>% 
  st_apply(c("X1", "X2"), sum, na.rm = TRUE)

ggplot() + 
  geom_stars(data = pft_area_sum2) +
  theme_void()

(pft_area_sum - pft_area_sum2 )  %>% st_extract()
range(pft_area_sum - pft_area_sum2)
ggplot() + 
  geom_stars(data = pft_area_sum - pft_area_sum2) +
  theme_void()



# Read in the assimilation variable from single_file
assim  <- ncvar_get(nc, "assim")  %>% st_as_stars()
dim(assim)

assim_1 <- assim  %>% slice(X4, 1)

ggplot() + 
  geom_stars(data = assim_1) +
  coord_equal() +
  facet_wrap(~X3) +
  theme_void()

# Read in the sif variable from single_file
sif  <- ncvar_get(nc, "sif")  %>% st_as_stars()

sif_12 <- sif  %>% slice(X4, 12)

ggplot() + 
  geom_stars(data = sif_12) +
  coord_equal() +
  facet_wrap(~X3) +
  theme_void()



# pre-processed file
sib4_path <- 'data/eda/sib4/'
pre_file <- paste0(sib4_path, 'daily_full/sib4-daily-2020.nc')

nc_vars(pre_file)
sib4_daily <- nc_open(pre_file)

sif_non_zero <- ncvar_get(sib4_daily, "sif_non_zero")  %>% st_as_stars()
sif_non_zero %>% slice(X3, 1) -> sif_non_zero_1 

sib4 <- read_stars(pre_file, sub = c("sif", "assim")) %>% 
  st_set_dimensions(names = c("lon", "lat", "time")) %>%
  st_set_crs(4326)

sib4 %>% slice(time, 250) -> sib4_1 
ggplot() +
  geom_stars(data = select(sib4_1, sif))

ggplot() +
  geom_stars(data = select(sib4_1, assim))

assim_non_zero <- ncvar_get(sib4_daily, "assim_non_zero")  %>% st_as_stars()
assim_non_zero %>% slice(X3, 1) -> assim_non_zero_1
ggplot() + 
  geom_stars(data = assim_non_zero_1)


# Check coarsened grid
sib4_path <- 'data/eda/sib4/daily_2x25/sib4-daily-2019.nc'
sib4_daily <- read_ncdf(sib4_path, var = c("sif_nonzero", "assim_nonzero"))

# read list of files as above, then combine with c(., along="time")

sib4_daily %>%
  slice("time", 1)  %>% 
  select(sif_nonzero) %>%
  st_as_sf() -> demo

ggplot() +
  geom_sf(data=demo, mapping=aes(fill=sif_nonzero), linewidth=0)