library(ncdf4)
library(stars)
library(dplyr)
library(ggplot2)

source("partials/display.R")

TIME_RESOLUTION <- "monthly"

# Read in data and combine with area weights
path <- paste0("data/eda/sib4/", TIME_RESOLUTION, "/")
file_list <- list.files(path, full.names = TRUE)

sif <- read_stars(file_list, sub = "sif_nonzero", along = "time") %>%
  st_set_dimensions(names = c("lon", "lat", "time")) %>%
  st_set_crs(4326) 

assim <- read_stars(file_list, sub = "assim_nonzero", along = "time") %>%
  st_set_dimensions(names = c("lon", "lat", "time")) %>%
  st_set_crs(4326)


# Compute means for each latitudinal band
sif_bands <- sif %>%
  select(sif_nonzero) %>%
  st_apply(c("lat", "time"), mean, na.rm = TRUE) %>% 
  as_tibble() %>% 
  mutate(month = format(as.Date(time), "%m")) %>% 
  select(-time) %>% 
  group_by(lat, month) %>% 
  summarise(sif = mean(mean, na.rm = TRUE)) %>% 
  na.omit()

assim_bands <- assim %>%
  select(assim_nonzero) %>%
  st_apply(c("lat", "time"), mean, na.rm = TRUE) %>% 
  as_tibble() %>% 
  mutate(month = format(as.Date(time), "%m")) %>% 
  select(-time) %>% 
  group_by(lat, month) %>% 
  summarise(assim = mean(mean, na.rm = TRUE)) %>% 
  na.omit()


# Combine and standardize
lat_bands <- full_join(sif_bands, assim_bands) %>% 
  mutate(
    sif_std = (sif - mean(sif)) / sd(sif),
    assim_std = (assim - mean(assim)) / sd(assim),
  ) %>%
  select(lat, month, sif_std, assim_std)


# Plot lat means by month
p <- ggplot(lat_bands) +
  geom_point(aes(x = sif_std, y = lat, color = "SIF")) +
  geom_point(aes(x = assim_std, y = lat, color = "GPP")) +
  facet_wrap(~month, nrow = 2) +
  labs(x="Standardized Process", y="Latitude") +
  theme(legend.title = element_blank())
p

ggsave_base(
  paste0("0_eda/figures/lat_mean_by_month.png"), 
  p,
  width=23,
  height=15,
)
