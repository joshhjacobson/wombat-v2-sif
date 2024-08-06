library(stars)
library(gstat)
library(dplyr)
library(ggplot2)

source("partials/display.R")


# Read in data, filter to 2015-2016, and combine with area weights
path <- "data/eda/sib4/daily"
file_list <- list.files(path, full.names = TRUE)

sib4 <- read_stars(file_list, sub = c("sif_nonzero", "assim_nonzero"), along = "time") %>%
    st_set_dimensions(names = c("lon", "lat", "time")) %>%
    st_set_crs(4326)

sif_2016_06  <- sib4 %>% 
    select("sif_nonzero") %>% 
    filter(between(time, as.POSIXct("2016-06-01", "UTC"), as.POSIXct("2016-07-01", "UTC")))

# v_st  <- variogramST(
#     sif_nonzero~1,
#     data = sif_2016_06
# )

# would need data in space-wide format
prcomp
