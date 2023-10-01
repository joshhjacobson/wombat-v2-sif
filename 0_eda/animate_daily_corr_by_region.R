library(stars)
library(dplyr)
library(ggplot2)
library(gganimate)

source("partials/display.R")

# Read in the coarser-resolution SiB4 rectilinear data
path <- "data/eda/sib4/daily_2x25"
file_list <- list.files(path, full.names = TRUE)
sib4 <- lapply(file_list, read_ncdf, var = c("sif_nonzero", "assim_nonzero")) %>% do.call(c, .)

# Read in the TRANSCOM mask
transcom_path <- "1_transport/intermediates/TRANSCOM_mask_GEOS_Chem_2x2.5.nc"
transcom_mask <- read_ncdf(transcom_path) %>%
    st_redimension(name = "mask_level") %>%
    st_set_dimensions(which = "mask_level", values = seq(0, 22), point = FALSE) %>%
    st_apply(c("lon", "lat"), function(x) which(x == 1) - 1) %>%
    rename(regions = everything())

sf_use_s2(FALSE)
sib4_with_mask_land <- sib4 %>%
    st_join(transcom_mask) %>%
    as_tibble() %>%
    filter(sif_nonzero > 0.01 & assim_nonzero > 0.01) %>%
    mutate(date = clock::as_date(time)) %>%
    select(-c(lon, lat, time)) %>% 
    filter(between(date, as.Date("2016-01-01"), as.Date("2019-01-01"))) %>%
    filter(regions %in% seq(11))


p <- ggplot(data = sib4_with_mask_land) +
    geom_point(aes(x = sif_nonzero, y = assim_nonzero, group = date), alpha = 0.1) +
    facet_wrap(~regions) +
    transition_time(date) +
    labs(
        x = bquote("SIF [" * W ~ m^-2 ~ nm^-1 ~ s^-1 * "]"), 
        y = bquote("GPP [" * mu ~ "mol" ~ C ~ m^-2 ~ s^-1 * "]"),
        title = "Daily SiB4 SIF vs. GPP at 2x2.5-deg res., Date: {frame_time}"
    )
anim <- animate(
    p,
    fps = 10,
    height = 12,
    width = 14,
    res = 300,
    units = "cm"
)
anim_save("sif_vs_gpp_daily_land.gif", path = "./0_eda/figures/")
