library(ncdf4)
library(stars)
library(clock)
library(dplyr)
library(ggplot2)

source("partials/display.R")

TIME_RESOLUTION <- "monthly"

# Read in data and combine with area weights
path <- paste0("data/eda/sib4/", TIME_RESOLUTION, "/")
file_list <- list.files(path, full.names = TRUE)

sib4 <- read_stars(file_list, sub = c("sif_nonzero", "assim_nonzero"), along = "time") %>%
  st_set_dimensions(names = c("lon", "lat", "time")) %>%
  st_set_crs(4326)


# Compute means for each latitudinal band
sib4_bands <- sib4 %>%
  st_apply(c("lat", "time"), mean, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(month = get_month(time)) %>%
  mutate(year = get_year(time)) %>%
  select(-time) %>%
  group_by(lat, year, month) %>%
  summarise(
    sif = mean(sif_nonzero, na.rm = TRUE),
    assim = mean(assim_nonzero, na.rm = TRUE),
  ) %>%
  na.omit() %>%
  group_by(month) %>%
  mutate(scale_factor = max(assim) / max(sif)) %>%
  ungroup()


# Combine and standardize
# lat_bands <- full_join(sif_bands, assim_bands) %>%
#   mutate(
#     sif_std = (sif - mean(sif)) / sd(sif),
#     assim_std = (assim - mean(assim)) / sd(assim),
#   ) %>%
#   select(lat, month, sif_std, assim_std)


# sif_color <- "#fb8b00"
# gpp_color <- "#018571"

gpp_color <- "#026358"
sif_color <- "#d6490c"
scale_factor_med <- median(sib4_bands$scale_factor)

# Plot lat means by month
p <- ggplot(sib4_bands, aes(x = lat)) +
  geom_line(aes(y = sif, group = as.factor(year)), alpha = 0.2, color = sif_color) +
  geom_line(
    aes(y = assim / scale_factor_med, group = as.factor(year)),
    alpha = 0.2,
    color = gpp_color
  ) +
  scale_y_continuous(
    name = bquote("SIF [" * W ~ m^-2 ~ nm^-1 ~ s^-1 * "]"),
    sec.axis = sec_axis(
      trans = ~ . * scale_factor_med,
      name = bquote("GPP [" * mu ~ "mol" ~ C ~ m^-2 ~ s^-1 * "]")
    )
  ) +
  coord_flip() +
  facet_wrap(~month, ncol=6) +
  # theme(
  #   axis.title.y.left = element_text(color = sif_color),
  #   axis.text.y.left = element_text(color = sif_color),
  #   axis.title.y.right = element_text(color = gpp_color),
  #   axis.text.y.right = element_text(color = gpp_color)
  # ) +
  theme(
    axis.title.x.bottom = element_text(color = sif_color),
    axis.text.x.bottom = element_text(color = sif_color),
    axis.title.x.top = element_text(color = gpp_color),
    axis.text.x.top = element_text(color = gpp_color),
    strip.placement = "outside"
  ) +
  labs(
    x = "Latitude",
    title = "Latitudinal Profile of Monthly Averages with Repetition Across Years"
  )
p

ggsave_base(
  "0_eda/figures/lat_mean_by_month_updated.png",
  p,
  width = 18,
  height = 14,
)
