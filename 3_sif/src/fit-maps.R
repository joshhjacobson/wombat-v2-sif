library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(rnaturalearth)
library(rnaturalearthdata)

library(patchwork)
library(colorspace)


source("partials/display.R")
region_sf <- readRDS('4_results/intermediates/region-sf.rds')
models <- fst::read_fst('3_sif/intermediates/model-sif-gpp.fst')

models <- models %>% tidyr::as_tibble()

p <- ggplot() +
  geom_raster(data = models, aes(x = longitude, y = latitude, fill = pvalue_slope)) +
  geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
  coord_sf(
    crs = sf::st_crs(4326),
    default_crs = sf::st_crs(4326)
  ) +
  scale_fill_continuous_sequential(
    name = "Slope P-value",
    palette = "BluYl"
  ) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~month, ncol = 3)
p
ggsave_base(
    "3_sif/figures/model_pvalue.png",
    p,
    width = 18,
    height = 12
)

summary(models$slope)

h <- ggplot() +
  geom_histogram(data = models, aes(x = slope))
h
b <- ggplot() +
  geom_boxplot(data = models, aes(y = slope))
b
ggsave_base(
    "3_sif/figures/model_slope_box.png",
    b,
    width = 9,
    height = 12
)


q95 <- quantile(models$slope, probs = 0.95)
models_sub <- models %>% filter(
  pvalue_slope < 0.05,
  between(slope, 0, q95)
)

h <- ggplot() +
  geom_histogram(data = models_sub, aes(x = slope))
h
b <- ggplot() +
  geom_boxplot(data = models_sub, aes(y = slope))
b
ggsave_base(
    "3_sif/figures/model_slope_trim_dist.png",
    h + b,
    width = 14,
    height = 9
)


p <- ggplot() +
  geom_raster(data = models_sub, aes(x = longitude, y = latitude, fill = count)) +
  geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
  coord_sf(
    crs = sf::st_crs(4326),
    default_crs = sf::st_crs(4326)
  ) +
  scale_fill_continuous_sequential(
    name = "Count",
    palette = "BluYl"
  ) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~month, ncol = 3)
p
ggsave_base(
    "3_sif/figures/model_count.png",
    p,
    width = 18,
    height = 12
)


p <- ggplot() +
  geom_raster(data = models_sub, aes(x = longitude, y = latitude, fill = intercept)) +
  geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
  coord_sf(
    crs = sf::st_crs(4326),
    default_crs = sf::st_crs(4326)
  ) +
  scale_fill_continuous_divergingx(
    name = "Intercept",
    palette = "RdYlBu",
    rev = TRUE,
    mid = 0
  ) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~month, ncol = 3)
p
ggsave_base(
    "3_sif/figures/model_intercept.png",
    p,
    width = 18,
    height = 12
)

p <- ggplot() +
  geom_raster(data = models_sub, aes(x = longitude, y = latitude, fill = slope)) +
  geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
  coord_sf(
    crs = sf::st_crs(4326),
    default_crs = sf::st_crs(4326)
  ) +
  scale_fill_continuous_sequential(
    name = "Slope",
    palette = "BluYl"
  ) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~month, ncol = 3)
p
ggsave_base(
    "3_sif/figures/model_slope.png",
    p,
    width = 18,
    height = 12
)
