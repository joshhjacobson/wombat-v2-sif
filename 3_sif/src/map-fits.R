library(colorspace)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(patchwork)


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
b <- ggplot() +
  geom_boxplot(data = models, aes(y = slope))
b
ggsave_base(
    "3_sif/figures/model_slope_box.png",
    b,
    width = 9,
    height = 12
)


p <- ggplot() +
  geom_raster(data = models, aes(x = longitude, y = latitude, fill = slope)) +
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
    "3_sif/figures/model_slope_all.png",
    p,
    width = 18,
    height = 12
)

quantiles <- quantile(models$slope, probs = c(0.05, 0.95))

p <- models %>% filter(slope > quantiles[2]) %>% 
  ggplot() +
  geom_raster(aes(x = longitude, y = latitude, fill = slope)) +
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
ggsave_base(
  "3_sif/figures/model_slope_extreme.png",
  p,
  width = 18,
  height = 12
)

p <- models %>% 
  filter(slope < 0) %>% 
  ggplot() +
    geom_raster(aes(x = longitude, y = latitude, fill = slope)) +
    geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
    coord_sf(
      crs = sf::st_crs(4326),
      default_crs = sf::st_crs(4326)
    ) +
    scale_fill_continuous_sequential(
      name = "Slope",
      palette = "YlOrRd",
    ) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~month, ncol = 3)
p
ggsave_base(
  "3_sif/figures/model_slope_negative.png",
  p,
  width = 18,
  height = 12
)


q95 <- quantile(models$slope, probs = 0.95)
models_sub <- models %>% 
  filter(between(slope, 0, q95)) %>%
  mutate(slope = if_else(pvalue_slope < 0.05, slope, 0))

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





plot_scatter_location <- function(nested_data, i, name) {
  row <- nested_data[i, ] 
  data <- row %>% select(data) %>% tidyr::unnest(cols = c(data))
  p_location <- ggplot() +
    geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
    geom_point(
      data = row,
      aes(x = longitude, y = latitude),
      color = "red",
      size = 3
    ) +
    labs(x = element_blank(), y = element_blank(), title = paste0("Month: ", row$month))

  specs <- paste0(
    "Intercept: ",
    round(row$intercept, 2),
    ", Slope: ",
    formatC(row$slope, format = "e", digits = 2)
  )
  p_data <- ggplot() +
    geom_point(
      data = data,
      aes(x = assim, y = sif), 
      alpha = 0.5,
      shape = 1
    ) +
    geom_abline(
      data = row,
      aes(intercept = intercept, slope = slope),
      color = "blue",
      linewidth = 1
    ) + 
    expand_limits(x = 0, y = 0) +
    labs(x = "GPP", y = "SIF", title = specs)

  fname <- paste0(
    "3_sif/figures/temp/",
    name,
    "/scatter_",
    i
  )
  ggsave(
    paste0(fname, ".png"),
    p_data + p_location,
    width = 8,
    height = 3,
  )
}


df <- readRDS('3_sif/intermediates/models-data.rds') %>% tidyr::as_tibble()

set.seed(18)

# Regular slopes
q95 <- quantile(df$slope, probs = 0.95)
df_reg <- df %>% 
  filter(
    pvalue_slope < 0.05,
    between(slope, 0, q95)
  ) %>% 
  sample_n(30)

for (i in seq_len(nrow(df_reg))) {
  plot_scatter_location(df_reg, i, "regular_slope")
}

# Extreme slopes
df_large <- df %>% 
  filter(slope > q95, pvalue_slope < 0.05) %>% 
  sample_n(30)

for (i in seq_len(nrow(df_large))) {
  plot_scatter_location(df_large, i, "extreme_slope")
}

# Negative slopes
df_neg <- df %>% 
  filter(slope < 0, pvalue_slope < 0.05) %>% 
  sample_n(30)

for (i in seq_len(nrow(df_neg))) {
  plot_scatter_location(df_neg, i, "negative_slope")
}
