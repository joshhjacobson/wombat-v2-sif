library(colorspace)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(patchwork)


source("partials/display.R")
region_sf <- readRDS('5_results/intermediates/region-sf.rds')
models <- readRDS('3_sif/intermediates/models-data.rds') %>% tidyr::as_tibble()
# models <- fst::read_fst('3_sif/intermediates/model-sif-assim.fst') %>% tidyr::as_tibble()


# p <- ggplot() +
#   geom_raster(data = models, aes(x = longitude, y = latitude, fill = pvalue_poly > 0.001)) +
#   geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
#   coord_sf(
#     crs = sf::st_crs(4326),
#     default_crs = sf::st_crs(4326)
#   ) +
#   scale_fill_discrete(
#     name = "F-stat. P-value > 0.001",
#   ) +
#   labs(x = NULL, y = NULL) +
#   facet_wrap(~month, ncol = 3)
# p
# ggsave_base(
#     "3_sif/figures/modelling/model_pvalue_poly_class.png",
#     p,
#     width = 18,
#     height = 12
# )

# p <- ggplot() +
#   geom_raster(data = models, aes(x = longitude, y = latitude, fill = correlation > 0.5)) +
#   geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
#   coord_sf(
#     crs = sf::st_crs(4326),
#     default_crs = sf::st_crs(4326)
#   ) +
#   scale_fill_discrete(
#     name = "Correlation > 0.5",
#   ) +
#   labs(x = NULL, y = NULL) +
#   facet_wrap(~month, ncol = 3)
# p
# ggsave_base(
#     "3_sif/figures/modelling/model_stong_correlation.png",
#     p,
#     width = 18,
#     height = 12
# )

# p <- ggplot() +
#   geom_raster(data = models, aes(x = longitude, y = latitude, fill = intercept > -0.6)) +
#   geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
#   coord_sf(
#     crs = sf::st_crs(4326),
#     default_crs = sf::st_crs(4326)
#   ) +
#   scale_fill_discrete(
#     name = "Intercept > -0.6",
#   ) +
#   labs(x = NULL, y = NULL) +
#   facet_wrap(~month, ncol = 3)
# p
# ggsave_base(
#     "3_sif/figures/modelling/model_intercept_class.png",
#     p,
#     width = 18,
#     height = 12
# )

# p <- ggplot() +
#   geom_raster(data = models_sub, aes(x = longitude, y = latitude, fill = pvalue_slope < 0.001)) +
#   geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
#   coord_sf(
#     crs = sf::st_crs(4326),
#     default_crs = sf::st_crs(4326)
#   ) +
#   scale_fill_discrete(
#     name = "Slope P-value < 0.05",
#   ) +
#   labs(x = NULL, y = NULL) +
#   facet_wrap(~month, ncol = 3)
# p
# ggsave_base(
#     "3_sif/figures/modelling/model_pvalue_slope_check.png",
#     p,
#     width = 18,
#     height = 12
# )


p <- ggplot() +
  geom_raster(data = models, aes(x = longitude, y = latitude, fill = count)) +
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
    "3_sif/figures/modelling/model_count.png",
    p,
    width = 18,
    height = 12
)


p <- ggplot() +
  geom_raster(data = models, aes(x = longitude, y = latitude, fill = intercept)) +
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
    "3_sif/figures/modelling/model_intercept.png",
    p,
    width = 18,
    height = 12
)

summary(models$slope)
q95 <- quantile(models$slope, probs = 0.95)
p <- ggplot() +
  geom_raster(data = models, aes(x = longitude, y = latitude, fill = slope)) +
  geom_sf(data = region_sf, fill = NA, colour = '#888888', size = 0.1) +
  coord_sf(
    crs = sf::st_crs(4326),
    default_crs = sf::st_crs(4326)
  ) +
  scale_fill_continuous_sequential(
    name = "Slope",
    palette = "BluYl",
    limits = c(0, q95),
    na.value = "red"
  ) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~month, ncol = 3)
p
ggsave_base(
    "3_sif/figures/modelling/model_slope_with_lims.png",
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
    "Correlation: ",
    round(row$correlation, 2),
    ", F-stat. P-value: ",
    formatC(row$pvalue_poly, format = "e", digits = 1),
    # ", F Statistic: ",
    # round(row$f_stat, 2),
    "\nIntercept: ",
    round(row$intercept, 2),
    ", Slope: ",
    formatC(row$slope, format = "e", digits = 1)
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
    "3_sif/figures/modelling/",
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



set.seed(18)
model_sample <- models %>% sample_n(30)

for (i in seq_len(nrow(model_sample))) {
  plot_scatter_location(model_sample, i, "regular_slope")
}