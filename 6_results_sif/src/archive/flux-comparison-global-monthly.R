library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(patchwork)

# source(Sys.getenv('UTILS_PARTIAL'))
# source(Sys.getenv('DISPLAY_PARTIAL'))
source('partials/display.R')
source('partials/utils.R')

# parser <- ArgumentParser()
# parser$add_argument('--area-1x1')
# parser$add_argument('--perturbations-augmented')
# parser$add_argument('--samples')
# parser$add_argument('--region')
# parser$add_argument('--output')
# args <- parser$parse_args()

args <- list()
args$area_1x1 <- 'data/area-1x1.nc'
args$observations <- '4_inversion/intermediates/observations.fst'
args$perturbations_augmented <- '5_results/intermediates/perturbations-augmented.fst'
args$samples_pair <- c(
  '4_inversion/intermediates/samples-LNLGISSIF.rds',
  '4_inversion/intermediates/samples-free-resp-LNLGISSIF.rds'
)
args$region <- 'global'
args$output_base <- '6_results_sif/figures'

output_path <- sprintf('%s/flux-comparison-free-resp-%s-monthly.pdf', args$output_base, args$region)

with_nc_file(list(fn = args$area_1x1), {
  longitude_area <- as.vector(ncdf4::ncvar_get(fn, 'lon'))
  latitude_area <- as.vector(ncdf4::ncvar_get(fn, 'lat'))
  area <- ncdf4::ncvar_get(fn, 'cell_area')
  area_1x1 <- expand.grid(
    longitude = longitude_area,
    latitude = latitude_area
  ) %>%
    mutate(area = as.vector(area))
})

if (!(args$region %in% names(REGION_PLOT_SETTINGS))) {
  stop('Invalid region')
}
plot_region <- REGION_PLOT_SETTINGS[[args$region]]
perturbations_base <- fst::read_fst(args$perturbations_augmented)
observations <- fst::read_fst(args$observations)

monthly_sif_part <- observations %>%
  filter(
    observation_group == '3_SIF',
    assimilate == 1,
    time >= '2015-01-01',
    time < '2021-01-01'
  ) %>%
  mutate(
    time = strftime(time, format = '%Y-%m')
  ) %>%
  group_by(time) %>%
  summarise(
    value = mean(value),
    .groups = 'drop'
  ) %>%
  mutate(
    output = 'OCO-2 SIF (scaled)'
  )

monthly_sif <- bind_rows(
  monthly_sif_part %>% mutate(
    inventory = 'bio_assim',
    value = value * -25
  ),
  monthly_sif_part %>% mutate(
    inventory = 'bio_resp_tot',
    value = value * -25 + 20
  )
)

area_495 <- (area_1x1 %>% filter(latitude == 49.5) %>% pull(area))[1]
area_505 <- (area_1x1 %>% filter(latitude == 50.5) %>% pull(area))[1]
area_both <- area_495 + area_505

perturbations_base <- perturbations_base %>%
  filter(
    time >= '2015-01-01',
    time < '2021-01-01'
  ) %>%
  mutate(
    inventory_time = interaction(
      inventory,
      time,
      drop = TRUE
    )
  )

# Splits grid cells that cross boundaries
perturbations_split <- bind_rows(
  perturbations_base %>%
    filter(latitude != 0),
  perturbations_base %>%
    filter(latitude == 0) %>%
    mutate(
      latitude = -0.5,
      latitude_bottom = -1,
      area = area / 2
    ),
  perturbations_base %>%
    filter(latitude == 0) %>%
    mutate(
      latitude = 0.5,
      latitude_bottom = 0,
      area = area / 2
    ),
  perturbations_base %>%
    filter(latitude == 50) %>%
    mutate(
      latitude = 49.5,
      latitude_bottom = 49,
      area = area * area_495 / area_both
    ),
  perturbations_base %>%
    filter(latitude == 50) %>%
    mutate(
      latitude = 50.5,
      latitude_bottom = 50,
      area = area * area_505 / area_both
    )
)

perturbations_region <- perturbations_split %>%
  filter(
    latitude_bottom >= plot_region$latitude_lower,
    latitude_bottom < plot_region$latitude_upper
  ) %>%
  group_by(inventory_time, basis_vector) %>%
  summarise(value = KG_M2_S_TO_PGC_MONTH * sum(area * value)) %>%
  left_join(
    perturbations_split %>%
      distinct(inventory_time, inventory, time),
    by = 'inventory_time'
  )


X_region <- with(perturbations_region, sparseMatrix(
  i = as.integer(inventory_time),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(inventory_time), nlevels(basis_vector))
))

prior_emissions <- perturbations_region %>%
  group_by(inventory_time, inventory, time) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  select(-inventory_time) %>%
  mutate(output = 'Bottom-up')

compute_posterior <- function(samples_path) {
  samples <- readRDS(samples_path)
  prior_emissions %>%
    mutate(
      output = 'Posterior',
      value_prior = value,
      value = value_prior + as.vector(
        X_region[, as.integer(samples$alpha_df$basis_vector)]
        %*% samples$alpha_df$value
      ),
      value_samples = value_prior + as.matrix(
        X_region[, as.integer(samples$alpha_df$basis_vector)]
        %*% samples$alpha_df$value_samples
      ),
      value_q025 = matrixStats::rowQuantiles(value_samples, probs = 0.025),
      value_q975 = matrixStats::rowQuantiles(value_samples, probs = 0.975)
    ) %>%
    select(-c(value_prior, value_samples))
}

emissions <- bind_rows(lapply(args$samples_pair, function(samples_path) {
  samples_type <- sub('samples-(.*)\\.rds', '\\1', basename(samples_path))
  compute_posterior(samples_path) %>%
    bind_rows(
      prior_emissions
    )  %>%
    filter(
      inventory != 'ocean'
    ) %>%
    mutate(
      time = strftime(time, format = '%Y-%m')
    )  %>% 
    bind_rows(
      monthly_sif
    ) %>%
    mutate(
      inventory = factor(c(
        'bio_assim' = 'GPP',
        'bio_resp_tot' = 'Respiration'
      )[inventory], levels = c(
        'GPP',
        'Respiration'
      )),
      time = as.Date(paste0(time, '-01')),
      samples_type = samples_type
    )
}))


output <- ggplot(emissions, aes(time)) +
  geom_line(
    mapping = aes(
      y = value,
      colour = output,
      linetype = output
    ),
    linewidth = 0.4
  ) +
  geom_ribbon(
    mapping = aes(
      ymin = value_q025,
      ymax = value_q975,
      fill = output
    ),
    alpha = 0.3
  ) +
  scale_colour_manual(values = c('black', '#d95f02', '#3B5EDA')) +
  scale_fill_manual(values = c('black', '#d95f02', '#3B5EDA')) +
  scale_linetype_manual(values = c('41', 'twodash', 'solid')) +
  labs(x = 'Time', y = 'Flux [PgC/month]', colour = NULL, fill = NULL, linetype = NULL) +
  guides(fill = 'none') +
  facet_grid(inventory ~ samples_type, scales = 'free_y') +
  theme(
    plot.margin = margin(t = 1, r = 1, b = 0, l = 1, unit = 'mm'),
    plot.title = element_text(
      size = 11,
      margin = margin(0, 0, 5.5, 0, unit = 'points')
    ),
    legend.position = if (plot_region$in_supplement) {
      'right'
    } else {
      'bottom'
    },
    legend.margin = margin(t = 0, r = 5, b = 0, l = 0, unit = 'mm'),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(size = 8)
  ) +
  plot_annotation(
    title = 'Monthly global fluxes',
    theme = theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 13
      )
    )
  )

ggsave_base(
  output_path,
  output,
  width = 16,
  height = 13
)
