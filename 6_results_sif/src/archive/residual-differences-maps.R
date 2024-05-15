library(argparse)
library(Matrix)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)

# For a given year, plot matrix with GPP, resp, and nee in rows, and months in columns

# source(Sys.getenv('UTILS_PARTIAL'))
# source(Sys.getenv('DISPLAY_PARTIAL'))
source('partials/display.R')
source('partials/utils.R')

# parser <- ArgumentParser()
# parser$add_argument('--perturbations-augmented')
# parser$add_argument('--samples')
# parser$add_argument('--output')
# args <- parser$parse_args()

args <- list()
args$control_emmissions <- '4_inversion/intermediates/control-emissions.fst'
args$perturbations_augmented <- '5_results/intermediates/perturbations-augmented.fst'
args$samples_pair <- c(
  '4_inversion/intermediates/samples-LNLGIS.rds',
  '4_inversion/intermediates/samples-LNLGISSIF.rds'
)
args$year <- 2016
# args$region_sf <- '5_results/intermediates/region-sf.rds'
args$output_base <- '6_results_sif/figures'

output_path <- sprintf('%s/residual-differences-maps-%s.pdf', args$output_base, args$year)

# region_sf <- readRDS(args$region_sf)
control_emissions <- fst::read_fst(args$control_emmissions)
samples <- lapply(args$samples_pair, readRDS)
perturbations_base <- fst::read_fst(args$perturbations_augmented)

perturbations_base <- perturbations_base %>%
  filter(
    inventory != 'ocean',
    component == 'residual',
    lubridate::year(time) == args$year
  ) %>%
  mutate(value = PER_SECONDS_TO_PER_YEAR * value)

residual_parts <- bind_rows(lapply(seq_along(samples), function(i) {
  perturbations_base %>%
    left_join(
      samples[[i]]$alpha_df %>%
        select(basis_vector, alpha = value),
      by = 'basis_vector'
    ) %>%
    mutate(
      sample_set = sprintf('set%02d', i),
      value = (1 + alpha) * value
    )
})) 

residual <- bind_rows(
  residual_parts,
  residual_parts %>%
    group_by(sample_set, longitude, latitude, time) %>%
    summarise(
      value = sum(value),
      .groups = 'drop'
    ) %>%
    mutate(inventory = 'nee')
) %>%
  mutate(
    value = if_else(sample_set == 'set01', value, -value)
  ) %>%
  group_by(inventory, longitude, latitude, time) %>%
  summarise(
    value = sum(value),
    .groups = 'drop'
  ) %>%
  mutate(
    inventory = factor(c(
      'bio_assim' = 'GPP',
      'bio_resp_tot' = 'Respiration',
      'nee' = 'NEE'
    )[inventory], c(
      'GPP',
      'Respiration',
      'NEE'
    )),
    time = lubridate::month(time, label = TRUE, abbr = TRUE)
  )

flux_breaks <- seq(-1, 1, by = 0.25)
flux_limits <- c(-1, 1)

# residual_sf <- residual %>%
#   group_by(inventory, time) %>%
#   group_map(~ {
#     control_emissions %>%
#       distinct(longitude, latitude) %>%
#       arrange(longitude, latitude) %>%
#       filter(abs(latitude) != 89.5) %>%
#       left_join(.x, by = c('longitude', 'latitude')) %>%
#       mutate(
#         value = discretise_by_breaks(value, flux_breaks, flux_limits)
#       ) %>%
#       grid_df_to_sf('value') %>%
#       mutate(
#         inventory = .y$inventory
#       )
#   }) %>%
#   bind_rows()

output <- ggplot(residual, aes(longitude, latitude, fill = value)) +
  geom_tile() +
  scale_fill_fermenter_n(
    breaks = flux_breaks,
    palette = 'RdYlBu',
    direction = -1,
    n_colours = 10,
    limits = flux_limits,
    labels = sprintf(paste0('%.', 2, 'f'), flux_breaks),
    guide = guide_coloursteps(
      title.position = 'top',
      title.hjust = 0.5,
      axis = FALSE,
      label.theme = element_text(size = 8),
      frame.colour = '#999999',
      barwidth = 13,
      even.steps = FALSE
    ),
    na.value = '#cccccc'
  ) +
  facet_grid(inventory ~ time) +
  labs(
    fill = expression('Flux difference [kgCO'[2]*'/'*m^2*'/year]'),
    x = NULL,
    y = NULL
  ) +
  ggtitle(sprintf('Difference in residual flux (LNLGIS - LNLGISSIF), %s', args$year)) +
  theme_minimal() +
  theme(
    legend.position = 'bottom',
    plot.title = element_text(size = 13, hjust = 0.5),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# output <- plot_map(
#   residual_sf,
#   value,
#   flux_breaks,
#   flux_limits,
#   show_excess = FALSE,
#   label_precision = 1,
#   bar_width = 16,
#   drop_second_labels = TRUE
# ) +
#   facet_grid(inventory ~ time) +
#   labs(
#     fill = expression('Flux [kgCO'[2]*'/'*m^2*'/year]')
#   ) +
#   ggtitle('Residual flux differences by month') +
#   theme(
#     legend.position = 'bottom',
#     plot.title = element_text(size = 13, hjust = 0.5),
#     panel.border = element_blank(),
#     panel.grid = element_blank(),
#     strip.text = element_text(size = 12)
#   )

ggsave_base(
  output_path,
  output,
  width = 60,
  height = 20
)
