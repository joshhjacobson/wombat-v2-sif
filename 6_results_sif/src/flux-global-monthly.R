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
args$perturbations_augmented <- '5_results/intermediates/perturbations-augmented.fst'
args$samples <- '4_inversion/intermediates/samples-free-resp-LNLGISSIF.rds'
args$wombat_v2_alpha <- 'data/wombat-v2-alpha-LNLGIS.fst'
args$region <- 'global'
args$output_base <- '6_results_sif/figures'

observation_groups <- sub('samples-(.*)\\.rds', '\\1', basename(args$samples))
output_path <- sprintf('%s/fluxes-%s-monthly-%s.pdf', args$output_base, args$region, observation_groups)
# osse_case <- sub('osse-samples-(.*)\\.rds', '\\1', basename(args$samples))
# base_case <- strsplit(osse_case, '-', fixed = TRUE)[[1]][1]
# output_path <- sprintf('%s/fluxes-%s-monthly-%s.pdf', args$output_base, args$region, osse_case)
posterior_label <- observation_groups

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
samples <- readRDS(args$samples)
perturbations_base <- fst::read_fst(args$perturbations_augmented)

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

# true_emissions <- prior_emissions %>%
#   mutate(output = 'Truth')

# if (base_case == 'ALPHAV2') {
#   wombat_v2_alpha <- fst::read_fst(args$wombat_v2_alpha)
#   true_emissions <- true_emissions %>%
#     mutate(
#       value = value + as.vector(
#         X_region[, as.integer(wombat_v2_alpha$basis_vector)]
#         %*% wombat_v2_alpha$value
#       )
#     )
# }

posterior_emissions <- prior_emissions %>%
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
  select(-value_prior)

emissions <- bind_rows(
  prior_emissions,
  posterior_emissions
) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        filter(inventory %in% c('bio_assim', 'bio_resp_tot')) %>%
        group_by(output, time) %>%
        summarise(
          value = sum(value),
          value_samples = t(colSums(value_samples)),
          .groups = 'drop'
        ) %>%
        mutate(
          inventory = 'nee',
          value_q025 = matrixStats::rowQuantiles(value_samples, probs = 0.025),
          value_q975 = matrixStats::rowQuantiles(value_samples, probs = 0.975)
        ),
      x %>%
        group_by(output, time) %>%
        summarise(
          value = sum(value),
          value_samples = t(colSums(value_samples)),
          .groups = 'drop'
        ) %>%
        mutate(
          inventory = 'total',
          value_q025 = matrixStats::rowQuantiles(value_samples, probs = 0.025),
          value_q975 = matrixStats::rowQuantiles(value_samples, probs = 0.975)
        )
    )
  } %>%
  mutate(
    inventory = factor(c(
      'bio_assim' = 'GPP',
      'bio_resp_tot' = 'Respiration',
      'nee' = 'NEE',
      'ocean' = 'Ocean',
      'total' = 'Total'
    )[inventory], levels = c(
      'GPP',
      'Respiration',
      'NEE',
      'Ocean',
      'Total'
    ))
  )

layout <- '
#BB
ABB
ABB
#BB
'
output <- ggplot(
  emissions %>%
    filter(inventory == 'Total'),
    aes(time)
) +
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
  scale_colour_manual(values = c('Bottom-up' = 'black', 'Posterior' = '#ff4444')) +
  scale_fill_manual(values = c('Bottom-up' = 'black', 'Posterior' = '#ff4444')) +
  scale_linetype_manual(values = c('Bottom-up' = '41', 'Posterior' = 'solid')) +
  labs(x = 'Time', y = 'Flux [PgC/month]', colour = NULL, fill = NULL, linetype = NULL) +
  guides(fill = 'none') +
  ggtitle('Total natural fluxes')


output <- wrap_plots(
  output, 
  wrap_plots(lapply(head(sort(unique(emissions$inventory)), 4), function(inventory_i) {
    ggplot(
      emissions %>%
        filter(inventory == inventory_i),
      aes(time)
    ) +
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
      scale_colour_manual(values = c('Bottom-up' = 'black', 'Posterior' = '#ff4444')) +
      scale_fill_manual(values = c('Bottom-up' = 'black', 'Posterior' = '#ff4444')) +
      scale_linetype_manual(values = c('Bottom-up' = '41', 'Posterior' = 'solid')) +
      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%m') +
      labs(x = 'Time', y = 'Flux [PgC/month]', colour = NULL, fill = NULL, linetype = NULL) +
      guides(fill = 'none') +
      ggtitle(inventory_i)
  }), ncol = 2, nrow = 2, guides = 'collect'),
  design = layout
) &
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
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(size = 8)
  )

output <- output +
  plot_annotation(
    title = sprintf('Monthly %s fluxes, %s', plot_region$lowercase_title, posterior_label),
    theme = theme(
      plot.title = element_text(
        hjust = if (plot_region$in_supplement) 0.32 else 0.5,
        size = 13
      )
    )
  )

ggsave_base(
  output_path,
  output,
  width = if (plot_region$in_supplement) {
    DISPLAY_SETTINGS$supplement_full_width
  } else {
    DISPLAY_SETTINGS$full_width + 6
  },
  height = if (plot_region$in_supplement) 11.7 else 13
)
