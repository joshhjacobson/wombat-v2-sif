library(argparse)
library(Matrix)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented-zonal')
parser$add_argument('--samples-wsif')
parser$add_argument('--samples-wosif')
parser$add_argument('--true-alpha')
parser$add_argument('--region')
parser$add_argument('--output')
args <- parser$parse_args()

if (!(args$region %in% names(REGION_PLOT_SETTINGS))) {
  stop('Invalid region')
}
plot_region <- REGION_PLOT_SETTINGS[[args$region]]
samples_wsif <- readRDS(args$samples_wsif)
samples_wosif <- readRDS(args$samples_wosif)
true_alpha <- if (!is.null(args$true_alpha)) fst::read_fst(args$true_alpha) else NULL
perturbations_zonal <- fst::read_fst(args$perturbations_augmented_zonal)

perturbations_zonal <- perturbations_zonal %>%
  mutate(
    minor_component = factor(
      case_when(
        component == 'residual' ~ 'residual',
        component %in% c('intercept', 'trend') ~ 'linear',
        TRUE ~ 'periodic'
      ),
      c('linear', 'periodic', 'residual')
    ),
    inventory_minor_component_time = interaction(
      inventory,
      minor_component,
      time,
      drop = TRUE
    )
  )

perturbations_region <- perturbations_zonal %>%
  filter(
    latitude_bottom >= plot_region$latitude_lower,
    latitude_bottom < plot_region$latitude_upper
  ) %>%
  group_by(inventory_minor_component_time, basis_vector) %>%
  summarise(
    value = KG_M2_S_TO_PGC_MONTH * sum(area * value),
    .groups = 'drop'
  ) %>%
  left_join(
    perturbations_zonal %>%
      distinct(inventory_minor_component_time, inventory, minor_component, time),
    by = 'inventory_minor_component_time'
  )

X_region <- with(perturbations_region, sparseMatrix(
  i = as.integer(inventory_minor_component_time),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(inventory_minor_component_time), nlevels(basis_vector))
))

prior_emissions <- perturbations_region %>%
  group_by(inventory_minor_component_time, inventory, minor_component, time) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  select(-inventory_minor_component_time) %>%
  mutate(estimate = 'Bottom-up')

true_emissions <- prior_emissions %>%
  mutate(estimate = 'Truth')

if (!is.null(true_alpha)) {
  true_emissions <- true_emissions %>%
    mutate(
      value = value + as.vector(
        X_region[, as.integer(true_alpha$basis_vector)]
        %*% true_alpha$value
      )
    )
}

posterior_emissions_wosif <- compute_posterior(prior_emissions, X_region, samples_wosif, 'Without SIF')
posterior_emissions_wsif <- compute_posterior(prior_emissions, X_region, samples_wsif, 'With SIF')

emissions <- bind_rows(
  true_emissions,
  posterior_emissions_wsif,
  posterior_emissions_wosif
) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        filter(inventory %in% c('bio_assim', 'bio_resp_tot')) %>%
        group_by(estimate, time, minor_component) %>%
        summarise(
          value = sum(value),
          value_samples = t(colSums(value_samples)),
          .groups = 'drop'
        ) %>%
        mutate(
          inventory = 'nee',
          value_q025 = matrixStats::rowQuantiles(value_samples, probs = 0.025),
          value_q975 = matrixStats::rowQuantiles(value_samples, probs = 0.975)
        )
    )
  } %>%
  filter(!(
    inventory == 'ocean' & minor_component %in% c('linear', 'periodic') & grepl('SIF', estimate, fixed = TRUE)
  )) %>%
  mutate(
    inventory = factor(c(
      'bio_assim' = 'GPP',
      'bio_resp_tot' = 'Respiration',
      'nee' = 'NEE',
      'ocean' = 'Ocean'
    )[inventory], levels = c(
      'GPP',
      'Respiration',
      'NEE',
      'Ocean'
    )),
    minor_component = factor(c(
      'linear' = 'Linear',
      'periodic' = 'Seasonal',
      'residual' = 'Residual'
    )[as.character(minor_component)], levels = c(
      'Linear',
      'Seasonal',
      'Residual'
    )),
    estimate = factor(
      estimate, 
      levels = c('Truth', 'Without SIF', 'With SIF'))
  )

colour_key <- c('Truth' = '#4053d3', 'Without SIF' = 'grey50', 'With SIF' = '#ebac23')

output <- wrap_plots(lapply(sort(unique(emissions$inventory)), function(inventory_i) {
  ggplot(
    emissions %>% filter(inventory == inventory_i),
    aes(time)
  ) +
    geom_line(
      mapping = aes(
        y = value,
        colour = estimate,
        linetype = estimate
      ),
      linewidth = 0.4
    ) +
    geom_ribbon(
      mapping = aes(
        ymin = value_q025,
        ymax = value_q975,
        fill = estimate
      ),
      alpha = 0.3
    ) +
    facet_grid(minor_component ~ ., scales = 'free_y') +
    scale_x_date(date_labels = '%Y-%m') +
    scale_colour_manual(values = colour_key) +
    scale_fill_manual(values = colour_key) +
    scale_linetype_manual(values = c('41', 'solid', 'solid')) +
    labs(x = 'Time', y = 'Flux [PgC per month]', colour = NULL, fill = NULL, linetype = NULL) +
    guides(fill = 'none') +
    ggtitle(inventory_i)
}), ncol = 2, nrow = 2, guides = 'collect') &
  theme(
    plot.margin = margin(t = 1, r = 0, b = 0, l = 1, unit = 'mm'),
    plot.title = element_text(
      size = 11,
      margin = margin(0, 0, 5.5, 0, unit = 'points')
    ),
    legend.position = if (plot_region$in_supplement) {
      'right'
    } else {
      'bottom'
    },
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm'),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(size = 8)
  )

output <- output +
  plot_annotation(
    title = sprintf('Decomposition of %s fluxes', plot_region$lowercase_title),
    theme = theme(
      plot.title = element_text(
        hjust = if (plot_region$in_supplement) 0.32 else 0.5,
        size = 13
      )
    )
  )

ggsave_base(
  args$output,
  output,
  width = if (plot_region$in_supplement) {
    DISPLAY_SETTINGS$supplement_full_width
  } else {
    DISPLAY_SETTINGS$full_width
  },
  height = if (plot_region$in_supplement) 11.7 else 13
)
