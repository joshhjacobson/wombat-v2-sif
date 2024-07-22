library(argparse)
library(Matrix)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented-zonal')
parser$add_argument('--samples-fixresp-wsif')
parser$add_argument('--samples-fixresp-wosif')
parser$add_argument('--samples-freeresp-wsif')
parser$add_argument('--samples-freeresp-wosif')
parser$add_argument('--true-alpha')
parser$add_argument('--region')
parser$add_argument('--output')
args <- parser$parse_known_args()[[1]]

if (!(args$region %in% names(REGION_PLOT_SETTINGS))) {
  stop('Invalid region')
}
plot_region <- REGION_PLOT_SETTINGS[[args$region]]
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
  log_debug('Adjusting bottom-up with true alpha from {args$true_alpha}')
  true_emissions <- true_emissions %>%
    mutate(
      value = value + as.vector(
        X_region[, as.integer(true_alpha$basis_vector)]
        %*% true_alpha$value
      )
    )
}

list_samples <- list(
  list(name = 'With SIF, fixed RLT', path = args$samples_fixresp_wsif),
  list(name = 'Without SIF, fixed RLT', path = args$samples_fixresp_wosif),
  list(name = 'With SIF, free RLT', path = args$samples_freeresp_wsif),
  list(name = 'Without SIF, free RLT', path = args$samples_freeresp_wosif)
)
if (sum(sapply(list_samples, function(x) !is.null(x$path))) < 2) {
  stop('At least two sets of posterior samples are required for a comparison')
}

posterior_emissions <- lapply(list_samples, function(samples_i) {
  if (is.null(samples_i$path)) return(NULL)
  samples <- readRDS(samples_i$path)
  compute_posterior(prior_emissions, X_region, samples, samples_i$name)
}) %>% bind_rows()

emissions <- bind_rows(
  prior_emissions,
  posterior_emissions,
  true_emissions
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
    (inventory == 'bio_resp_tot' & minor_component == 'linear' & grepl('fixed RLT', estimate, fixed = TRUE))
    | (inventory == 'ocean' & minor_component %in% c('linear', 'periodic') & grepl('SIF', estimate, fixed = TRUE))
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
      levels = c(
        'Truth',
        'Bottom-up',
        'Without SIF, fixed RLT',
        'With SIF, fixed RLT',
        'Without SIF, free RLT',
        'With SIF, free RLT'
      )
    )
  )

colour_key <- c(
  'Truth' = 'black',
  'Bottom-up' = 'grey50',
  'Without SIF, fixed RLT' = '#4682b4',
  'With SIF, fixed RLT' = '#fb8b00',
  'Without SIF, free RLT' = '#4682b4',
  'With SIF, free RLT' = '#fb8b00'
)
linetype_key <- c(
  'Truth' = 'solid',
  'Bottom-up' = '11',
  'Without SIF, fixed RLT' = '41',
  'With SIF, fixed RLT' = '41',
  'Without SIF, free RLT' = '41',
  'With SIF, free RLT' = '41'
)

output <- wrap_plots(lapply(sort(unique(emissions$inventory)), function(inventory_i) {
  ggplot(
    emissions %>% filter(
      inventory == inventory_i
    ),
    aes(time)
  ) +
    geom_line(
      mapping = aes(
        y = value,
        colour = estimate,
        linetype = estimate
      ),
      linewidth = 0.4,
      alpha = 0.9
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
    scale_linetype_manual(values = linetype_key) +
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
    legend.position = 'bottom',
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
        hjust = 0.5,
        size = 13
      )
    )
  )

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width,
  height = 13
)
