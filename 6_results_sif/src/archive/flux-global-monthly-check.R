library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(patchwork)

source('partials/display.R')
source('partials/utils.R')

args <- list()
args$perturbations_augmented <- '5_results/intermediates/perturbations-augmented.fst'
args$alpha <- list(
  ALPHAV2 = 'data/wombat-v2-alpha-LNLGIS.fst',
  ALPHAFREE = '4_inversion/intermediates/osse-alpha.fst'
)
args$region <- 'global'
args$output_base <- '6_results_sif/figures'

output_path <- sprintf('%s/fluxes-global-monthly-check.pdf', args$output_base)

if (!(args$region %in% names(REGION_PLOT_SETTINGS))) {
  stop('Invalid region')
}
plot_region <- REGION_PLOT_SETTINGS[[args$region]]
perturbations_base <- fst::read_fst(args$perturbations_augmented)

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

perturbations_region <- perturbations_base %>%
  group_by(inventory_time, basis_vector) %>%
  summarise(value = KG_M2_S_TO_PGC_MONTH * sum(area * value)) %>%
  left_join(
    perturbations_base %>%
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

true_emissions <- bind_rows(lapply(names(args$alpha), function(alpha_i) {
  alpha <- fst::read_fst(args$alpha[[alpha_i]])
  prior_emissions %>%
    mutate(
      output = alpha_i,
      value = value + as.vector(
        X_region[, as.integer(alpha$basis_vector)]
        %*% alpha$value
      )
    )
}))

emissions <- bind_rows(
  prior_emissions,
  true_emissions
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
          .groups = 'drop'
        ) %>%
        mutate(inventory = 'nee'),
      x %>%
        group_by(output, time) %>%
        summarise(
          value = sum(value),
          .groups = 'drop'
        ) %>%
        mutate(inventory = 'total')
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
      linewidth = 0.4,
      alpha = 0.5
    ) +
  scale_colour_manual(values = c('Bottom-up' = 'black', 'ALPHAV2' = 'blue', 'ALPHAFREE' = 'green')) +
  scale_linetype_manual(values = c('Bottom-up' = '41', 'ALPHAV2' = 'dotdash', 'ALPHAFREE' = 'solid')) +
  labs(x = 'Time', y = 'Flux [PgC/month]', colour = NULL, linetype = NULL) +
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
        linewidth = 0.4,
        alpha = 0.5
      ) +
      scale_colour_manual(values = c('Bottom-up' = 'black', 'ALPHAV2' = 'blue', 'ALPHAFREE' = 'green')) +
      scale_linetype_manual(values = c('Bottom-up' = '41', 'ALPHAV2' = 'dotdash', 'ALPHAFREE' = 'solid')) +
      scale_x_date(date_breaks = '6 months', date_labels = '%Y-%m') +
      labs(x = 'Time', y = 'Flux [PgC/month]', colour = NULL, linetype = NULL) +
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
    title = sprintf('Monthly %s fluxes', plot_region$lowercase_title),
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
