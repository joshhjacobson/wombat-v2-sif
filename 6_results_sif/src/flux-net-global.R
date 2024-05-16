library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(patchwork)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--perturbations-augmented')
parser$add_argument('--samples-LNLGIS')
parser$add_argument('--samples-LNLGISSIF')
parser$add_argument('--output')
args <- parser$parse_args()

perturbations_base <- fst::read_fst(args$perturbations_augmented)
samples_LNLGIS <- readRDS(args$samples_LNLGIS)
samples_LNLGISSIF <- readRDS(args$samples_LNLGISSIF)

# TODO: remove once we have full LNLGISSIF samples
samples_LNLGIS$alpha_df$value_samples <- samples_LNLGIS$alpha_df$value_samples[
  , 
  1:ncol(samples_LNLGISSIF$alpha_df$value_samples)
]

study_start <- as.Date('2015-01-01')
study_end <- as.Date('2021-01-01')

perturbations_base <- perturbations_base %>%
  filter(
    between(time, study_start, study_end)
  ) %>%
  mutate(
    inventory_time = factor(
      as.character(inventory_time),
      as.character(unique(inventory_time))
    )
  )

perturbations <- perturbations_base %>%
  group_by(inventory_time, basis_vector) %>%
  summarise(
    value = KG_M2_S_TO_PGC_MONTH * sum(area * value),
    .groups = 'drop'
  ) %>%
  left_join(
    perturbations_base %>%
      distinct(inventory_time, inventory, time),
    by = 'inventory_time'
  )

X_global <- with(perturbations, sparseMatrix(
  i = as.integer(inventory_time),
  j = as.integer(basis_vector),
  x = value,
  dims = c(nlevels(inventory_time), nlevels(basis_vector))
))

prior_emissions <- perturbations %>%
  group_by(inventory_time, inventory, time) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  select(-inventory_time) %>%
  mutate(name = 'Bottom-up')

compute_posterior <- function(samples, posterior_name) {
  prior_emissions %>%
    mutate(
      name = posterior_name,
      value_prior = value,
      value = value_prior + as.vector(
        X_global[, as.integer(samples$alpha_df$basis_vector)]
        %*% samples$alpha_df$value
      ),
      value_samples = value_prior + as.matrix(
        X_global[, as.integer(samples$alpha_df$basis_vector)]
        %*% samples$alpha_df$value_samples
      ),
      value_q025 = matrixStats::rowQuantiles(value_samples, probs = 0.025),
      value_q975 = matrixStats::rowQuantiles(value_samples, probs = 0.975)
    ) %>%
    select(-value_prior)
}

posterior_emissions_LNLGIS <- compute_posterior(samples_LNLGIS, 'Without SIF')
posterior_emissions_LNLGISSIF <- compute_posterior(samples_LNLGISSIF, 'With SIF')

emissions <- bind_rows(
  prior_emissions,
  posterior_emissions_LNLGIS,
  posterior_emissions_LNLGISSIF
) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        filter(inventory %in% c('bio_assim', 'bio_resp_tot')) %>%
        group_by(name, time) %>%
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
    name = factor(
      name,
      levels = c('Bottom-up', 'Without SIF', 'With SIF')
    )
  )

colour_key <- c('Bottom-up' = 'black', 'Without SIF' = 'grey50', 'With SIF' = '#fb8b00')
linetype_key <- c('Bottom-up' = '41', 'Without SIF' = 'solid', 'With SIF' = 'solid')

output <- emissions %>%
  filter(
    inventory %in% c('GPP', 'Respiration', 'NEE')
  ) %>%
  ggplot(aes(x = time)) +
  geom_line(
    mapping = aes(
      y = value,
      colour = name,
      linetype = name
    ),
    linewidth = 0.4
  ) +
  geom_ribbon(
    mapping = aes(
      ymin = value_q025,
      ymax = value_q975,
      fill = name
    ),
    alpha = 0.3
  ) +
  facet_wrap(~inventory, scales = 'free_y') +
  scale_colour_manual(values = colour_key) +
  scale_fill_manual(values = colour_key) +
  scale_linetype_manual(values = linetype_key) +
  guides(fill = 'none') +
  labs(x = 'Time', y = 'Flux [PgC per month]', colour = NULL, fill = NULL, linetype = NULL) +
  ggtitle(
    'Monthly global fluxes'
  ) +
  theme(
    plot.margin = margin(t = 1, r = 1, b = 0, l = 1, unit = 'mm'),
    plot.title = element_text(size = 13, hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 10),
    strip.text = element_text(size = 11),
    legend.position = 'bottom',
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm')
  )

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$supplement_full_width,
  height = 6.9
)
