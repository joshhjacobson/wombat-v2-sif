library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(patchwork)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--six-year-average')
parser$add_argument('--flux-component')
parser$add_argument('--region-sf')
parser$add_argument('--output')
args <- parser$parse_args()

convert_to_sf <- function(
  df,
  mean_breaks,
  mean_limits,
  sd_breaks,
  sd_limits,
  mad_breaks = NULL,
  mad_limits = NULL
) {
  list(
    mean = df %>%
      group_by(estimate) %>%
      group_map(~ {
        .x %>%
          mutate(
            value = discretise_by_breaks(value_mean, mean_breaks, mean_limits)
          ) %>%
          grid_df_to_sf('value') %>%
          mutate(
            estimate = .y$estimate
          )
      }) %>%
      bind_rows(),
    sd = df %>%
      group_by(estimate) %>%
      group_map(~ {
        .x %>%
          mutate(
            value = discretise_by_breaks(value_scale, sd_breaks, sd_limits)
          ) %>%
          grid_df_to_sf('value') %>%
          mutate(
            estimate = .y$estimate
          )
      }) %>%
      bind_rows(),
    mad = if (!is.null(mad_breaks) & !is.null(mad_limits)) {
      df %>%
        filter(estimate == 'FLUXCOM') %>%
        mutate(
          value = discretise_by_breaks(value_scale, mad_breaks, mad_limits)
        ) %>%
        grid_df_to_sf('value') %>%
        mutate(
          estimate = 'FLUXCOM'
        )
    } else NULL
  )
}

flux_key <- list(
  'gpp' = list(
    name = 'bio_assim',
    label = 'GPP',
    label_precision = 0,
    drop_second_labels = FALSE,
    palette = 'BluYl',
    reverse = FALSE,
    symmetric = FALSE,
    mean_breaks = round(seq(-12, 0, by = 2), 1),
    mean_limits = c(-14, 0),
    diff_breaks = round(seq(-8, 8, by = 2), 1),
    diff_limits = c(-10, 10),
    sd_breaks = round(seq(0, 0.25, by = 0.05), 3),
    sd_limits = c(0, 0.25 + 1.5 * 0.05),
    mad_breaks = round(seq(0, 0.6, by = 0.15), 3),
    mad_limits = c(0, 0.75)
  ),
  'resp' = list(
    name = 'bio_resp_tot',
    label = 'respiration',
    label_precision = 0,
    drop_second_labels = FALSE,
    palette = 'Magenta',
    reverse = TRUE,
    symmetric = FALSE,
    mean_breaks = round(seq(0, 12, by = 2), 1),
    mean_limits = c(0, 14),
    diff_breaks = round(seq(-8, 8, by = 2), 1),
    diff_limits = c(-10, 10),
    sd_breaks = round(seq(0, 0.25, by = 0.05), 3),
    sd_limits = c(0, 0.25 + 1.5 * 0.05),
    mad_breaks = round(seq(0, 0.6, by = 0.15), 3),
    mad_limits = c(0, 0.75)
  ),
  'nee' = list(
    name = 'nee',
    label = 'NEE',
    label_precision = 1,
    drop_second_labels = TRUE,
    palette = 'Tropic',
    reverse = FALSE,
    symmetric = TRUE,
    mean_breaks = round(seq(-2.5, 2.5, by = 0.5), 1),
    mean_limits = c(-2.5, 2.5),
    diff_breaks = round(seq(-2.5, 2.5, by = 0.5), 1),
    diff_limits = c(-3, 3),
    sd_breaks = round(seq(0, 0.1, by = 0.025), 3),
    sd_limits = c(0, 0.1 + 1.5 * 0.025),
    mad_breaks = round(seq(0, 0.45, by = 0.15), 3),
    mad_limits = c(0, 0.6)
  )
)
if (!(args$flux_component %in% names(flux_key))) {
  stop('Invalid flux component')
}
flux_key <- flux_key[[args$flux_component]]

region_sf <- readRDS(args$region_sf)

six_year_average_base <- fst::read_fst(args$six_year_average) %>%
  filter(
    estimate %in% c('LNLGISSIF', 'FLUXCOM', 'FLUXCOM Difference'),
    inventory == flux_key$name,
    abs(latitude) != 89.5
  ) %>%
  select(-inventory) %>%
  mutate(
    estimate = case_match(
      estimate,
      'FLUXCOM Difference' ~ 'Difference',
      .default = estimate
    )
  )

# Join the six-year average with the full TransCom grid to display missing values
six_year_average <- expand.grid(
  longitude = seq(-180, 177.5, by = 2.5),
  latitude = seq(-88, 88, by = 2),
  estimate = c('LNLGISSIF', 'FLUXCOM', 'Difference')
) %>%
  left_join(
    six_year_average_base,
    by = c('longitude', 'latitude', 'estimate')
  )

six_year_average_sfs <- convert_to_sf(
  six_year_average %>% filter(estimate != 'Difference'),
  flux_key$mean_breaks,
  flux_key$mean_limits,
  flux_key$sd_breaks,
  flux_key$sd_limits,
  flux_key$mad_breaks,
  flux_key$mad_limits
)

six_year_average_diff_sfs <- convert_to_sf(
  six_year_average %>% filter(estimate == 'Difference'),
  flux_key$diff_breaks,
  flux_key$diff_limits,
  flux_key$sd_breaks,
  flux_key$sd_limits
)

flux_mean_label <- expression('Flux [kgCO'[2]~m^{-2}~yr^{-1}*']')
flux_sd_label <- expression('Posterior st. dev. [kgCO'[2]~m^{-2}~yr^{-1}*']')
flux_mad_label <- expression('Median abs. dev. [kgCO'[2]~m^{-2}~yr^{-1}*']')

# TODO: use uneven colour splits

average_mean_fluxcom <- six_year_average_sfs$mean %>%
  filter(estimate == 'FLUXCOM') %>%
  plot_map(
    value,
    flux_key$mean_breaks,
    flux_key$mean_limits,
    flux_key$palette,
    reverse = flux_key$reverse,
    symmetric = flux_key$symmetric,
    label_precision = flux_key$label_precision,
    drop_second_labels = flux_key$drop_second_labels,
    show_excess = TRUE
  ) +
    labs(fill = flux_mean_label) +
    ggtitle('FLUXCOM mean')

average_mad_fluxcom <- six_year_average_sfs$mad %>%
  filter(estimate == 'FLUXCOM') %>%
  plot_map(
    value,
    flux_key$mad_breaks,
    flux_key$mad_limits,
    'PuBu',
    show_excess = TRUE,
    drop_second_labels = FALSE,
    label_precision = 2,
    symmetric = FALSE,
    reverse = TRUE
  ) +
    labs(fill = flux_mad_label) +
    ggtitle('FLUXCOM median abs. dev.')

average_posterior_mean_wombat <- six_year_average_sfs$mean %>%
  filter(estimate == 'LNLGISSIF') %>%
  plot_map(
    value,
    flux_key$mean_breaks,
    flux_key$mean_limits,
    flux_key$palette,
    reverse = flux_key$reverse,
    symmetric = flux_key$symmetric,
    label_precision = flux_key$label_precision,
    drop_second_labels = flux_key$drop_second_labels,
    show_excess = TRUE
  ) +
    labs(fill = flux_mean_label) +
    ggtitle('WOMBAT v2S posterior mean')

average_posterior_sd_wombat <- six_year_average_sfs$sd %>%
  filter(estimate == 'LNLGISSIF') %>%
  plot_map(
    value,
    flux_key$sd_breaks,
    flux_key$sd_limits,
    'BuPu',
    show_excess = TRUE,
    drop_second_labels = FALSE,
    label_precision = 2,
    symmetric = FALSE,
    reverse = TRUE
  ) +
    labs(fill = flux_sd_label) +
    ggtitle('WOMBAT v2S posterior st. dev.')

average_posterior_mean_diff <- six_year_average_diff_sfs$mean %>%
  filter(estimate == 'Difference') %>%
  plot_map(
    value,
    flux_key$diff_breaks,
    flux_key$diff_limits,
    'RdBu',
    reverse = TRUE,
    symmetric = TRUE,
    label_precision = flux_key$label_precision,
    drop_second_labels = TRUE,
    show_excess = TRUE
  ) +
    labs(fill = expression('Flux difference [kgCO'[2]~m^{-2}~yr^{-1}*']')) +
    ggtitle('WOMBAT v2S - FLUXCOM mean')

average_posterior_sd_diff <- six_year_average_diff_sfs$sd %>%
  filter(estimate == 'Difference') %>%
  plot_map(
    value,
    flux_key$sd_breaks,
    flux_key$sd_limits,
    'BuPu',
    show_excess = TRUE,
    drop_second_labels = FALSE,
    label_precision = 2,
    symmetric = FALSE,
    reverse = TRUE
  ) +
    labs(fill = flux_sd_label) +
    ggtitle('Posterior st. dev. (v2S - FLUXCOM)')

base_theme <- theme(
  legend.position = 'bottom',
  legend.margin = margin(t = -0.4, l = 0.05, b = -0.2, r = 0.05, unit = 'cm'),
  legend.title = element_text(size = 8),
  plot.title = element_text(
    hjust = 0.5,
    vjust = 1,
    size = 9,
    margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')
  ),
  plot.margin = margin(t = 0.4, b = 0.2, l = 0.05, r = 0.05, unit = 'cm')
)

top_theme <- base_theme +
  theme(
    plot.margin = margin(t = 0.3, b = 0.2, l = 0.05, r = 0.05, unit = 'cm')
  )

output <- wrap_plots(
  average_mean_fluxcom + top_theme,
  average_mad_fluxcom + top_theme,
  average_posterior_mean_wombat + base_theme,
  average_posterior_sd_wombat + base_theme,
  average_posterior_mean_diff + base_theme,
  average_posterior_sd_diff + base_theme,
  nrow = 3,
  ncol = 2
)

output <- wrap_plots(
  wrap_elements(
      panel = grid::textGrob(sprintf('Average %s from January 2015 to December 2020', flux_key$label))
    ),
    output,
    heights = c(0.05, 1)
)

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width,
  height = 17.12
)
