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

convert_to_sf <- function(df, mean_breaks, mean_limits, sd_breaks, sd_limits) {
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
            value = discretise_by_breaks(value_sd, sd_breaks, sd_limits)
          ) %>%
          grid_df_to_sf('value') %>%
          mutate(
            estimate = .y$estimate
          )
      }) %>%
      bind_rows()
  )
}

plot_map_mean <- function(df, variable, flux_key) {
  plot_map(
    df,
    {{ variable }},
    flux_key$mean_breaks,
    flux_key$mean_limits,
    flux_key$palette,
    reverse = flux_key$reverse,
    symmetric = flux_key$symmetric,
    drop_second_labels = flux_key$drop_second_labels,
    label_precision = flux_key$label_precision_mean,
    show_excess = TRUE
  )
}

plot_map_sd <- function(df, variable, flux_key) {
  plot_map(
    df,
    {{ variable }},
    flux_key$sd_breaks,
    flux_key$sd_limits,
    'BuPu',
    show_excess = TRUE,
    drop_second_labels = FALSE,
    label_precision = flux_key$label_precision_sd,
    symmetric = FALSE,
    reverse = TRUE
  )
}

flux_key <- list(
  'gpp' = list(
    name = 'bio_assim',
    label = 'GPP',
    label_precision_mean = 0,
    label_precision_sd = 2,
    drop_second_labels = FALSE,
    palette = 'BluYl',
    reverse = FALSE,
    symmetric = FALSE,
    mean_breaks = round(seq(-12, 0, by = 2), 1),
    mean_limits = c(-14, 0),
    diff_breaks = round(seq(-3, 3, by = 1), 1),
    diff_limits = c(-4, 4),
    sd_breaks = round(seq(0, 0.25, by = 0.05), 3),
    sd_limits = c(0, 0.25 + 1.5 * 0.05)
  ),
  'resp' = list(
    name = 'bio_resp_tot',
    label = 'respiration',
    label_precision_mean = 0,
    label_precision_sd = 2,
    drop_second_labels = FALSE,
    palette = 'Magenta',
    reverse = TRUE,
    symmetric = FALSE,
    mean_breaks = round(seq(0, 12, by = 2), 1),
    mean_limits = c(0, 14),
    diff_breaks = round(seq(-3, 3, by = 1), 1),
    diff_limits = c(-4, 4),
    sd_breaks = round(seq(0, 0.25, by = 0.05), 3),
    sd_limits = c(0, 0.25 + 1.5 * 0.05)
  ),
  'nee' = list(
    name = 'nee',
    label = 'NEE',
    label_precision_mean = 1,
    label_precision_sd = 3,
    drop_second_labels = TRUE,
    palette = 'Tropic',
    reverse = FALSE,
    symmetric = TRUE,
    mean_breaks = round(seq(-0.6, 0.6, by = 0.1), 1),
    mean_limits = c(-0.6, 0.6),
    diff_breaks = round(seq(-0.6, 0.6, by = 0.1), 1),
    diff_limits = c(-0.6, 0.6),
    sd_breaks = round(seq(0, 0.1, by = 0.025), 3),
    sd_limits = c(0, 0.1 + 1.5 * 0.025)
  )
)
if (!(args$flux_component %in% names(flux_key))) {
  stop('Invalid flux component')
}
flux_key <- flux_key[[args$flux_component]]

region_sf <- readRDS(args$region_sf)

# Join the six-year average with the full TransCom grid to display missing values
six_year_average <- expand.grid(
  longitude = seq(-180, 177.5, by = 2.5),
  latitude = seq(-88, 88, by = 2),
  estimate = c('Bottom-up', 'LNLGIS', 'LNLGISSIF', 'Difference')
) %>%
  left_join(
    fst::read_fst(args$six_year_average) %>%
      filter(
        inventory == flux_key$name,
        abs(latitude) != 89.5
      ),
    by = c('longitude', 'latitude', 'estimate')
  )

six_year_average_sfs <- convert_to_sf(
  six_year_average %>% filter(estimate != 'Difference'),
  flux_key$mean_breaks,
  flux_key$mean_limits,
  flux_key$sd_breaks,
  flux_key$sd_limits
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

average_bottom_up <- six_year_average_sfs$mean %>%
  filter(estimate == 'Bottom-up') %>%
  plot_map_mean(value, flux_key) +
    labs(fill = flux_mean_label) +
    ggtitle('Bottom-up')

average_posterior_mean_wosif <- six_year_average_sfs$mean %>%
  filter(estimate == 'LNLGIS') %>%
  plot_map_mean(value, flux_key) +
    labs(fill = flux_mean_label) +
    ggtitle('Posterior mean, v2')

average_posterior_sd_wosif <- six_year_average_sfs$sd %>%
  filter(estimate == 'LNLGIS') %>%
  plot_map_sd(value, flux_key) +
    labs(fill = flux_sd_label) +
    ggtitle('Posterior st. dev., v2')

average_posterior_mean_wsif <- six_year_average_sfs$mean %>%
  filter(estimate == 'LNLGISSIF') %>%
  plot_map_mean(value, flux_key) +
    labs(fill = flux_mean_label) +
    ggtitle('Posterior mean, v2S')

average_posterior_sd_wsif <- six_year_average_sfs$sd %>%
  filter(estimate == 'LNLGISSIF') %>%
  plot_map_sd(value, flux_key) +
    labs(fill = flux_sd_label) +
    ggtitle('Posterior st. dev., v2S')

average_posterior_mean_diff <- six_year_average_diff_sfs$mean %>%
  filter(estimate == 'Difference') %>%
  plot_map(
    value,
    flux_key$diff_breaks,
    flux_key$diff_limits,
    'RdBu',
    reverse = TRUE,
    symmetric = TRUE,
    drop_second_labels = flux_key$drop_second_labels,
    label_precision = flux_key$label_precision_mean,
    show_excess = TRUE
  ) +
    labs(fill = expression('Flux difference [kgCO'[2]~m^{-2}~yr^{-1}*']')) +
    ggtitle('Difference (v2S - v2)')

average_posterior_sd_diff <- six_year_average_diff_sfs$sd %>%
  filter(estimate == 'Difference') %>%
  plot_map_sd(value, flux_key) +
    labs(fill = expression('Posterior st. dev. [kgCO'[2]~m^{-2}~yr^{-1}*']')) +
    ggtitle('Posterior st. dev. (v2S - v2)')

base_theme <- theme(
  legend.position = 'bottom',
  legend.margin = margin(t = -0.2, l = 0.05, b = -0.2, r = 0.05, unit = 'cm'),
  legend.title = element_text(size = 8),
  plot.title = element_text(
    hjust = 0.5,
    vjust = 1,
    size = 9,
    margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'cm')
  ),
  plot.margin = margin(t = 0.1, b = 0.2, l = 0.05, r = 0.05, unit = 'cm')
)

top_theme <- base_theme +
  theme(
    legend.position = 'none',
    plot.margin = margin(t = 0.2, b = 0, l = 0.05, r = 0.05, unit = 'cm')
  )

middle_theme <- base_theme +
  theme(
    legend.position = 'none',
    plot.margin = margin(t = -0.2, b = 0.2, l = 0.05, r = 0.05, unit = 'cm')
  )

bottom_theme <- base_theme +
  theme(
    plot.margin = margin(t = 0.4, b = 0.2, l = 0.05, r = 0.05, unit = 'cm')
  )

output <- wrap_plots(
  average_bottom_up + top_theme,
  wrap_plots(
    average_posterior_mean_wosif + middle_theme,
    average_posterior_sd_wosif + middle_theme,
    average_posterior_mean_wsif + base_theme,
    average_posterior_sd_wsif + base_theme,
    average_posterior_mean_diff + bottom_theme,
    average_posterior_sd_diff + bottom_theme,
    nrow = 3,
    ncol = 2
  ),
  ncol = 1,
  widths = c(1, 4),
  heights = c(1, 5)
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
  height = 19.32
)
