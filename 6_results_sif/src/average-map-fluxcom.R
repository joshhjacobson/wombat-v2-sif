library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(Matrix)
library(patchwork)

source(Sys.getenv('UTILS_PARTIAL'))
source(Sys.getenv('DISPLAY_PARTIAL'))

parser <- ArgumentParser()
parser$add_argument('--six-year-average')
parser$add_argument('--fluxcom-monthly-2x25')
parser$add_argument('--flux-component')
parser$add_argument('--region-sf')
parser$add_argument('--output')
args <- parser$parse_args()

convert_to_sf <- function(
  df,
  mean_breaks,
  mean_limits,
  sd_breaks = NULL,
  sd_limits = NULL,
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
    sd = if (!is.null(sd_breaks) & !is.null(sd_limits)) {
      df %>%
        filter(estimate == 'LNLGISSIF') %>%
        mutate(
          value = discretise_by_breaks(value_scale, sd_breaks, sd_limits)
        ) %>%
        grid_df_to_sf('value')
    } else NULL,
    mad = if (!is.null(mad_breaks) & !is.null(mad_limits)) {
      df %>%
        filter(estimate == 'FLUXCOM') %>%
        mutate(
          value = discretise_by_breaks(value_scale, mad_breaks, mad_limits)
        ) %>%
        grid_df_to_sf('value')
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

six_year_average_fluxcom <- fst::read_fst(args$fluxcom_monthly_2x25) %>%
  filter(
    inventory == flux_key$name,
    abs(latitude) != 89.5
  ) %>%
  group_by(longitude, latitude, method) %>%
  summarise(
    value = GC_DAY_TO_KGCO2_YEAR * sum(value) / 72,
    .groups = 'drop'
  ) %>%
  group_by(longitude, latitude) %>%
  summarise(
    value_mean = mean(value),
    value_scale = mad(value, constant = 1),
    .groups = 'drop'
  ) %>%
  mutate(
    estimate = 'FLUXCOM'
  )

six_year_average <- fst::read_fst(args$six_year_average) %>%
  filter(
    estimate == 'LNLGISSIF',
    inventory == flux_key$name,
    abs(latitude) != 89.5
  ) %>%
  select(-inventory) %>%
  rename(value_scale = value_sd) %>%
  bind_rows(six_year_average_fluxcom) %>%
  {
    x <- .

    bind_rows(
      x,
      x %>%
        select(-value_scale) %>%
        tidyr::pivot_wider(
          names_from = estimate,
          values_from = value_mean
        ) %>%
        mutate(
          estimate = 'Difference',
          value_mean = LNLGISSIF - FLUXCOM
        ) %>%
        select(longitude, latitude, estimate, value_mean)
    )
  }

# Join the six-year average with the full TransCom grid to display missing values
six_year_average <- expand.grid(
  longitude = seq(-180, 177.5, by = 2.5),
  latitude = seq(-88, 88, by = 2),
  estimate = c('LNLGISSIF', 'FLUXCOM', 'Difference')
) %>%
  left_join(
    six_year_average,
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
  flux_key$diff_limits
)

flux_mean_label <- expression('Flux [kgCO'[2]~m^{-2}~yr^{-1}*']')
flux_sd_label <- expression('Posterior st. dev. [kgCO'[2]~m^{-2}~yr^{-1}*']')
flux_mad_label <- expression('Median abs. dev. [kgCO'[2]~m^{-2}~yr^{-1}*']')

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

average_diff <- six_year_average_diff_sfs$mean %>%
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
    ggtitle('WOMBAT v2S - FLUXCOM')

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

bottom_theme <- base_theme +
  theme(
    plot.margin = margin(t = 0, b = 0.2, l = 0.05, r = 0.05, unit = 'cm'),
    legend.margin = margin(t = -0.4, l = 0.05, b = 0, r = 0.05, unit = 'cm')
  )

output <- wrap_plots(
  wrap_plots(
    average_mean_fluxcom + top_theme,
    average_mad_fluxcom + top_theme,
    average_posterior_mean_wombat + base_theme,
    average_posterior_sd_wombat + base_theme,
    nrow = 2,
    ncol = 2
  ),
  average_diff + bottom_theme,
  ncol = 1,
  widths = c(2.5, 1),
  heights = c(2.5, 1)
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
  height = 17.52
)
