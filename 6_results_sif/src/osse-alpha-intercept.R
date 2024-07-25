library(argparse)
library(dplyr, warn.conflicts = FALSE)

source(Sys.getenv('DISPLAY_PARTIAL'))

args <- list(
  samples = '4_inversion/intermediates/osse-samples-ALPHASMALL-FREERESP-WSIF.rds',
  output = '6_results_sif/figures/osse-alpha-intercepts-ALPHASMALL.pdf'
)

samples <- readRDS(args$samples)

n_samples_available <- samples$n_samples - samples$n_warm_up
n_intercept_samples <- 100
stopifnot(n_samples_available >= n_intercept_samples)

intercept_coefficients <- samples$alpha_df %>%
  filter(component == 'intercept') %>%
  select(region, inventory, value_samples) %>%
  nest_by(region) %>%


regions <- intercept_coefficients %>%
  filter(inventory == 'bio_resp_tot') %>%
  pull(region) %>%
  unique

intercept_samples <- lapply(
  as.character(regions),
  function(r) {
    sample_indices <- sample(seq(n_samples_available), n_intercept_samples)
    samples_region <- intercept_coefficients %>%
      filter(region == r)
    data.frame(
      region = r,
      gpp = samples_region$value_samples[1, sample_indices],
      resp = samples_region$value_samples[2, sample_indices]
    )
  }
) %>%
  bind_rows()

output <- intercept_samples %>%
  ggplot(aes(x = resp, y = gpp)) +
  geom_point(shape = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  facet_wrap(~region, scales = 'free') +
  labs(
    x = 'Respiration coefficient',
    y = 'GPP coefficient',
    title = 'Intercept coefficients: With SIF, Free RLT, ALPHASMALL'
  ) +
  theme(
    aspect.ratio = 1,
    plot.margin = margin(t = 1, r = 2, b = 0, l = 1, unit = 'mm'),
    plot.title = element_text(size = 13, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 9)
  )

ggsave_base(
  args$output,
  output,
  width = DISPLAY_SETTINGS$full_width,
  height = 12.58
)
