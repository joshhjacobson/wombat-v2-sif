library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(patchwork)

source('partials/display.R')

# SCALE_FACTOR <- 50

observations_inversion_path <- '4_inversion/intermediates/observations.fst'

observations_inversion <- fst::read_fst(observations_inversion_path) %>%
  mutate(
    coef_variation_measure = measurement_error / value,
    coef_variation_model = model_error / value,
    coef_variation_overall = error / value
  )  %>%
  filter(
    observation_group %in% c('1_LNLG', '3_SIF'),
    assimilate == 1
  )


# TODO: 164,319 rows under most recent approach... how many when we filter out "sparse" 10s values?
# RESULT: only 89,370 rows after filtering to count >= 10
# RESULT: 136,214 rows if filtering to count >= 5
observations_inversion_sif <- observations_inversion %>%
  filter(
    observation_group == '3_SIF'
  )

observations_inversion_lnlg <- observations_inversion %>%
  filter(
    observation_group == '1_LNLG'
  )


errors <- observations_inversion %>%
  select(c(observation_group, measurement_error, model_error, total_error = error)) %>%
  tidyr::pivot_longer(
    cols = c(measurement_error, model_error, total_error),
    names_to = 'error_type',
    values_to = 'error'
  ) %>%
  mutate(
    error_type = factor(c(
      'model_error' = 'Model Error',
      'measurement_error' = 'Measurement Error',
      'total_error' = 'Total Error'
    )[as.character(error_type)], levels = c(
      'Model Error',
      'Measurement Error',
      'Total Error'
    ))
  )

errors_top <- ggplot(
  errors %>% filter(observation_group == '3_SIF')
) +
  geom_histogram(aes(x = error), fill = '#fb8b00') +
  facet_wrap(~error_type, scales = 'free', nrow = 1) +
  labs(
    x = expression('SIF uncertainty [W' * m^-2 * µm^-1 * sr^-1 * ']'),
    y = 'Count',
    title = 'Uncertainties for LNLG SIF observations used in the inversion'
  )

errors_bottom <- ggplot(
  errors %>% filter(observation_group == '1_LNLG')
) +
  geom_histogram(aes(x = error), fill = '#625D5D') +
  facet_wrap(~error_type, scales = 'free', nrow = 1) +
  labs(
    x = expression('XCO2 uncertainty [ppm]'),
    y = 'Count',
    title = 'Uncertainties for LNLG mole-fraction observations used in the inversion'
  )

errors_top / errors_bottom

ggsave_base(
  '0_eda/figures/error-histograms-final.pdf',
  errors_top / errors_bottom,
  width = 18,
  height = 14
)




coefficients <- observations_inversion %>%
  select(c(observation_group, coef_variation_measure, coef_variation_model, coef_variation_overall)) %>%
  tidyr::pivot_longer(
    cols = c(coef_variation_measure, coef_variation_model, coef_variation_overall),
    names_to = 'coef_type',
    values_to = 'coef'
  )

p_errors <- ggplot(errors, aes(x = error)) +
  geom_histogram() +
  facet_wrap(observation_group~error_type, scales = 'free', nrow = 2) +
  labs(
    x = 'Error',
    y = 'Count'
  )

ggsave_base(
  '0_eda/figures/error-histograms-all-updated.pdf',
  p_errors,
  width = 18,
  height = 14
)

p_coefs <- ggplot(coefficients, aes(x = coef)) +
  geom_histogram() +
  facet_wrap(observation_group~coef_type, scales = 'free', nrow = 2) +
  scale_x_log10() +
  labs(
    x = 'Coefficient of Variation\n(negative SIF values not shown in log10 scale)',
    y = 'Count'
  )

ggsave_base(
  '0_eda/figures/coef-variation-all-updated.pdf',
  p_coefs,
  width = 18,
  height = 14
)

ggplot(coefficients %>% filter(observation_group == '3_SIF'), aes(x = coef)) +
  geom_histogram() +
  facet_wrap(~coef_type, scales = 'free', nrow = 1) +
  scale_x_log10()


observations_sif_path <- '3_sif/intermediates/observations-sif.fst'

observations_sif <- fst::read_fst(observations_sif_path) %>%
  mutate(
    coef_variation = measurement_error / value
  )


top <- wrap_plots(
  ggplot(observations_inversion, aes(x = measurement_error)) +
    geom_histogram() +
    labs(
      x = 'Measurement error\n(10s averages)',
      y = 'Count'
    )+
    ggtitle('Errors for SIF observations used in the inversion'),
  # ggplot(observations_inversion, aes(x = sqrt(SCALE_FACTOR * measurement_error^2))) +
  #   geom_histogram() +
  #   labs(
  #     x = 'Scaled measurement error\n(10s averages)',
  #     y = 'Count'
  #   ),
  ggplot(
    observations_inversion %>%
    filter(
      between(
        coef_variation, 
        quantile(coef_variation, 0.025), 
        quantile(coef_variation, 0.975)
      )
    ),
    aes(x = coef_variation)
  ) +
    geom_histogram() +
    labs(
      x = 'Coefficient of variation',
      y = 'Count'
    ),
  nrow = 1, guides = 'collect'
)

bottom <- wrap_plots(
  ggplot(observations_sif, aes(x = measurement_error)) +
    geom_histogram() +
    labs(
      x = 'Measurement error\n(10s averages)',
      y = 'Count'
    )+ 
    ggtitle('Errors for all SIF observations'),
  # ggplot(observations_sif, aes(x = sqrt(SCALE_FACTOR * measurement_error^2))) +
  #   geom_histogram() +
  #   labs(
  #     x = 'Scaled measurement error\n(10s averages)',
  #     y = 'Count'
  #   ),
  ggplot(
    observations_sif %>%
    filter(
      between(
        coef_variation, 
        quantile(coef_variation, 0.025), 
        quantile(coef_variation, 0.975)
      )
    ),
    aes(x = coef_variation)
  ) +
    geom_histogram() +
    labs(
      x = 'Coefficient of variation',
      y = 'Count'
    ),
  nrow = 1, guides = 'collect'
)

ggsave_base(
  '0_eda/figures/error-histograms-update.pdf',
  (top / bottom),
  width = 22,
  height = 14
)
