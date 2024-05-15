library(dplyr)

data.frame(
  hyperparameter_group = '3_SIF',
  rho = 0.8,
  ell = 0.75,
  gamma = 0.6,
  ell_unit = 'mins'
)

params_LNLGIS <- fst::read_fst('/data/wombat-v2-workflow/3_inversion/intermediates/hyperparameter-estimates.fst')

params_LNLGISSIF <- fst::read_fst('4_inversion/intermediates/hyperparameter-estimates.fst')


bind_rows(
  params_LNLGIS %>%
    mutate(inversion = 'LNLGIS'),
  data.frame(
    inversion = 'hard-coded',
    hyperparameter_group = '3_SIF',
    rho = 0.8,
    ell = 0.75,
    gamma = 0.6,
    ell_unit = 'mins'
  ),
  params_LNLGISSIF %>%
    mutate(inversion = 'LNLGISSIF') %>%
    filter(hyperparameter_group != 'lauder')
) %>%
  select(inversion, everything())
