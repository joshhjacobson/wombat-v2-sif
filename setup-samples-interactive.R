# Setup file to run 03_inversion/src/samples.R interactively.
# With an R session running in the base directory, either source this file within samples.R, 
# or run the following lines:

Sys.setenv(UTILS_PARTIAL = 'partials/utils.R')
Sys.setenv(UTILS_CPP_PARTIAL = 'partials/utils.cpp')
Sys.setenv(HMC_EXACT_CPP_PARTIAL = 'partials/hmc-exact.cpp')

args <- list(
  overall_observation_mode = c('LN', 'LG', 'IS'),
  control = c(
    '2_matching/intermediates/runs/base/obspack-hourly-assim-1.fst',
    '2_matching/intermediates/runs/base/oco2-hourly.fst'
  ),
  constraints = '3_inversion/intermediates/constraints.rds',
  component_name = c('LNLG', 'IS'),
  component_parts = c('LN|LG', 'IS'),
  component_transport_matrix = c(
    '3_inversion/intermediates/H-LNLG.mat.lz4',
    '3_inversion/intermediates/H-IS.mat.lz4'
  ),
  observations = '3_inversion/intermediates/observations.fst',
  hyperparameter_estimates = '3_inversion/intermediates/hyperparameter-estimates.fst',
  basis_vectors = '3_inversion/intermediates/basis-vectors.fst',
  prior = '3_inversion/intermediates/prior.rds',
  output = '3_inversion/intermediates/samples-LNLGIS.rds'
)

# Then run each line in the file samples.R except for the ones that set up the args 
# (lines 98 to 110).