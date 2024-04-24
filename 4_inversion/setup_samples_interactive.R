# Setup file to run 04_inversion/src/samples.R interactively.
# With an R session running in the base directory, either source this file within samples_interactive.R, 
# or run the following lines:

Sys.setenv(UTILS_PARTIAL = 'partials/utils.R')
Sys.setenv(UTILS_CPP_PARTIAL = 'partials/utils.cpp')
Sys.setenv(HMC_EXACT_CPP_PARTIAL = 'partials/hmc-exact.cpp')

args <- list(
  overall_observation_mode = c('LN', 'LG', 'IS', 'LN_SIF', 'LG_SIF'),
  control = c(
    '2_matching/intermediates/runs/base/oco2-hourly.fst',
    '2_matching/intermediates/runs/base/obspack-hourly-assim-1.fst',
    '3_sif/intermediates/oco2-hourly-sif.fst'
  ),
  constraints = '4_inversion/intermediates/constraints.rds',
  free_resp_linear = FALSE,
  component_name = c('LNLG', 'IS', 'SIF'),
  component_parts = c('LN|LG', 'IS', 'LN_SIF|LG_SIF'),
  component_transport_matrix = c(
    '4_inversion/intermediates/H-LNLG.mat.lz4',
    '4_inversion/intermediates/H-IS.mat.lz4',
    '4_inversion/intermediates/H-SIF.mat.lz4'
  ),
  observations = '4_inversion/intermediates/osse-observations-ALPHAV2.fst',
  hyperparameter_estimates = '4_inversion/intermediates/hyperparameter-estimates.fst',
  basis_vectors = '4_inversion/intermediates/basis-vectors.fst',
  prior = '4_inversion/intermediates/prior.rds',
  alpha = 'data/wombat-v2-alpha-LGLGIS.fst',
  output = '4_inversion/intermediates/samples-LNLGISSIF.rds'
)

# Then run each line in the file samples.R except for the ones that set up the args 
# (lines 98 to 110).