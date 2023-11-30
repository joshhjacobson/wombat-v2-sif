Sys.setenv(UTILS_PARTIAL = 'partials/utils.R')

args <- list(
  oco2_sif_directory = 'data/OCO2_L2_Lite_SIF_10r',
  start_date = '2014-09-01',
  end_date = ' 2021-04-01',
  output = '0_sif/intermediates/observations_sif.fst'
)