Sys.setenv(UTILS_PARTIAL = 'partials/utils.R')

args <- list(
  oco2_sif_directory = 'data/OCO2_L2_Lite_SIF_10r',
  start_date = '2014-09-01',
  end_date = '2021-04-01',
  # output = '3_sif/intermediates/observations_sif.fst',
  output = '1_transport/intermediates/region-mask-base.rds',
  transcom_mask = '1_transport/intermediates/TRANSCOM_mask_GEOS_Chem_2x2.5.nc'
)
