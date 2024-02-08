library(argparse)
library(dplyr, warn.conflicts = FALSE)
library(ncdf4)
library(parallel)


# Questions:
# 1. Do we need both part1 and part2?
#   - looks like just part1; oco2 hourly has ~ 2 billion rows and max is 4 billion
# 2. Do we need both hourly and daily?
#   - looks like just hourly
# 3. Do we need the NZ separation?



# What should a resulting sensitivity file look like?
df_result <- fst::read_fst('/data/wombat-mipv10/3_inversion/intermediates/sensitivities-oco2-hourly-part-1.fst')

dim(df_result)
head(df_result)

col_names <- c('observation_id', 'resolution', 'region', 'month', 'inventory', 'component', 'value')
# These should all be factors, except for value
# observation_id will be the OCO-2 SIF observation_id
# month is either YYYY-MM or NA
# Constants: resolution = hourly, inventory = bio_assim

df_result %>% sample_n(10)



# What do the pre-transport climatology and residuals look like?
# NOTE: Use ncdump to view in terminal
df_clim <- nc_open('1_transport/intermediates/sib4-hourly-climatology-inventory-assim-2016.nc')
df_resid <- nc_open('1_transport/intermediates/sib4-hourly-residual-assim-2016.nc')