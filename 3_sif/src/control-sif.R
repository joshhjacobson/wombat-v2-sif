library(ncdf4)
library(ncmeta)

temp <- fst::read_fst("3_inversion/intermediates/control-emissions.fst")
head(temp)

temp <- fst::read_fst("2_matching/intermediates/runs/base/oco2-hourly.fst")
head(temp)

obs <- nc_open("data/OCO2_b10c_10sec_GOOD_r5.nc4")
fname <- "data/OCO2_b10c_10sec_GOOD_r5.nc4"
nc_vars(fname)


# Match SiB4 SIF inventory to OCO-2 observations
# Match fitted slopes to OCO-2 observations