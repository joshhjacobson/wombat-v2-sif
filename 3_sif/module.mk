$(shell mkdir -p 3_sif/intermediates)

OCO2_OBSERVATIONS_SIF = 3_sif/intermediates/observations-sif.fst

SIB4_INPUT_YEARS = 2014 2015 2016 2017 2018 2019 2020
SIB4_SIF_HOURLY = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),3_sif/intermediates/sib4-hourly-sif-$(SIB4_YEAR).nc)
# SIB4_SIF_GPP_HOURLY_2X25 = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),3_sif/intermediates/sib4-hourly-sif-gpp-2x25-$(SIB4_YEAR).nc)

# SLOPE_SIF_GPP = 3_sif/intermediates/slope-sif-gpp.nc
# CONTROL_SIF = 3_sif/intermediates/control-sif.fst


# SIF control

# $(CONTROL_SIF): \
# 	3_sif/src/control-sif.R \
# 		$(SIB4_SIF_HOURLY) \
# 		$(SLOPES) \
# 	Rscript $< \
# 		--inventory $(SIB4_SIF_HOURLY) \
# 		--slopes $(SLOPES) \
# 		--output $@

# Fitted slope coefficients

# NOTE: depending on chosen time resolution, may need to input GPP product separately
# $(SLOPE_SIF_GPP): \
# 	3_sif/src/linear-model.R \
# 		$(SIB4_SIF_GPP_HOURLY_2X25)
# 	Rscript $< \
# 		--input $(SIB4_SIF_GPP_HOURLY_2X25)
# 		--region-mask $(REGION_MASK_BASE) \
# 		--ouput $a

# SIF and GPP inventory

# bash script to call cdo in loop over yearly files
# $(SIB4_SIF_GPP_HOURLY_2X25) &: \
# 	$(SIB4_HOURLY) \
# 	$(SIB4_SIF_HOURLY) \
# 	$(SIB4_SIF_GPP_HOURLY) \
# 	$(GEOS_2X25_GRID) \
# 	cdo combine sif and assim to produce SIB4_SIF_GPP_HOURLY
# 	cdo -f nc2 remapcon,$(GEOS_2X25_GRID) $(SIB4_SIF_GPP_HOURLY) $@

# SiB4 SIF units are listed as W/m2/nm/sr but are actually W/m^2/µm/sr
# See SIF section SiB4 technical description: https://hdl.handle.net/10217/200691
# Also see the related paper on the canopy scaling method: https://doi.org/10.1111/gcb.12948
$(SIB4_SIF_HOURLY) &:
	cdo -w -z zip_6 \
		-splityear \
		-shifttime,-30minutes \
		-settunits,minutes \
		-setattribute,sif@units=W/m^2/µm/sr \
		-vertsum \
		-select,name=sif \
		$(SIB4_HOURLY_DIRECTORY)/sib4-hourly-{2014,2015,2016,2017,2018,2019,2020}-* \
		3_sif/intermediates/sib4-hourly-sif

# Observations (units: W/m^2/µm/sr)
$(OCO2_OBSERVATIONS_SIF): \
	3_sif/src/observations-sif.R
	Rscript $< \
		--oco2-sif-directory $(OCO2_SIF_DIRECTORY) \
		--start-date $(INVERSION_START_DATE) \
		--end-date $(INVERSION_END_DATE) \
		--output $@