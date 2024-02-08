3_SIF_SRC_DIR = 3_sif/src
3_SIF_INTERMEDIATES_DIR = 3_sif/intermediates
3_SIF_FIGURES_DIR = 3_sif/figures

$(shell mkdir -p $(3_SIF_INTERMEDIATES_DIR))
$(shell mkdir -p $(3_SIF_FIGURES_DIR))

# NOTE: for consistency the rest of the project, maybe rename GPP to ASSIM throughout the this directory

SIB4_SIF_HOURLY = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),3_sif/intermediates/sib4-hourly-sif-$(SIB4_YEAR).nc)
SIB4_SIF_GPP_HOURLY_2X25 = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),3_sif/intermediates/sib4-hourly-sif-gpp-2x25-$(SIB4_YEAR).nc)
MODEL_SIF_GPP = 3_sif/intermediates/model-sif-gpp.fst

OCO2_OBSERVATIONS_SIF = 3_sif/intermediates/observations-sif.fst
CONTROL_SIF = 3_sif/intermediates/control-sif.fst

SIB4_CLIMATOLOGY_INVENTORY_ASSIM_HOURLY_2X25 = $(foreach SIB4_YEAR,$(INVENTORY_OUTPUT_YEARS),3_sif/intermediates/sib4-hourly-climatology-inventory-assim-2x25-$(SIB4_YEAR).nc)
SIB4_RESIDUAL_ASSIM_HOURLY_2X25 = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),3_sif/intermediates/sib4-hourly-residual-assim-2x25-$(SIB4_YEAR).nc)

SENSITIVITIES_SIF_BASE_PART_1 = 3_sif/intermediates/sensitivities-sif-base-oco2-hourly-part-1.fst

# Sensitivities

$(SENSITIVITIES_SIF_BASE_PART_1) &: \
	3_sif/src/sensitivities-sif.R
	Rscript $< \
		--input \
			oco2-hourly.fst \
			oco2-daily.fst \
		--resolution hourly daily \
		--runs 1_transport/intermediates/runs \
		--matched-runs 2_matching/intermediates/runs \
		--output-base 3_inversion/intermediates/sensitivities-sif-base-oco2-hourly \
			3_inversion/intermediates/sensitivities-sif-base-oco2-daily

## Regrid time-series components to basis grid

$(SIB4_CLIMATOLOGY_INVENTORY_ASSIM_HOURLY_2X25) &: \
	3_sif/src/regrid-decomposition.sh \
	$(GEOS_2X25_GRID) \
	$(SIB4_CLIMATOLOGY_INVENTORY_ASSIM_HOURLY)
	bash $< \
		$(GEOS_2X25_GRID) \
		"$(SIB4_CLIMATOLOGY_INVENTORY_ASSIM_HOURLY)" \
		3_sif/intermediates

$(SIB4_RESIDUAL_ASSIM_HOURLY_2X25) &: \
	3_sif/src/regrid-decomposition.sh \
	$(GEOS_2X25_GRID) \
	$(SIB4_RESIDUAL_ASSIM_HOURLY)
	bash $< \
		$(GEOS_2X25_GRID) \
		"$(SIB4_RESIDUAL_ASSIM_HOURLY)" \
		3_sif/intermediates

# Control SIF

$(CONTROL_SIF): \
	3_sif/src/match-sif.R \
	$(OCO2_OBSERVATIONS_SIF) \
	$(SIB4_SIF_GPP_HOURLY_2X25) \
	$(MODEL_SIF_GPP)
	Rscript $< \
		--oco2-observations-sif $(OCO2_OBSERVATIONS_SIF) \
		--inventory-list "$(SIB4_SIF_GPP_HOURLY_2X25)" \
		--linear-models $(MODEL_SIF_GPP) \
		--output $@

# Fitted slope coefficients

$(MODEL_SIF_GPP): \
	3_sif/src/linear-model.R \
	$(SIB4_SIF_GPP_HOURLY_2X25)
	Rscript $< \
		--input-list "$(SIB4_SIF_GPP_HOURLY_2X25)" \
		--parallel-strategy multicore \
		--output $@

# SIF and GPP inventories

$(SIB4_SIF_GPP_HOURLY_2X25) &: \
	3_sif/src/regrid-sif-gpp.sh \
	$(GEOS_2X25_GRID) \
	$(SIB4_HOURLY) \
	$(SIB4_SIF_HOURLY)
	bash $< \
		$(GEOS_2X25_GRID) \
		"$(SIB4_HOURLY)" \
		"$(SIB4_SIF_HOURLY)" \
		3_sif/intermediates

# SiB4 SIF units are listed as W/m2/nm/sr but are actually W/m^2/µm/sr
# See SIF section in SiB4 technical description: https://hdl.handle.net/10217/200691
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
		3_sif/intermediates/sib4-hourly-sif-

# Observations (units: W/m^2/µm/sr)
$(OCO2_OBSERVATIONS_SIF): \
	3_sif/src/observations-sif.R
	Rscript $< \
		--oco2-sif-directory $(OCO2_SIF_DIRECTORY) \
		--start-date $(INVERSION_START_DATE) \
		--end-date $(INVERSION_END_DATE) \
		--output $@