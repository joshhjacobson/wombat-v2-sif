6_RESULTS_SIF_SRC_DIR = 6_results_sif/src
6_RESULTS_SIF_INTERMEDIATES_DIR = 6_results_sif/intermediates
6_RESULTS_SIF_FIGURES_DIR = 6_results_sif/figures
6_RESULTS_SIF_PRODUCTS_DIR = 6_results_sif/products

$(shell mkdir -p $(6_RESULTS_SIF_INTERMEDIATES_DIR))
$(shell mkdir -p $(6_RESULTS_SIF_FIGURES_DIR))
$(shell mkdir -p $(6_RESULTS_SIF_PRODUCTS_DIR))

# Intermediates
PERTURBATIONS_AUGMENTED_SIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/perturbations-augmented.fst
PERTURBATIONS_AUGMENTED_SIF_ZONAL = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/perturbations-augmented-zonal.fst
OSSE_FLUX_AGGREGATES_SAMPLES_BASE = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples
OSSE_FLUX_AGGREGATES_SAMPLES_CASES = $(foreach OSSE_CASE,$(OSSE_CASES),$(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-$(OSSE_CASE).rds)
OSSE_FLUX_AGGREGATES_ZONAL_SAMPLES_BASE = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-zonal-samples
OSSE_FLUX_AGGREGATES_ZONAL_SAMPLES_CASES = $(foreach OSSE_CASE,$(OSSE_CASES),$(OSSE_FLUX_AGGREGATES_ZONAL_SAMPLES_BASE)-$(OSSE_CASE).rds)

SIX_YEAR_AVERAGE_LNLGIS = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/six-year-average-LNLGIS.fst
SIX_YEAR_AVERAGE_LNLGISSIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/six-year-average-LNLGISSIF.fst

FLUXCOM_GPP_MONTHLY_2x25 = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/fluxcom-gpp-monthly-2x25.nc
FLUXCOM_TER_MONTHLY_2x25 = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/fluxcom-ter-monthly-2x25.nc
FLUXCOM_NEE_MONTHLY_2x25 = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/fluxcom-nee-monthly-2x25.nc
FLUXCOM_MONTHLY_2x25 = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/fluxcom-monthly-2x25.fst

6_RESULTS_SIF_TARGETS += \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-true-fluxes.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-table.tex \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-ALPHA0.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-ALPHAV2.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-ALPHAFREE.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-zonal-ALPHA0.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-zonal-ALPHAV2.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-zonal-ALPHAFREE.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-global.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-n-boreal.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-n-temperate.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-tropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-n-tropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-s-tropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-s-extratropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/observation-count.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-global.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-zonal.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-map.pdf


## Products


## Figures

$(6_RESULTS_SIF_FIGURES_DIR)/osse-true-fluxes.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-true-fluxes.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(ALPHA_SMALL) \
	$(ALPHA_LARGE) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--alpha-v2 $(ALPHA_WOMBAT_V2) \
		--alpha-free $(ALPHA_SMALL) \
		--alpha-free-large $(ALPHA_LARGE) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-table.tex: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-table.R \
	$(OSSE_FLUX_AGGREGATES_SAMPLES_CASES)
	Rscript $< \
		--flux-samples-alpha0-fixresp-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHA0-FIXRESP-WSIF.rds \
		--flux-samples-alpha0-fixresp-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHA0-FIXRESP-WOSIF.rds \
		--flux-samples-alpha0-freeresp-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHA0-FREERESP-WSIF.rds \
		--flux-samples-alpha0-freeresp-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHA0-FREERESP-WOSIF.rds \
		--flux-samples-alphav2-fixresp-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAV2-FIXRESP-WSIF.rds \
		--flux-samples-alphav2-fixresp-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAV2-FIXRESP-WOSIF.rds \
		--flux-samples-alphav2-freeresp-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAV2-FREERESP-WSIF.rds \
		--flux-samples-alphav2-freeresp-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAV2-FREERESP-WOSIF.rds \
		--flux-samples-alphafree-fixresp-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAFREE-FIXRESP-WSIF.rds \
		--flux-samples-alphafree-fixresp-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAFREE-FIXRESP-WOSIF.rds \
		--flux-samples-alphafree-freeresp-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAFREE-FREERESP-WSIF.rds \
		--flux-samples-alphafree-freeresp-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAFREE-FREERESP-WOSIF.rds \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-monthly.R \
	$(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-%-FIXRESP-WSIF.rds \
	$(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-%-FIXRESP-WOSIF.rds \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--flux-samples-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-$*-FIXRESP-WSIF.rds \
		--flux-samples-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-$*-FIXRESP-WOSIF.rds \
		--osse-base-case $* \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-zonal-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-zonal.R \
	$(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-zonal-samples-%-FIXRESP-WSIF.rds \
	$(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-zonal-samples-%-FIXRESP-WOSIF.rds \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--flux-samples-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-zonal-samples-$*-FIXRESP-WSIF.rds \
		--flux-samples-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-zonal-samples-$*-FIXRESP-WOSIF.rds \
		--osse-base-case $* \
		--output $@

# $(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-zonal-ALPHAFREE.pdf: \
# 	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-zonal.R \
# 	$(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-zonal-samples-ALPHAFREE-FREERESP-WSIF.rds \
# 	$(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-zonal-samples-ALPHAFREE-FIXRESP-WOSIF.rds \
# 	$(DISPLAY_PARTIAL)
# 	Rscript $< \
# 		--flux-samples-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-zonal-samples-ALPHAFREE-FREERESP-WSIF.rds \
# 		--flux-samples-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-zonal-samples-ALPHAFREE-FIXRESP-WOSIF.rds \
# 		--osse-base-case ALPHAFREE \
# 		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-flux-decomposition.R \
	$(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
	$(OSSE_SAMPLES_BASE)-ALPHAV2-FREERESP-WSIF.rds \
	$(OSSE_SAMPLES_BASE)-ALPHAV2-FREERESP-WOSIF.rds \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented-zonal $(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
		--samples-wsif $(OSSE_SAMPLES_BASE)-ALPHAV2-FREERESP-WSIF.rds \
		--samples-wosif $(OSSE_SAMPLES_BASE)-ALPHAV2-FREERESP-WOSIF.rds \
		--true-alpha $(ALPHA_WOMBAT_V2) \
		--region $* \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/observation-count.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/observation-count.R \
	$(OBSERVATIONS) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--observations $(OBSERVATIONS) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-global.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/flux-net-global.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(SAMPLES_LNLGIS) \
	$(SAMPLES_LNLGISSIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_LNLGIS) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-zonal.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/flux-net-zonal.R \
	$(AREA_1X1) \
	$(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
	$(SAMPLES_LNLGIS) \
	$(SAMPLES_LNLGISSIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented-zonal $(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
		--samples-LNLGIS $(SAMPLES_LNLGIS) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--output $@

# $(6_RESULTS_SIF_FIGURES_DIR)/average-map.pdf: \
# 	$(6_RESULTS_SIF_SRC_DIR)/average-map.R \
# 	$(SIX_YEAR_AVERAGE_LNLGIS) \
# 	$(SIX_YEAR_AVERAGE_LNLGISSIF) \
# 	$(REGION_SF) \
# 	$(DISPLAY_PARTIAL)
# 	Rscript $< \
# 		--six-year-average-LNLGIS $(SIX_YEAR_AVERAGE_LNLGIS) \
# 		--six-year-average-LNLGISSIF $(SIX_YEAR_AVERAGE_LNLGISSIF) \
# 		--region-sf $(REGION_SF) \
# 		--output $@

## Intermediates

# TODO: add FLUXCOM details to README
$(FLUXCOM_MONTHLY_2x25): \
	$(6_RESULTS_SIF_SRC_DIR)/fluxcom-monthly.R \
	$(FLUXCOM_GPP_MONTHLY_2x25) \
	$(FLUXCOM_TER_MONTHLY_2x25) \
	$(FLUXCOM_NEE_MONTHLY_2x25)
	Rscript $< \
		--fluxcom-gpp-monthly-2x25 $(FLUXCOM_GPP_MONTHLY_2x25) \
		--fluxcom-ter-monthly-2x25 $(FLUXCOM_TER_MONTHLY_2x25) \
		--fluxcom-nee-monthly-2x25 $(FLUXCOM_NEE_MONTHLY_2x25) \
		--output $@

$(FLUXCOM_GPP_MONTHLY_2x25): \
	$(GEOS_2X25_GRID)
	cdo -v -z zip_6 \
		-remapcon,$(GEOS_2X25_GRID) \
		-select,name=GPP,GPP_mad \
		$(FLUXCOM_DIRECTORY)/GPP.RS_V006.FP-ALL.MLM-ALL.METEO-NONE.720_360.monthly.{2015,2016,2017,2018,2019,2020}.nc \
		$@

$(FLUXCOM_TER_MONTHLY_2x25): \
	$(GEOS_2X25_GRID)
	cdo -v -z zip_6 \
		-remapcon,$(GEOS_2X25_GRID) \
		-select,name=TER,TER_mad \
		$(FLUXCOM_DIRECTORY)/TER.RS_V006.FP-ALL.MLM-ALL.METEO-NONE.720_360.monthly.{2015,2016,2017,2018,2019,2020}.nc \
		$@

$(FLUXCOM_NEE_MONTHLY_2x25): \
	$(GEOS_2X25_GRID)
	cdo -v -z zip_6 \
		-remapcon,$(GEOS_2X25_GRID) \
		-select,name=NEE,NEE_mad \
		$(FLUXCOM_DIRECTORY)/NEE.RS_V006.FP-NONE.MLM-ALL.METEO-NONE.720_360.monthly.{2015,2016,2017,2018,2019,2020}.nc \
		$@

$(6_RESULTS_SIF_INTERMEDIATES_DIR)/six-year-average-%.fst: \
	$(6_RESULTS_SIF_SRC_DIR)/six-year-average.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	4_inversion/intermediates/samples-%.rds \
	$(UTILS_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples 4_inversion/intermediates/samples-$*.rds \
		--output $@

$(OSSE_FLUX_AGGREGATES_ZONAL_SAMPLES_BASE)-%.rds: \
	$(6_RESULTS_SIF_SRC_DIR)/flux-aggregates-zonal-samples.R \
	$(OSSE_SAMPLES_BASE)-%.rds \
	$(PERTURBATIONS_AUGMENTED_SIF_ZONAL)
	Rscript $< $(OSSE_FLAGS_ALPHA) \
		--samples $(OSSE_SAMPLES_BASE)-$*.rds \
		--perturbations-augmented-zonal $(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
		--output $@

$(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-%.rds: \
	$(6_RESULTS_SIF_SRC_DIR)/flux-aggregates-samples.R \
	$(OSSE_SAMPLES_BASE)-%.rds \
	$(PERTURBATIONS_AUGMENTED_SIF)
	Rscript $< $(OSSE_FLAGS_ALPHA) \
		--samples $(OSSE_SAMPLES_BASE)-$*.rds \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--output $@

$(PERTURBATIONS_AUGMENTED_SIF_ZONAL): \
	$(6_RESULTS_SIF_SRC_DIR)/perturbations-augmented-zonal.R \
	$(BASIS_VECTORS) \
	$(CONTROL_EMISSIONS) \
	$(PERTURBATIONS) \
	$(AREA_1X1)
	Rscript $< \
		--basis-vectors $(BASIS_VECTORS) \
		--control-emissions $(CONTROL_EMISSIONS) \
		--perturbations $(PERTURBATIONS) \
		--area-1x1 $(AREA_1X1) \
		--output $@

$(PERTURBATIONS_AUGMENTED_SIF): \
	$(6_RESULTS_SIF_SRC_DIR)/perturbations-augmented.R \
	$(BASIS_VECTORS) \
	$(CONTROL_EMISSIONS) \
	$(PERTURBATIONS)
	Rscript $< \
		--basis-vectors $(BASIS_VECTORS) \
		--control-emissions $(CONTROL_EMISSIONS) \
		--perturbations $(PERTURBATIONS) \
		--output $@
