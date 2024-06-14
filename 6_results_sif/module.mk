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

REGION_GRID_SIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/region-grid.rds
REGION_SF_SIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/region-sf.rds
SIX_YEAR_AVERAGE_SIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/six-year-average.fst

# HB (non-HB) refers to daytime (nighttime) flux partioning
FLUXCOM_FLUXES = GPP GPP_HB TER TER_HB NEE
FLUXCOM_METHODS = ANNnoPFT GMDH_CV KRR MARSens MTE MTEM MTE_Viterbo RFmiss SVM
FLUXCOM_MONTHLY_2x25_BASE = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/fluxcom-monthly-2x25
FLUXCOM_MONTHLY_2x25_FILES = \
	$(foreach FLUX,$(FLUXCOM_FLUXES),\
	$(foreach METHOD,$(FLUXCOM_METHODS),\
	$(FLUXCOM_MONTHLY_2x25_BASE)-$(FLUX)-$(METHOD).nc))
FLUXCOM_MONTHLY_2x25 = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/fluxcom-monthly-2x25.fst
FLUXCOM_MONTHLY_2x25_ZONAL = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/fluxcom-monthly-2x25-zonal.fst

6_RESULTS_SIF_TARGETS += \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-true-fluxes.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-table.tex \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-ALPHA0.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-ALPHAV2.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-ALPHAMD.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-zonal-ALPHA0.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-zonal-ALPHAV2.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-zonal-ALPHAMD.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/region-map.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/observation-count.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-global.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-zonal.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/seasonal-cycle-global.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/seasonal-cycle-zonal.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-map-wombat-gpp.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-map-wombat-resp.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-map-wombat-nee.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-map-fluxcom-gpp.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-map-fluxcom-resp.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-map-fluxcom-nee.pdf \

# 6_RESULTS_SIF_TARGETS += $(OSSE_FLUX_DECOMPOSITIONS)


## Products


## Figures

$(6_RESULTS_SIF_FIGURES_DIR)/osse-true-fluxes.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-true-fluxes.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(ALPHA_SMALL) \
	$(ALPHA_MEDIUM) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--alpha-v2 $(ALPHA_WOMBAT_V2) \
		--alpha-small $(ALPHA_SMALL) \
		--alpha-large $(ALPHA_MEDIUM) \
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
		--flux-samples-alphasmall-fixresp-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHASMALL-FIXRESP-WSIF.rds \
		--flux-samples-alphasmall-fixresp-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHASMALL-FIXRESP-WOSIF.rds \
		--flux-samples-alphasmall-freeresp-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHASMALL-FREERESP-WSIF.rds \
		--flux-samples-alphasmall-freeresp-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHASMALL-FREERESP-WOSIF.rds \
		--flux-samples-alphalarge-fixresp-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAMD-FIXRESP-WSIF.rds \
		--flux-samples-alphalarge-fixresp-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAMD-FIXRESP-WOSIF.rds \
		--flux-samples-alphalarge-freeresp-wsif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAMD-FREERESP-WSIF.rds \
		--flux-samples-alphalarge-freeresp-wosif $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-flux-aggregates-samples-ALPHAMD-FREERESP-WOSIF.rds \
		--output $@

# TODO: If keeping these figures, add intervals and maybe do cross-comparison
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

# REGIONS = global n-boreal n-temperate tropical n-tropical s-tropical s-extratropical
REGIONS = global
RESP_TYPES = FIXRESP FREERESP
OSSE_FLUX_DECOMPOSITIONS = \
	$(foreach RESP_TYPE,$(RESP_TYPES),\
	$(foreach REGION,$(REGIONS),\
	$(foreach OSSE_CASE,$(OSSE_BASE_CASES),\
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-$(REGION)_$(OSSE_CASE)-$(RESP_TYPE).pdf)))
OSSE_FLUX_DECOMPOSITIONS += \
	$(foreach REGION,$(REGIONS),\
	$(foreach OSSE_CASE,$(OSSE_BASE_CASES),\
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-$(REGION)_$(OSSE_CASE).pdf))

$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-%-FIXRESP.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-flux-decomposition.R \
	$(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
	$(OSSE_SAMPLES_CASES) \
	$(DISPLAY_PARTIAL)
	Rscript $< $(OSSE_FLAGS_ALPHA) \
		--perturbations-augmented-zonal $(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
		--samples-wsif $(OSSE_SAMPLES_BASE)-$(lastword $(subst _, ,$*))-FIXRESP-WSIF.rds \
		--samples-wosif $(OSSE_SAMPLES_BASE)-$(lastword $(subst _, ,$*))-FIXRESP-WOSIF.rds \
		--region $(firstword $(subst _, ,$*)) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-%-FREERESP.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-flux-decomposition.R \
	$(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
	$(OSSE_SAMPLES_CASES) \
	$(DISPLAY_PARTIAL)
	Rscript $< $(OSSE_FLAGS_ALPHA) \
		--perturbations-augmented-zonal $(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
		--samples-wsif $(OSSE_SAMPLES_BASE)-$(lastword $(subst _, ,$*))-FREERESP-WSIF.rds \
		--samples-wosif $(OSSE_SAMPLES_BASE)-$(lastword $(subst _, ,$*))-FREERESP-WOSIF.rds \
		--region $(firstword $(subst _, ,$*)) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-%-mixed.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-flux-decomposition.R \
	$(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
	$(OSSE_SAMPLES_CASES) \
	$(DISPLAY_PARTIAL)
	Rscript $< $(OSSE_FLAGS_ALPHA) \
		--perturbations-augmented-zonal $(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
		--samples-fixresp-wosif $(OSSE_SAMPLES_BASE)-$(lastword $(subst _, ,$*))-FIXRESP-WOSIF.rds \
		--samples-freeresp-wsif $(OSSE_SAMPLES_BASE)-$(lastword $(subst _, ,$*))-FREERESP-WSIF.rds \
		--region $(firstword $(subst _, ,$*)) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/region-map.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/region-map.R \
	$(REGION_SF_SIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--region-sf $(REGION_SF_SIF) \
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
	$(FLUXCOM_MONTHLY_2x25) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_LNLGIS) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--fluxcom-monthly-2x25 $(FLUXCOM_MONTHLY_2x25) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-zonal.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/flux-net-zonal.R \
	$(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
	$(SAMPLES_LNLGIS) \
	$(SAMPLES_LNLGISSIF) \
	$(FLUXCOM_MONTHLY_2x25_ZONAL) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented-zonal $(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
		--samples-LNLGIS $(SAMPLES_LNLGIS) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--fluxcom-monthly-2x25-zonal $(FLUXCOM_MONTHLY_2x25_ZONAL) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/seasonal-cycle-global.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/seasonal-cycle-global.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(SAMPLES_LNLGIS) \
	$(SAMPLES_LNLGISSIF) \
	$(FLUXCOM_MONTHLY_2x25) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_LNLGIS) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--fluxcom-monthly-2x25 $(FLUXCOM_MONTHLY_2x25) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/seasonal-cycle-zonal.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/seasonal-cycle-zonal.R \
	$(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
	$(SAMPLES_LNLGIS) \
	$(SAMPLES_LNLGISSIF) \
	$(FLUXCOM_MONTHLY_2x25_ZONAL) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented-zonal $(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
		--samples-LNLGIS $(SAMPLES_LNLGIS) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--fluxcom-monthly-2x25-zonal $(FLUXCOM_MONTHLY_2x25_ZONAL) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/average-map-wombat-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/average-map-wombat.R \
	$(SIX_YEAR_AVERAGE_SIF) \
	$(REGION_SF_SIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--six-year-average $(SIX_YEAR_AVERAGE_SIF) \
		--flux-component $* \
		--region-sf $(REGION_SF_SIF) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/average-map-fluxcom-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/average-map-fluxcom.R \
	$(SIX_YEAR_AVERAGE_SIF) \
	$(FLUXCOM_MONTHLY_2x25) \
	$(REGION_SF_SIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--six-year-average $(SIX_YEAR_AVERAGE_SIF) \
		--fluxcom-monthly-2x25 $(FLUXCOM_MONTHLY_2x25) \
		--flux-component $* \
		--region-sf $(REGION_SF_SIF) \
		--output $@

## Intermediates

$(FLUXCOM_MONTHLY_2x25_ZONAL): \
	$(6_RESULTS_SIF_SRC_DIR)/fluxcom-monthly-zonal.R \
	$(FLUXCOM_MONTHLY_2x25) \
	$(AREA_1X1)
	Rscript $< \
		--fluxcom-monthly-2x25 $(FLUXCOM_MONTHLY_2x25) \
		--area-1x1 $(AREA_1X1) \
		--output $@

$(FLUXCOM_MONTHLY_2x25): \
	$(6_RESULTS_SIF_SRC_DIR)/fluxcom-monthly.R \
	$(FLUXCOM_MONTHLY_2x25_FILES) \
	$(CONTROL_EMISSIONS)
	Rscript $< \
		--input-files $(FLUXCOM_MONTHLY_2x25_FILES) \
		--control-emissions $(CONTROL_EMISSIONS) \
		--output $@

$(FLUXCOM_MONTHLY_2x25_BASE)-%.nc: \
	$(GEOS_2X25_GRID) \
	$(FLUXCOM_05X05_GRID)
	cdo -v -z zip_6 \
		-remapcon,$(GEOS_2X25_GRID) \
		-setattribute,$(firstword $(subst -, ,$*))@method=$(lastword $(subst -, ,$*)) \
		-select,name=$(firstword $(subst -, ,$*)) \
		-setgrid,$(FLUXCOM_05X05_GRID) \
		$(FLUXCOM_DIRECTORY)/$(firstword $(subst -, ,$*)).$(lastword $(subst -, ,$*)).monthly.{2015,2016,2017,2018,2019,2020}.nc \
		$@

$(SIX_YEAR_AVERAGE_SIF): \
	$(6_RESULTS_SIF_SRC_DIR)/six-year-average.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(SAMPLES_LNLGIS) \
	$(SAMPLES_LNLGISSIF) \
	$(UTILS_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_LNLGIS) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
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

$(REGION_SF_SIF): \
	$(6_RESULTS_SIF_SRC_DIR)/region-sf.R \
	$(REGION_GRID)
	Rscript $< \
		--region-grid $(REGION_GRID_SIF) \
		--output $@

$(REGION_GRID_SIF): \
	$(6_RESULTS_SIF_SRC_DIR)/region-grid.R \
	$(TRANSCOM_MASK_GEOS_2X25)
	Rscript $< \
		--transcom-grid $(TRANSCOM_MASK_GEOS_2X25) \
		--output $@
