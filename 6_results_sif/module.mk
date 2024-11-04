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

XBASE_FLUXES = GPP TER NEE
XBASE_YEARS = 2015 2016 2017 2018 2019 2020
XBASE_TER_BASE = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/xbase_TER_unweighted
XBASE_TER_FILES = $(foreach YEAR,$(XBASE_YEARS),$(XBASE_TER_BASE)_$(YEAR).nc)
XBASE_MONTHLY_BASE = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/xbase_monthly
XBASE_MONTHLY_FILES = \
	$(foreach FLUX,$(XBASE_FLUXES),\
	$(foreach YEAR,$(XBASE_YEARS),\
	$(XBASE_MONTHLY_BASE)_$(FLUX)_$(YEAR).nc))
XBASE_MONTHLY_2x25_BASE = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/xbase-monthly-2x25
XBASE_MONTHLY_2x25_FILES = $(foreach FLUX,$(XBASE_FLUXES),$(XBASE_MONTHLY_2x25_BASE)-$(FLUX).nc)
XBASE_MONTHLY_2x25 = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/xbase-monthly-2x25.fst
XBASE_MONTHLY_2x25_ZONAL = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/xbase-monthly-2x25-zonal.fst

6_RESULTS_SIF_TARGETS += \
	$(6_RESULTS_SIF_FIGURES_DIR)/region-map.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/observation-count.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/sif-gpp-average-slope.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/sif-gpp-map-slope.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/sif-gpp-map-intercept.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/error-params-table.tex \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-true-fluxes.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-table-rmse.tex \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-table-crps.tex \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-annual-average-table.csv \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-global.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-zonal.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/seasonal-cycle-zonal.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/seasonal-latitude-profile.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-maps-gpp.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-maps-resp.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-maps-nee.pdf


REGIONS = global Region01 Region02 Region03 Region04 Region05 Region06 Region07 Region08 Region09 Region10 Region11
FLUX_DECOMPOSITIONS = \
	$(foreach REGION,$(REGIONS),$(6_RESULTS_SIF_FIGURES_DIR)/flux-decomposition-$(REGION).pdf)
6_RESULTS_SIF_TARGETS += $(FLUX_DECOMPOSITIONS)

RESP_TYPES = FIXRESP FREERESP
OSSE_FLUX_DECOMPOSITIONS = \
	$(foreach RESP_TYPE,$(RESP_TYPES),\
	$(foreach REGION,$(REGIONS),\
	$(foreach OSSE_CASE,$(OSSE_BASE_CASES),\
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-$(REGION)_$(OSSE_CASE)-$(RESP_TYPE).pdf)))
OSSE_FLUX_DECOMPOSITIONS += \
	$(foreach REGION,$(REGIONS),\
	$(foreach OSSE_CASE,$(OSSE_BASE_CASES),\
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-$(REGION)_$(OSSE_CASE)-cross.pdf))
6_RESULTS_SIF_TARGETS += $(OSSE_FLUX_DECOMPOSITIONS)


## Products


## Figures

$(6_RESULTS_SIF_FIGURES_DIR)/osse-true-fluxes.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-true-fluxes.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(OSSE_ADJUSTED_ALPHAS) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--alpha-v2 $(ALPHA_WOMBAT_V2) \
		--alpha-positive $(ALPHA_ADJUSTMENT_BASE)-positive.fst \
		--alpha-negative $(ALPHA_ADJUSTMENT_BASE)-negative.fst \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-table.tex: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-table-old.R \
	$(OSSE_FLUX_AGGREGATES_SAMPLES_CASES)
	Rscript $< \
		--flux-samples-alpha0-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHA0-FIXRESP-WOSIF.rds \
		--flux-samples-alpha0-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHA0-FREERESP-WSIF.rds \
		--flux-samples-alphav2-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAV2-FIXRESP-WOSIF.rds \
		--flux-samples-alphav2-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAV2-FREERESP-WSIF.rds \
		--flux-samples-alphap-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAP-FIXRESP-WOSIF.rds \
		--flux-samples-alphap-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAP-FREERESP-WSIF.rds \
		--flux-samples-alphan-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAN-FIXRESP-WOSIF.rds \
		--flux-samples-alphan-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAN-FREERESP-WSIF.rds \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-table-full.tex: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-table-full-old.R \
	$(OSSE_FLUX_AGGREGATES_SAMPLES_CASES)
	Rscript $< \
		--flux-samples-alpha0-fixresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHA0-FIXRESP-WSIF.rds \
		--flux-samples-alpha0-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHA0-FIXRESP-WOSIF.rds \
		--flux-samples-alpha0-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHA0-FREERESP-WSIF.rds \
		--flux-samples-alpha0-freeresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHA0-FREERESP-WOSIF.rds \
		--flux-samples-alphav2-fixresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAV2-FIXRESP-WSIF.rds \
		--flux-samples-alphav2-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAV2-FIXRESP-WOSIF.rds \
		--flux-samples-alphav2-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAV2-FREERESP-WSIF.rds \
		--flux-samples-alphav2-freeresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAV2-FREERESP-WOSIF.rds \
		--flux-samples-alphap-fixresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAP-FIXRESP-WSIF.rds \
		--flux-samples-alphap-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAP-FIXRESP-WOSIF.rds \
		--flux-samples-alphap-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAP-FREERESP-WSIF.rds \
		--flux-samples-alphap-freeresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAP-FREERESP-WOSIF.rds \
		--flux-samples-alphan-fixresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAN-FIXRESP-WSIF.rds \
		--flux-samples-alphan-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAN-FIXRESP-WOSIF.rds \
		--flux-samples-alphan-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAN-FREERESP-WSIF.rds \
		--flux-samples-alphan-freeresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAN-FREERESP-WOSIF.rds \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-table-%.tex: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-table.R \
	$(OSSE_FLUX_AGGREGATES_SAMPLES_CASES)
	Rscript $< \
		--metric $* \
		--flux-samples-alpha0-fixresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHA0-FIXRESP-WSIF.rds \
		--flux-samples-alpha0-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHA0-FIXRESP-WOSIF.rds \
		--flux-samples-alpha0-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHA0-FREERESP-WSIF.rds \
		--flux-samples-alpha0-freeresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHA0-FREERESP-WOSIF.rds \
		--flux-samples-alphav2-fixresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAV2-FIXRESP-WSIF.rds \
		--flux-samples-alphav2-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAV2-FIXRESP-WOSIF.rds \
		--flux-samples-alphav2-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAV2-FREERESP-WSIF.rds \
		--flux-samples-alphav2-freeresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAV2-FREERESP-WOSIF.rds \
		--flux-samples-alphap-fixresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAP-FIXRESP-WSIF.rds \
		--flux-samples-alphap-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAP-FIXRESP-WOSIF.rds \
		--flux-samples-alphap-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAP-FREERESP-WSIF.rds \
		--flux-samples-alphap-freeresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAP-FREERESP-WOSIF.rds \
		--flux-samples-alphan-fixresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAN-FIXRESP-WSIF.rds \
		--flux-samples-alphan-fixresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAN-FIXRESP-WOSIF.rds \
		--flux-samples-alphan-freeresp-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAN-FREERESP-WSIF.rds \
		--flux-samples-alphan-freeresp-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-ALPHAN-FREERESP-WOSIF.rds \
		--output $@

# TODO: If keeping these figures, add intervals and maybe do cross-comparison
$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-monthly.R \
	$(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-%-FIXRESP-WSIF.rds \
	$(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-%-FIXRESP-WOSIF.rds \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--flux-samples-wsif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-$*-FIXRESP-WSIF.rds \
		--flux-samples-wosif $(OSSE_FLUX_AGGREGATES_SAMPLES_BASE)-$*-FIXRESP-WOSIF.rds \
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

$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-%-FIXRESP.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-flux-decomposition.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(OSSE_SAMPLES_CASES) \
	$(DISPLAY_PARTIAL)
	Rscript $< $(OSSE_FLAGS_ALPHA) \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-fixresp-wsif $(OSSE_SAMPLES_BASE)-$(lastword $(subst _, ,$*))-FIXRESP-WSIF.rds \
		--samples-fixresp-wosif $(OSSE_SAMPLES_BASE)-$(lastword $(subst _, ,$*))-FIXRESP-WOSIF.rds \
		--region $(firstword $(subst _, ,$*)) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-%-FREERESP.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-flux-decomposition.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(OSSE_SAMPLES_CASES) \
	$(DISPLAY_PARTIAL)
	Rscript $< $(OSSE_FLAGS_ALPHA) \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-freeresp-wsif $(OSSE_SAMPLES_BASE)-$(lastword $(subst _, ,$*))-FREERESP-WSIF.rds \
		--samples-freeresp-wosif $(OSSE_SAMPLES_BASE)-$(lastword $(subst _, ,$*))-FREERESP-WOSIF.rds \
		--region $(firstword $(subst _, ,$*)) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-flux-decomposition-%-cross.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-flux-decomposition.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(OSSE_SAMPLES_CASES) \
	$(DISPLAY_PARTIAL)
	Rscript $< $(OSSE_FLAGS_ALPHA) \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
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
	2_matching/intermediates/runs/base/oco2-hourly.fst \
	2_matching/intermediates/runs/base/obspack-hourly-assim-1.fst \
	3_sif/intermediates/oco2-hourly-sif.fst \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--observations $(OBSERVATIONS) \
		--control \
			2_matching/intermediates/runs/base/oco2-hourly.fst \
			2_matching/intermediates/runs/base/obspack-hourly-assim-1.fst \
			3_sif/intermediates/oco2-hourly-sif.fst \
		--region-sf $(REGION_SF_SIF) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/sif-gpp-average-slope.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/sif-gpp-average-slope.R \
	$(MODEL_SIF_ASSIM) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--model-sif-assim $(MODEL_SIF_ASSIM) \
		--region-sf $(REGION_SF_SIF) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/sif-gpp-map-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/sif-gpp-map-model.R \
	$(MODEL_SIF_ASSIM) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--model-sif-assim $(MODEL_SIF_ASSIM) \
		--region-sf $(REGION_SF_SIF) \
		--term $* \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-annual-average-table.csv \
	$(6_RESULTS_SIF_SRC_DIR)/flux-annual-average.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(SAMPLES_WOMBAT_V2) \
	$(SAMPLES_LNLGISSIF) \
	$(XBASE_MONTHLY_2x25)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_WOMBAT_V2) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--xbase-monthly-2x25 $(XBASE_MONTHLY_2x25) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-global.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/flux-net-global-seasonal.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(SAMPLES_WOMBAT_V2) \
	$(SAMPLES_LNLGISSIF) \
	$(XBASE_MONTHLY_2x25) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_WOMBAT_V2) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--xbase-monthly-2x25 $(XBASE_MONTHLY_2x25) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-global.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/flux-net-global.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(SAMPLES_WOMBAT_V2) \
	$(SAMPLES_LNLGISSIF) \
	$(XBASE_MONTHLY_2x25) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_WOMBAT_V2) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--xbase-monthly-2x25 $(XBASE_MONTHLY_2x25) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-zonal.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/flux-net-zonal.R \
	$(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
	$(SAMPLES_WOMBAT_V2) \
	$(SAMPLES_LNLGISSIF) \
	$(XBASE_MONTHLY_2x25_ZONAL) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented-zonal $(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
		--samples-LNLGIS $(SAMPLES_WOMBAT_V2) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--xbase-monthly-2x25-zonal $(XBASE_MONTHLY_2x25_ZONAL) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/seasonal-cycle-global.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/seasonal-cycle-global.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(SAMPLES_WOMBAT_V2) \
	$(SAMPLES_LNLGISSIF) \
	$(XBASE_MONTHLY_2x25) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_WOMBAT_V2) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--xbase-monthly-2x25 $(XBASE_MONTHLY_2x25) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/seasonal-cycle-zonal.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/seasonal-cycle-zonal.R \
	$(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
	$(SAMPLES_WOMBAT_V2) \
	$(SAMPLES_LNLGISSIF) \
	$(XBASE_MONTHLY_2x25_ZONAL) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented-zonal $(PERTURBATIONS_AUGMENTED_SIF_ZONAL) \
		--samples-LNLGIS $(SAMPLES_WOMBAT_V2) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--xbase-monthly-2x25-zonal $(XBASE_MONTHLY_2x25_ZONAL) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/seasonal-latitude-profile.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/seasonal-latitude-profile.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(SAMPLES_WOMBAT_V2) \
	$(SAMPLES_LNLGISSIF) \
	$(XBASE_MONTHLY_2x25) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_WOMBAT_V2) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--xbase-monthly-2x25 $(XBASE_MONTHLY_2x25) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-decomposition-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/flux-decomposition.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(SAMPLES_WOMBAT_V2) \
	$(SAMPLES_LNLGISSIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_WOMBAT_V2) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--region $* \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/average-maps-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/average-maps.R \
	$(SIX_YEAR_AVERAGE_SIF) \
	$(REGION_SF_SIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--six-year-average $(SIX_YEAR_AVERAGE_SIF) \
		--flux-component $* \
		--region-sf $(REGION_SF_SIF) \
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
	$(REGION_SF_SIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--six-year-average $(SIX_YEAR_AVERAGE_SIF) \
		--flux-component $* \
		--region-sf $(REGION_SF_SIF) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/error-params-table.tex: \
	$(6_RESULTS_SIF_SRC_DIR)/error-params-table.R \
	$(HYPERPARAMETER_ESTIMATES)
	Rscript $< \
		--hyperparameter-estimates $(HYPERPARAMETER_ESTIMATES) \
		--output $@

## Intermediates

$(SIX_YEAR_AVERAGE_SIF): \
	$(6_RESULTS_SIF_SRC_DIR)/six-year-average.R \
	$(PERTURBATIONS_AUGMENTED_SIF) \
	$(SAMPLES_WOMBAT_V2) \
	$(SAMPLES_LNLGISSIF) \
	$(XBASE_MONTHLY_2x25) \
	$(UTILS_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED_SIF) \
		--samples-LNLGIS $(SAMPLES_WOMBAT_V2) \
		--samples-LNLGISSIF $(SAMPLES_LNLGISSIF) \
		--xbase-monthly-2x25 $(XBASE_MONTHLY_2x25) \
		--output $@

$(XBASE_MONTHLY_2x25_ZONAL): \
	$(6_RESULTS_SIF_SRC_DIR)/xbase-monthly-zonal.R \
	$(XBASE_MONTHLY_2x25) \
	$(AREA_1X1)
	Rscript $< \
		--xbase-monthly-2x25 $(XBASE_MONTHLY_2x25) \
		--area-1x1 $(AREA_1X1) \
		--output $@

$(XBASE_MONTHLY_2x25): \
	$(6_RESULTS_SIF_SRC_DIR)/xbase-monthly.R \
	$(XBASE_MONTHLY_2x25_FILES) \
	$(CONTROL_EMISSIONS)
	Rscript $< \
		--input-files $(XBASE_MONTHLY_2x25_FILES) \
		--control-emissions $(CONTROL_EMISSIONS) \
		--output $@

$(XBASE_MONTHLY_2x25_BASE)-%.nc: \
	$(GEOS_2X25_GRID) \
	$(XBASE_05X05_GRID) \
	$(XBASE_MONTHLY_FILES)
	cdo -v -z zip_6 \
		-setctomiss,0 \
		-remapcon,$(GEOS_2X25_GRID) \
		-setgrid,$(XBASE_05X05_GRID) \
		-select,name=$* \
		$(XBASE_MONTHLY_BASE)_$*_{2015,2016,2017,2018,2019,2020}.nc \
		$@

$(XBASE_MONTHLY_BASE)_NEE_%.nc:
	cdo -v -z zip_6 \
		-mul \
		-selvar,NEE \
		$(XBASE_DIRECTORY)/NEE_$*_050_monthly.nc \
		-selvar,land_fraction \
		$(XBASE_DIRECTORY)/NEE_$*_050_monthly.nc \
		$@

$(XBASE_MONTHLY_BASE)_GPP_%.nc:
	cdo -v -z zip_6 \
		-mul \
		-selvar,GPP \
		$(XBASE_DIRECTORY)/GPP_$*_050_monthly.nc \
		-selvar,land_fraction \
		$(XBASE_DIRECTORY)/GPP_$*_050_monthly.nc \
		$@

$(XBASE_MONTHLY_BASE)_TER_%.nc: \
	$(XBASE_TER_FILES)
	cdo -v -z zip_6 \
		-mul \
		-selvar,TER \
		$(XBASE_TER_BASE)_$*.nc \
		-selvar,land_fraction \
		$(XBASE_DIRECTORY)/GPP_$*_050_monthly.nc \
		$@

$(XBASE_TER_BASE)_%.nc:
	cdo -v -z zip_6 \
		-chname,NEE,TER \
		-add \
		-selvar,NEE \
		$(XBASE_DIRECTORY)/NEE_$*_050_monthly.nc \
		-selvar,GPP \
		$(XBASE_DIRECTORY)/GPP_$*_050_monthly.nc \
		$@

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

# NOTE(jhj): FLUXCOM MTE, GMDH_CV, and MARSens methods occasionally report
# non-physical values; we remove values outside the range of [-20, 20] gC/m^2/day
# TODO: make comparisons with X-BASE product instead?

$(FLUXCOM_MONTHLY_2x25_BASE)-%.nc: \
	$(GEOS_2X25_GRID) \
	$(FLUXCOM_05X05_GRID)
	cdo -v -z zip_6 \
		-setattribute,$(firstword $(subst -, ,$*))@method=$(lastword $(subst -, ,$*)) \
		-setvrange,0,20 \
		-setctomiss,0 \
		-remapcon,$(GEOS_2X25_GRID) \
		-setmisstoc,0 \
		-setgrid,$(FLUXCOM_05X05_GRID) \
		-select,name=$(firstword $(subst -, ,$*)) \
		$(FLUXCOM_DIRECTORY)/$(firstword $(subst -, ,$*)).$(lastword $(subst -, ,$*)).monthly.{2015,2016,2017,2018,2019,2020}.nc \
		$@

$(FLUXCOM_MONTHLY_2x25_BASE)-NEE-%.nc: \
	$(GEOS_2X25_GRID) \
	$(FLUXCOM_05X05_GRID)
	cdo -v -z zip_6 \
		-setattribute,NEE@method=$* \
		-setvrange,-20,20 \
		-setctomiss,0 \
		-remapcon,$(GEOS_2X25_GRID) \
		-setmisstoc,0 \
		-setgrid,$(FLUXCOM_05X05_GRID) \
		-select,name=NEE \
		$(FLUXCOM_DIRECTORY)/NEE.$*.monthly.{2015,2016,2017,2018,2019,2020}.nc \
		$@

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
