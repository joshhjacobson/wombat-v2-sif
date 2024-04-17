# NOTE: 5_RESULTS_SRC_DIR is used here too
6_RESULTS_SIF_SRC_DIR = 6_results_sif/src
6_RESULTS_SIF_INTERMEDIATES_DIR = 6_results_sif/intermediates
6_RESULTS_SIF_FIGURES_DIR = 6_results_sif/figures

$(shell mkdir -p $(6_RESULTS_SIF_INTERMEDIATES_DIR))
$(shell mkdir -p $(6_RESULTS_SIF_FIGURES_DIR))

# Intermediates
CLIMATOLOGY_BY_REGION_SIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/climatology-by-region.rds
TREND_GRID_SIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/trend-grid.fst
SIX_YEAR_AVERAGE_SIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/six-year-average.fst

OSSE_RESULTS_TABLE = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/osse-results-table.txt


6_RESULTS_SIF_TARGETS += \
	$(6_RESULTS_SIF_FIGURES_DIR)/average-and-trend-map.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-global-zonal.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-land-transcoms.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-global-zonal-table.txt \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-land-transcoms-table.txt \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-map-nee.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-map-gpp.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-map-resp.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-map-nee-2.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-map-gpp-2.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-map-resp-2.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-map-nee-3.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-map-gpp-3.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-map-resp-3.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/trend-table.txt \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-components-global.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-components-n-boreal.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-components-n-temperate.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-components-tropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-components-n-tropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-components-s-tropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-components-s-extratropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-global.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-n-boreal.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-n-temperate.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-tropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-n-tropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-s-tropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-s-extratropical.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/el-nino-2015-residual-map.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/effective-sample-size.txt \
	$(6_RESULTS_SIF_FIGURES_DIR)/hyperparameter-table.tex \
	$(6_RESULTS_SIF_FIGURES_DIR)/ell-table.tex \


# $(6_RESULTS_SIF_FIGURES_DIR)/observation-map.pdf \
# $(6_RESULTS_SIF_FIGURES_DIR)/traceplots.pdf


$(OSSE_RESULTS_TABLE): \
	$(6_RESULTS_SIF_SRC_DIR)/osse-results-table.R \
	4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WSIF.rds \
	4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WOSIF.rds \
	4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WSIF.rds \
	4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WOSIF.rds
	Rscript $< \
		--flux-samples-alpha0-wsif 4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WSIF.rds \
		--flux-samples-alpha0-wosif 4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WOSIF.rds \
		--flux-samples-alphav2-wsif 4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WSIF.rds \
		--flux-samples-alphav2-wosif 4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WOSIF.rds \
		--output $@

## Figures

$(6_RESULTS_SIF_FIGURES_DIR)/average-and-trend-map.pdf: \
	$(5_RESULTS_SRC_DIR)/average-and-trend-map.R \
	$(TREND_GRID_SIF) \
	$(SIX_YEAR_AVERAGE_SIF) \
	$(REGION_SF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--climatology-trend $(TREND_GRID_SIF) \
		--six-year-average $(SIX_YEAR_AVERAGE_SIF) \
		--region-sf $(REGION_SF) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-%-table.txt: \
	$(5_RESULTS_SRC_DIR)/harmonic-shift-table.R \
	$(CLIMATOLOGY_BY_REGION_SIF) \
	$(SIB4_CLIMATOLOGY_ASSIM_2X25) \
	$(BASE_PARTIAL)
	Rscript $< \
		--regions $* \
		--climatology-by-region $(CLIMATOLOGY_BY_REGION_SIF) \
		--sib4-climatology-assim $(SIB4_CLIMATOLOGY_ASSIM_2X25) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-%.pdf: \
	$(5_RESULTS_SRC_DIR)/harmonic-shift.R \
	$(CLIMATOLOGY_BY_REGION_SIF) \
	$(SIB4_CLIMATOLOGY_ASSIM_2X25) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--regions $* \
		--region-sf $(REGION_SF) \
		--climatology-by-region $(CLIMATOLOGY_BY_REGION_SIF) \
		--sib4-climatology-assim $(SIB4_CLIMATOLOGY_ASSIM_2X25) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/harmonic-shift-map-%.pdf: \
	$(5_RESULTS_SRC_DIR)/harmonic-shift-map.R \
	$(CONTROL_EMISSIONS) \
	$(REGION_SF) \
	$(REGION_GRID) \
	$(SAMPLES_LNLGISSIF) \
	$(SIB4_CLIMATOLOGY_ASSIM_2X25) \
	$(SIB4_CLIMATOLOGY_RESP_TOT_2X25) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--control-emissions $(CONTROL_EMISSIONS) \
		--region-sf $(REGION_SF) \
		--region-grid $(REGION_GRID) \
		--samples $(SAMPLES_LNLGISSIF) \
		--sib4-climatology-assim $(SIB4_CLIMATOLOGY_ASSIM_2X25) \
		--sib4-climatology-resp-tot $(SIB4_CLIMATOLOGY_RESP_TOT_2X25) \
		--inventory-harmonic $* \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/trend-table.txt: \
	$(5_RESULTS_SRC_DIR)/trend-table.R \
	$(CLIMATOLOGY_BY_REGION_SIF) \
	$(BASE_PARTIAL)
	Rscript $< \
		--climatology-by-region $(CLIMATOLOGY_BY_REGION_SIF) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-components-%.pdf: \
	$(5_RESULTS_SRC_DIR)/flux-components.R \
	$(AREA_1X1) \
	$(PERTURBATIONS_AUGMENTED) \
	$(SAMPLES_LNLGISSIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--area-1x1 $(AREA_1X1) \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--samples $(SAMPLES_LNLGISSIF) \
		--region $* \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-components-%.pdf: \
	$(5_RESULTS_SRC_DIR)/flux-components.R \
	$(AREA_1X1) \
	$(PERTURBATIONS_AUGMENTED) \
	$(SAMPLES_LNLGISSIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--area-1x1 $(AREA_1X1) \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--samples $(SAMPLES_LNLGISSIF) \
		--region $* \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/flux-net-%.pdf: \
	$(5_RESULTS_SRC_DIR)/flux-net.R \
	$(AREA_1X1) \
	$(PERTURBATIONS_AUGMENTED) \
	$(SAMPLES_LNLGISSIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--area-1x1 $(AREA_1X1) \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--samples $(SAMPLES_LNLGISSIF) \
		--region $* \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/el-nino-2015-residual-map.pdf: \
	$(5_RESULTS_SRC_DIR)/el-nino-2015-residual-map.R \
	$(CONTROL_EMISSIONS) \
	$(PERTURBATIONS_AUGMENTED) \
	$(REGION_SF) \
	$(SAMPLES_LNLGISSIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--control-emissions $(CONTROL_EMISSIONS) \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--region-sf $(REGION_SF) \
		--samples $(SAMPLES_LNLGISSIF) \
		--output $@

# $(6_RESULTS_SIF_FIGURES_DIR)/observation-map.pdf: \
# 	$(5_RESULTS_SRC_DIR)/observation-map.R \
# 	$(OBSERVATIONS) \
# 	$(DISPLAY_PARTIAL)
# 	Rscript $< \
# 		--observations $(OBSERVATIONS) \
# 		--output $@

# $(6_RESULTS_SIF_FIGURES_DIR)/traceplots.pdf: \
# 	$(5_RESULTS_SRC_DIR)/traceplots.R \
# 	$(SAMPLES_LNLGISSIF) \
# 	$(DISPLAY_PARTIAL)
# 	Rscript $< \
# 		--samples $(SAMPLES_LNLGISSIF) \
# 		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/effective-sample-size.txt: \
	$(5_RESULTS_SRC_DIR)/effective-sample-size.R \
	$(SAMPLES_LNLGISSIF) \
	$(BASE_PARTIAL)
	Rscript $< \
		--samples $(SAMPLES_LNLGISSIF) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/ell-table.tex: \
	$(5_RESULTS_SRC_DIR)/ell-table.R \
	$(HYPERPARAMETER_ESTIMATES) \
	$(BASE_PARTIAL)
	Rscript $< \
		--hyperparameter-estimates $(HYPERPARAMETER_ESTIMATES) \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/hyperparameter-table.tex: \
	$(5_RESULTS_SRC_DIR)/hyperparameter-table.R \
	$(SAMPLES_LNLGISSIF) \
	$(BASE_PARTIAL)
	Rscript $< \
		--samples $(SAMPLES_LNLGISSIF) \
		--output $@

## Intermediates

$(SIX_YEAR_AVERAGE_SIF): \
	$(5_RESULTS_SRC_DIR)/six-year-average.R \
	$(CONTROL_EMISSIONS) \
	$(PERTURBATIONS_AUGMENTED) \
	$(SAMPLES_LNLGISSIF) \
	$(UTILS_PARTIAL)
	Rscript $< \
		--control-emissions $(CONTROL_EMISSIONS) \
		--samples $(SAMPLES_LNLGISSIF) \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--output $@

$(TREND_GRID_SIF): \
	$(5_RESULTS_SRC_DIR)/trend-grid.R \
	$(CONTROL_EMISSIONS) \
	$(REGION_GRID) \
	$(SAMPLES_LNLGISSIF) \
	$(SIB4_CLIMATOLOGY_ASSIM_2X25) \
	$(SIB4_CLIMATOLOGY_RESP_TOT_2X25) \
	$(UTILS_PARTIAL)
	Rscript $< \
		--control-emissions $(CONTROL_EMISSIONS) \
		--region-grid $(REGION_GRID) \
		--samples $(SAMPLES_LNLGISSIF) \
		--sib4-climatology-assim $(SIB4_CLIMATOLOGY_ASSIM_2X25) \
		--sib4-climatology-resp-tot $(SIB4_CLIMATOLOGY_RESP_TOT_2X25) \
		--output $@

$(CLIMATOLOGY_BY_REGION_SIF): \
	$(5_RESULTS_SRC_DIR)/climatology-by-region.R \
	$(CONTROL_EMISSIONS) \
	$(REGION_GRID) \
	$(SAMPLES_LNLGISSIF) \
	$(SIB4_CLIMATOLOGY_ASSIM_2X25) \
	$(SIB4_CLIMATOLOGY_RESP_TOT_2X25) \
	$(LANDSCHUTZER_CLIMATOLOGY_2X25) \
	$(UTILS_PARTIAL)
	Rscript $< \
		--control-emissions $(CONTROL_EMISSIONS) \
		--region-grid $(REGION_GRID) \
		--samples $(SAMPLES_LNLGISSIF) \
		--sib4-climatology-assim $(SIB4_CLIMATOLOGY_ASSIM_2X25) \
		--sib4-climatology-resp-tot $(SIB4_CLIMATOLOGY_RESP_TOT_2X25) \
		--landschutzer-climatology $(LANDSCHUTZER_CLIMATOLOGY_2X25) \
		--output $@
