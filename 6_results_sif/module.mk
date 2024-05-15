6_RESULTS_SIF_SRC_DIR = 6_results_sif/src
6_RESULTS_SIF_INTERMEDIATES_DIR = 6_results_sif/intermediates
6_RESULTS_SIF_FIGURES_DIR = 6_results_sif/figures
6_RESULTS_SIF_PRODUCTS_DIR = 6_results_sif/products

$(shell mkdir -p $(6_RESULTS_SIF_INTERMEDIATES_DIR))
$(shell mkdir -p $(6_RESULTS_SIF_FIGURES_DIR))
$(shell mkdir -p $(6_RESULTS_SIF_PRODUCTS_DIR))


CLIMATOLOGY_BY_REGION_SIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/climatology-by-region.rds
TREND_GRID_SIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/trend-grid.fst
SIX_YEAR_AVERAGE_SIF = $(6_RESULTS_SIF_INTERMEDIATES_DIR)/six-year-average.fst

6_RESULTS_SIF_TARGETS += \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-table.tex \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-ALPHA0.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-ALPHAV2.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-ALPHAFREE.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-regional-ALPHA0.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-regional-ALPHAV2.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-regional-ALPHAFREE.pdf \
	$(6_RESULTS_SIF_FIGURES_DIR)/observation-count.pdf


## Products


## Figures

# TODO: convert to \textbf{}
$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-table.tex: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-table.R \
	4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WSIF.rds \
	4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WOSIF.rds \
	4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WSIF.rds \
	4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WOSIF.rds \
	4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAFREE-WSIF.rds \
	4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAFREE-WOSIF.rds
	Rscript $< \
		--flux-samples-alpha0-wsif 4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WSIF.rds \
		--flux-samples-alpha0-wosif 4_inversion/intermediates/osse-flux-aggregates-samples-ALPHA0-WOSIF.rds \
		--flux-samples-alphav2-wsif 4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WSIF.rds \
		--flux-samples-alphav2-wosif 4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAV2-WOSIF.rds \
		--flux-samples-alphafree-wsif 4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAFREE-WSIF.rds \
		--flux-samples-alphafree-wosif 4_inversion/intermediates/osse-flux-aggregates-samples-ALPHAFREE-WOSIF.rds \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-monthly-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-monthly.R \
	4_inversion/intermediates/osse-flux-aggregates-samples-%-WSIF.rds \
	4_inversion/intermediates/osse-flux-aggregates-samples-%-WOSIF.rds \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--flux-samples-wsif 4_inversion/intermediates/osse-flux-aggregates-samples-$*-WSIF.rds \
		--flux-samples-wosif 4_inversion/intermediates/osse-flux-aggregates-samples-$*-WOSIF.rds \
		--osse-base-case $* \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/osse-metrics-regional-%.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/osse-metrics-regional.R \
	4_inversion/intermediates/osse-flux-aggregates-samples-%-WSIF.rds \
	4_inversion/intermediates/osse-flux-aggregates-samples-%-WOSIF.rds \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--flux-samples-wsif 4_inversion/intermediates/osse-flux-aggregates-samples-$*-WSIF.rds \
		--flux-samples-wosif 4_inversion/intermediates/osse-flux-aggregates-samples-$*-WOSIF.rds \
		--osse-base-case $* \
		--output $@

$(6_RESULTS_SIF_FIGURES_DIR)/observation-count.pdf: \
	$(6_RESULTS_SIF_SRC_DIR)/observation-count.R \
	$(OBSERVATIONS) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--observations $(OBSERVATIONS) \
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
