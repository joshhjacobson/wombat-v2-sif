6_RESULTS_SIF_SRC_DIR = 5_results/src
6_RESULTS_SIF_INTERMEDIATES_DIR = 5_results/intermediates
6_RESULTS_SIF_FIGURES_DIR = 5_results/figures
6_RESULTS_SIF_PRODUCTS_DIR = 5_results/products

$(shell mkdir -p $(6_RESULTS_SIF_INTERMEDIATES_DIR))
$(shell mkdir -p $(6_RESULTS_SIF_FIGURES_DIR))
$(shell mkdir -p $(6_RESULTS_SIF_PRODUCTS_DIR))


TRACEPLOTS = $(6_RESULTS_SIF_FIGURES_DIR)/traceplots.pdf


$(6_RESULTS_FIGURES_DIR)/traceplots.pdf: \
	$(6_RESULTS_SRC_DIR)/traceplots.R \
	$(SAMPLES_LNLGISSIF) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--samples $(SAMPLES_LNLGISSIF) \
		--output $@