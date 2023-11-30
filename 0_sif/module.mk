$(shell mkdir -p 0_sif/intermediates)

OCO2_OBSERVATIONS_SIF = 0_sif/intermediates/observations_sif.fst


$(OBSERVATIONS): \
	0_sif/src/observations-sif.R \
	$(OCO2_OBSERVATIONS_SIF)
	Rscript $< \
		--oco2-sif-directory $(OCO2_SIF_DIRECTORY) \
		--start-date $(INVERSION_START_DATE) \
		--end-date $(INVERSION_END_DATE) \
		--output $@