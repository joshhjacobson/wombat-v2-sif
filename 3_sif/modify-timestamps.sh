#!/bin/bash

touch 3_sif/intermediates/observations-sif.fst
sleep 1s

touch 3_sif/intermediates/sib4-hourly-sif-20{14..20}.nc
sleep 1s

touch 3_sif/intermediates/sib4-hourly-sif-assim-2x25-20{14..20}.nc
sleep 1s

touch 3_sif/intermediates/model-sif-assim.fst
sleep 1s

touch 3_sif/intermediates/oco2-hourly-sif.fst
sleep 1s

touch 3_sif/intermediates/sib4-hourly-climatology-inventory-assim-2x25-20{14..21}.nc
sleep 1s

touch 3_sif/intermediates/sib4-hourly-residual-assim-2x25-20{14..20}.nc
sleep 1s

touch 3_sif/intermediates/sensitivities-oco2-hourly-sif.fst
sleep 1s