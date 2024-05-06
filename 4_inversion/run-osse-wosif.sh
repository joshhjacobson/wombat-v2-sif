#!/bin/bash

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHA0-WOSIF.rds
sleeps 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHAV2-WOSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHAFREE-WOSIF.rds
sleep 5s