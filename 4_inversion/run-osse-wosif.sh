#!/bin/bash

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHASMALL-FREERESP-WOSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHASMALL-FIXRESP-WOSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHA0-FIXRESP-WOSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHA0-FREERESP-WOSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHAV2-FIXRESP-WOSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHAV2-FREERESP-WOSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHAMD-FIXRESP-WOSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHAMD-FREERESP-WOSIF.rds
sleep 5s