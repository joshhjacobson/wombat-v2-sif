#!/bin/bash

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHASMALL-FREERESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHASMALL-FIXRESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHA0-FIXRESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHA0-FREERESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHAV2-FIXRESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHAV2-FREERESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHAMD-FIXRESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHAMD-FREERESP-WSIF.rds
sleep 5s