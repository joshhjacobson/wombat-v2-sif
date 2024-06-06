#!/bin/bash

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHASMALL-FIXRESP-WSIF.rds
sleeps 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHASMALL-FREERESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHALARGE-FIXRESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHALARGE-FREERESP-WSIF.rds
sleep 5s