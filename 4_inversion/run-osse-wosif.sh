#!/bin/bash

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHASMALL-FIXRESP-WOSIF.rds
sleeps 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHASMALL-FREERESP-WOSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHALARGE-FIXRESP-WOSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace make 4_inversion/intermediates/osse-samples-ALPHALARGE-FREERESP-WOSIF.rds
sleep 5s