#!/bin/bash

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace RCPP_CACHE_DIR=rcpp-cache make 4_inversion/intermediates/osse-samples-ALPHA0-FREERESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace RCPP_CACHE_DIR=rcpp-cache make 4_inversion/intermediates/osse-samples-ALPHAV2-FREERESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace RCPP_CACHE_DIR=rcpp-cache make 4_inversion/intermediates/osse-samples-ALPHAP-FREERESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace RCPP_CACHE_DIR=rcpp-cache make 4_inversion/intermediates/osse-samples-ALPHAN-FREERESP-WSIF.rds
sleep 5s


OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace RCPP_CACHE_DIR=rcpp-cache make 4_inversion/intermediates/osse-samples-ALPHA0-FIXRESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace RCPP_CACHE_DIR=rcpp-cache make 4_inversion/intermediates/osse-samples-ALPHAV2-FIXRESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace RCPP_CACHE_DIR=rcpp-cache make 4_inversion/intermediates/osse-samples-ALPHAP-FIXRESP-WSIF.rds
sleep 5s

OMP_NUM_THREADS=16 WOMBAT_LOG_LEVEL=trace RCPP_CACHE_DIR=rcpp-cache make 4_inversion/intermediates/osse-samples-ALPHAN-FIXRESP-WSIF.rds
sleep 5s
