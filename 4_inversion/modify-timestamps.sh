#!/bin/bash

my_targets=$(make -qp | awk -F':' '/^[a-zA-Z0-9][^$#\\\t=]*:([^=]|$)/ {split($1,A,/ /);for(i in A)print A[i]}')

touch 4_inversion/intermediates/observations.fst
sleep 2s

for target in $(echo "$my_targets" | grep "4_inversion/intermediates/sensitivities-base-*"); do
    touch $target
done
sleep 2s

for target in $(echo "$my_targets" | grep "4_inversion/intermediates/sensitivities-r10-r15-rNZ-*"); do
    touch $target
done
sleep 2s

for target in $(echo "$my_targets" | grep "4_inversion/intermediates/H-*"); do
    touch $target
done
sleep 2s

touch 4_inversion/intermediates/residual-1st-stage.fst
sleep 2s

touch 4_inversion/intermediates/hyperparameter-estimates.fst
sleep 2s