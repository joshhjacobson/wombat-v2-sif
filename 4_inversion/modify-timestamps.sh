#!/bin/bash

touch 4_inversion/intermediates/observations.fst
sleep 1s

touch 4_inversion/intermediates/perturbations.fst
sleep 1s

touch 4_inversion/intermediates/basis-vectors.fst
sleep 1s

touch 4_inversion/intermediates/prior-base.rds
sleep 1s

touch 4_inversion/intermediates/control-emissions.fst
sleep 1s

touch 4_inversion/intermediates/constraints.rds
sleep 1s

touch 4_inversion/intermediates/prior.rds
sleep 1s

sensitivities_paths=$(find "4_inversion/intermediates" -name "sensitivities-*")

for path in $sensitivities_paths; do
    touch $path
done
sleep 1s

H_paths=$(find "4_inversion/intermediates" -name "H-*")

for target in $H_paths; do
    touch $target
done
sleep 1s

touch 4_inversion/intermediates/residual-1st-stage.fst
sleep 1s

touch 4_inversion/intermediates/hyperparameter-estimates.fst
sleep 1s

obs_paths=$(find "4_inversion/intermediates" -name "osse-observations-*")

for target in $obs_paths; do
    touch $target
done
sleep 1s
