#!/bin/bash

# Regrid each component of the basis function decomposition

set -aex

source /etc/profile.d/conda.sh
conda activate ./.conda_env

grid_file=$1
IFS=' ' read -ra input_files <<< "$2"
output_directory=$3

# Can we do this in parallel?

for i in "${!input_files[@]}"; do

    input_file=${input_files[$i]}
    basename=$(basename "$(echo $input_file | grep -oP '.*(?=-\d+\.nc)')")
    year=$(echo $input_file | grep -oP '(?<=-)\d+(?=.nc)')
    echo $year

    output_file="${output_directory}/${basename}-2x25-${year}.nc"

    cdo -f nc2 -remapcon,$grid_file $input_file $output_file

done