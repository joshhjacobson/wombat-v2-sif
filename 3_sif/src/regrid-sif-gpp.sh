#!/bin/bash

# Combine 1-degree hourly SIF and GPP files and regrid to 2x2.5 degrees

set -aex

grid_file=$1
IFS=' ' read -ra gpp_input_files <<< "$2"
IFS=' ' read -ra sif_input_files <<< "$3"
output_directory=$4

if [ ${#gpp_input_files[@]} -ne ${#sif_input_files[@]} ]; then
    echo "Input lists for SIF and GPP files must be the same length."
    exit 1
fi

for i in "${!gpp_input_files[@]}"; do

    gpp_input_file=${gpp_input_files[$i]}
    sif_input_file=${sif_input_files[$i]}

    gpp_year=$(echo $gpp_input_file | grep -oP '(?<=-)\d+(?=.nc)')
    sif_year=$(echo $sif_input_file | grep -oP '(?<=-)\d+(?=.nc)')

    if [ $sif_year -ne $gpp_year ]; then
        echo "SIF and GPP yearly files must be aligned by year."
        exit 1
    fi
    echo $gpp_year

    output_file="${output_directory}/sib4-hourly-sif-gpp-2x25-${gpp_year}.nc"

    cdo -f nc2 \
        -remapcon,$grid_file \
        -merge \
        -selvar,sif \
        $sif_input_file \
        -selvar,assim \
        $gpp_input_file \
        $output_file

done