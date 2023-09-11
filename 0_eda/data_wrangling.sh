#!/bin/bash
# CDO Commands for Processing SiB4 Hourly Files

printf "NOTES: \n"
printf "[] Best to run this script on Node 5 since the SiB4 files are stored there.\n"
printf "[] This script will take a while to run. It is recommended to run this script in a screen session.\n"


## Hourly mean
printf "Creating hourly files...\n"
cdo -w -z zip_6 \
-splityear \
-shifttime,-30minutes \
-settunits,minutes \
-selname,assim_nonzero,sif_nonzero \
-aexpr,"assim_nonzero=(abs(assim) + abs(sif) > 0)?assim:missval(assim)" \
-aexpr,"sif_nonzero=(abs(assim) + abs(sif) > 0)?sif:missval(sif)" \
-vertsum \
-select,name=assim,sif \
/data/2021-06-10_SiB4/sib4-hourly-{2014,2015,2016,2017,2018,2019,2020}-* \
data/eda/sib4/hourly/sib4-hourly-
printf "Hourly done.\n"


## Daily mean
printf "Creating daily files...\n"
cdo -w -z zip_6 \
-splityear \
-daymean \
-selname,assim_nonzero,sif_nonzero \
-aexpr,"assim_nonzero=(abs(assim) + abs(sif) > 0)?assim:missval(assim)" \
-aexpr,"sif_nonzero=(abs(assim) + abs(sif) > 0)?sif:missval(sif)" \
-vertsum \
-select,name=assim,sif \
/data/2021-06-10_SiB4/sib4-hourly-{2014,2015,2016,2017,2018,2019,2020}-* \
data/eda/sib4/daily/sib4-daily-
printf "Daily done.\n"


## Weekly mean (with hourly data, weekly mean is calculated as the mean of the 168 hours in a week)
printf "Creating weekly files...\n"
cdo -w -z zip_6 \
-splityear \
-timselmean,168 \
-selname,assim_nonzero,sif_nonzero \
-aexpr,"assim_nonzero=(abs(assim) + abs(sif) > 0)?assim:missval(assim)" \
-aexpr,"sif_nonzero=(abs(assim) + abs(sif) > 0)?sif:missval(sif)" \
-vertsum \
-select,name=assim,sif \
/data/2021-06-10_SiB4/sib4-hourly-{2014,2015,2016,2017,2018,2019,2020}-* \
data/eda/sib4/weekly/sib4-weekly-
printf "Weekly done.\n"


## Monthly mean
printf "Creating monthly files...\n"
cdo -w -z zip_6 \
-splityear \
-monmean \
-selname,assim_nonzero,sif_nonzero \
-aexpr,"assim_nonzero=(abs(assim) + abs(sif) > 0)?assim:missval(assim)" \
-aexpr,"sif_nonzero=(abs(assim) + abs(sif) > 0)?sif:missval(sif)" \
-vertsum \
-select,name=assim,sif \
/data/2021-06-10_SiB4/sib4-hourly-{2014,2015,2016,2017,2018,2019,2020}-* \
data/eda/sib4/monthly/sib4-monthly-
printf "Monthly done.\n"


## Yearly mean
printf "Creating yearly files...\n"
cdo -w -z zip_6 \
-splityear \
-yearmean \
-selname,assim_nonzero,sif_nonzero \
-aexpr,"assim_nonzero=(abs(assim) + abs(sif) > 0)?assim:missval(assim)" \
-aexpr,"sif_nonzero=(abs(assim) + abs(sif) > 0)?sif:missval(sif)" \
-vertsum \
-select,name=assim,sif \
/data/2021-06-10_SiB4/sib4-hourly-{2014,2015,2016,2017,2018,2019,2020}-* \
data/eda/sib4/yearly/sib4-yearly-
printf "Yearly done.\n"