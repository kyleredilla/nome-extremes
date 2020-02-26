#!/bin/bash
# download the files saved in wrf_filenames.txt

wget -nc -P /workspace/UA/kmredilla/data-raw/WRF/hourly/ERA-Interim/historical/v10 -i /workspace/UA/kmredilla/Nome_Mets/data/wrf_era-interim_v10_fns.txt
