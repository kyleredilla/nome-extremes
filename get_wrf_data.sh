#!/bin/bash
# download the files saved in wrf_filenames.txt

wget -nc -P /workspace/UA/kmredilla/data-raw/WRF/ -i /workspace/UA/kmredilla/Nome_Mets/data/wrf_filenames.txt
