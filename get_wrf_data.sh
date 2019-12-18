#!/bin/bash
# download the files saved in wrf_filenames.txt

wget -nc -P /workspace/UA/kmredilla/raw_data/WRF/ -i /workspace/UA/kmredilla/Nome_Mets_aux/data/wrf_filenames.txt
