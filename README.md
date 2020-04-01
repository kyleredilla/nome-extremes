# Nome Mets

Evaluate projected changes in extreme weather events using WRF future output.

Download Snowfall, Air Temperature, and Wind data from the ERA5 reanalysis product and subset to Nome, AK. 
Calibrate with observed data using quantile mapping.

**Run scripts from this directory**  

### `Nome_Mets/`
`adj_wrf_tmin_ts`: view adjusted WRF tmin for Nome  
`compare_dist.Rmd`: calculate the euclidean distance between observed Nome data and the surrounding ERA5 grid cells  
`compare_extremes.Rmd`: compare extreme low temperatures and max snowdepth/snowfall between observed Nome data and surrounding ERA5 grid cells  
`compare_sf.Rmd`: compare observed snowfall at Nome with the ERA5 snowfall, where the ERA5 snowfall is scaled by a factor of 7 and 10 (because output is in SWE)  
`convert_era5_ws.R`: Convert ERA5 wind components to direction/speed  
`convert_wrf_ws.R`: Convert WRF wind components to direction/speed  
`find_Nome.Rmd`: locate Nome in the ERA5 grid  
`get_era5_data.py`: download daily ERA5 temperature, snowfall, and snow depth for 9 grid cells centered on Nome  
`get_era5_winds.py`: download hourly ERA5 wind data, 9 grid cells centered on Nome  
`get_era5_winds_2019.py`: download remaining winds from 2019  
`get_land_sea_mask.py`: download land-sea mask for 25 ERA5 grid cells centered on Nome  
`get_wrf_data.sh`: `wget` to download WRF data from Amazon  
`get_wrf_era-interim_v10.sh`: `wget` to download ERA-Interim v10 component (u10 component downloaded prior)  
`helpers.R`: helper functions for R scripts  
`manuscript_content.R`: make figures for manuscript
`mk_wrf_era_interim_v10_fns.py`: make URLs for download via `wget`
`mk_wrf_fns.py`: make target urls for downloading data  
`mk_wrf_sf.R`: Make snowfall time series from WRF precip and temperature data  
`qmap_era5_sf.R`: quantile map ERA5 snowfall data   
`qmap_era5_tmin.R`: quantile map ERA5 minimum temp data  
`qmap_era5_ws.R`: quantile map ERA5 wind speed data   
`qmap_gcm_sf.R`: quantile map WRF CM3 and CCSM4 snow output  
`qmap_gcm_tmin.R`: quantile map WRF CM3 and CCSM4 min temp output  
`qmap_gcm_ws.R`: quantile map WRF CM3 and CCSM4 wind speed output  
`render.R`: Render rmarkdown documents  
`wrf_dec_extr.Rmd`: Visualize projections of extreme events

### `data/`
project data (generated/intermediate)

### `docs/`: 
documents (generated) and supporting info

### `figs/`: 
figures (generated)

### `files/`:
All other files related to the project not to be tracked by git. E.g. manuscripts, data, etc.
