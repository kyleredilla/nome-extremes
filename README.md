# Meteorological Variables for Nome

Download Snowfall, Snowdepth, and Air Temperature data from the ERA5 reanalysis product and subset to Nome, AK. 
Calibrate with observed daily data using quantile mapping, evaluate projected changes in extremes.

Note: scripts should be run from this directory

#### root
**compare_dist.Rmd**: calculate the euclidean distance between observed Nome data and the surrounding ERA5 grid cells  
**compare_extremes.Rmd**: compare extreme low temperatures and max snowdepth/snowfall between observed Nome data and surrounding ERA5 grid cells  
**compare_sf.Rmd**: compare observed snowfall at Nome with the ERA5 snowfall, where the ERA5 snowfall is scaled by a factor of 7 and 10 (because output is in SWE)  
**find_Nome.Rmd**: locate Nome in the ERA5 grid  
**get_data.py**: download ERA5 temperature, snowfall, and snow depth for 9 grid cells centered on Nome  
**get_land_sea_mask.py**: download land-sea mask for 25 ERA5 grid cells centered on Nome  
**helpers.R**: helper functions for R scripts  
**mk_wrf_fns.py**: make target urls for downloading data  
**qmap_era5_sf.R**: quantile map ERA5 snow data using daily snowfall records for Nome  
**qmap_era5_tmin.R**: quantile map ERA5 minimum temp data using daily temp records for Nome  
**qmap_gcm_sf.R**: quantile map CM3 and CCSM4 snow output using adjusted ERA5 snow data  
**qmap_gcm_tmin.R**: quantile map CM3 and CCSM4 min temp output using adjusted ERA5 temp data  
**render.R**: Render rmarkdown documents  
