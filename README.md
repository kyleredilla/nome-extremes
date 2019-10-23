# Meteorological Variables for Nome

Download and subset Snowfall, Snowdepth, and Air Temperature data from the ERA5 reanalysis product and subset to Nome, AK. 
Calibrate with observation data using quantile mapping

#### root
**find_Nome.Rmd**: Lon/Lat coords for MAISE polygons, original file, provided by "Scott"  
**nsidc_0051_sic_nasateam_1978-2018_north_smoothed.nc**: Smoothed Sea Ice concentration from "make_daily_timeseries.py" (run by Michael before leaving, created approx. 2019-09-15)  
**nsidc_sample_20181231.tif**: Sample Sea Ice Concentration file from NSIDC (data from 2018-12-31)  

#### data/
**ERA5_2m_temperature_Nome_quad_1979-1990.nc**: ERA5 air temperature for four grid cells around Nome (1979-1990)
**ERA5_2m_temperature_Nome_quad_1991-2002.nc**: ERA5 air temperature for four grid cells around Nome (1991-2002)
**ERA5_2m_temperature_Nome_quad_2003-2014.nc**: ERA5 air temperature for four grid cells around Nome (2003-2014)
**ERA5_2m_temperature_Nome_quad_2015-2018.nc**: ERA5 air temperature for four grid cells around Nome (2015-2018)
**ERA5_snow_depth_Nome_quad_1979-1990.nc**: ERA5 snow depth for four grid cells around Nome (1979-1990)
**ERA5_snow_depth_Nome_quad_1991-2002.nc**: ERA5 snow depth for four grid cells around Nome (1991-2002)
**ERA5_snow_depth_Nome_quad_2003-2014.nc**: ERA5 snow depth for four grid cells around Nome (2003-2014)
**ERA5_snow_depth_Nome_quad_2015-2018.nc**: ERA5 snow depth for four grid cells around Nome (2015-2018)
**ERA5_snowfall_Nome_quad_1979-1990.nc**: ERA5 snowfall for four grid cells around Nome (1979-1990)
**ERA5_snowfall_Nome_quad_1991-2002.nc**: ERA5 snowfall for four grid cells around Nome (1991-2002)
**ERA5_snowfall_Nome_quad_2003-2014.nc**: ERA5 snowfall for four grid cells around Nome (2003-2014)
**ERA5_snowfall_Nome_quad_2015-2018.nc**: ERA5 snowfall for four grid cells around Nome (2015-2018)
**Nome_bb.csv**: Bounding box of ERA5 grid cells (lon/lat coords)
**Nome_daily.csv**: Daily met data at Nome downloaded from GHCND (1900-2019)

#### documents/
**find_Nome.pdf**: output of **find_Nome.Rmd**; locates Nome within the ERA5 grid  