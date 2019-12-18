# make filenames for automating download of the temperature, snowfall, and snowdepth output

# http://wrf-ak-ar5.s3-website-us-east-1.amazonaws.com/?prefix=daily/GFDL-CM3/historical/t2min/
# http://wrf-ak-ar5.s3-website-us-east-1.amazonaws.com/?prefix=hourly/GFDL-CM3/historical/snow/
# http://wrf-ak-ar5.s3-website-us-east-1.amazonaws.com/?prefix=hourly/GFDL-CM3/historical/acsnow/

models = ["GFDL-CM3", "NCAR-CCSM4"]# {} placeholder will be substituted with specific model & historical/future
hrly_vars = ["snow", "acsnow", "t2", "snowh"]

url1 = "http://wrf-ak-ar5.s3-website-us-east-1.amazonaws.com/daily/{}/{}/t2min/"
url2 = "http://wrf-ak-ar5.s3-website-us-east-1.amazonaws.com/hourly/{}/{}/{}/"

# Range(start, stop), where stop is one past the last number you want
historical_range = range(1979, 2006)
future_range = range(2006, 2101)

filenames = ""

# http://wrf-ak-ar5.s3.amazonaws.com/daily/GFDL-CM3/historical/t2min/t2min_daily_wrf_GFDL-CM3_historical_1970.nc
for model in models:
    for r in historical_range:
        fn1 = url1 + "t2min_daily_wrf_{}_historical_{}.nc\n"
        filenames += fn1.format(model, "historical", model, r)
        for var in hrly_vars:
        	fn2 = url2 + "{}_hourly_wrf_{}_historical_{}.nc\n"
        	filenames += fn2.format(model, "historical", var, var, model, r)

    for r in future_range:
        fn1 = url1 + "t2min_daily_wrf_{}_rcp85_{}.nc\n"
        filenames += fn1.format(model, "rcp85", model, r)
        for var in hrly_vars:
    	    fn2 = url2 + "{}_hourly_wrf_{}_rcp85_{}.nc\n"
    	    filenames += fn2.format(model, "rcp85", var, var, model, r)

with open("../Nome_Mets_aux/data/wrf_filenames.txt", "w") as fh:
  fh.write(filenames)
