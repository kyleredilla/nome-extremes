# make filenames for automating download of the WRF ERA-Interim v10 wind component

url = "http://wrf-ak-ar5.s3-website-us-east-1.amazonaws.com/hourly/ERA-Interim/historical/v10/"

# Range(start, stop), where stop is one past the last number you want
historical_range = range(1979, 2016)

filenames = ""

for r in historical_range:
    fn = url + "v10_hourly_wrf_ERA-Interim_historical_{}.nc\n"
    filenames += fn.format(r)

with open("data/wrf_era-interim_v10_fns.txt", "w") as fh:
  fh.write(filenames)
