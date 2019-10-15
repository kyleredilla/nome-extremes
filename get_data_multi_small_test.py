# Test getting small amount of ERA5 data via multiprocessing  on local machine
# extracting four cells around Nome
# Print timing of both serial and multiprocessing runs
import cdsapi

def get_data(year):
	c = cdsapi.Client()
	c.retrieve(
		'reanalysis-era5-single-levels',
	    {
	        'product_type':'reanalysis',
	        'variable':'snowfall',
	        'year':year,
	        'month':[
	            '01'
	        ],
	        'day':[
	            '01'
	        ],
	        'time':[
	            '00:00'
	        ],
	        'area':[64.75, 194.50, 64.25, 195],
	        'format':'netcdf'
	    },
	    'C:/Users/Keal/Desktop/IARC/Nome_Mets/data/testing/ERA5_snowfall_Nome_' + year + '.nc')

if __name__ == '__main__':
	#import cdsapi
	from multiprocessing import Pool
	import time
	# operate on years 2010-2018
	years = list(range(2010, 2019))
	years = [str(i) for i in years]

	# multiprocessing run
	start_time = time.time()
	pool = Pool()
	pool.map(get_data, years)
	print("Multi run took %s seconds" % (time.time() - start_time))

	# serial run
	start_time = time.time()
	for year in years:
		get_data(year)
	print("Serial run took %s seconds" % (time.time() - start_time))
