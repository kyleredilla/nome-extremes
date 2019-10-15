# Download ERA5 data for four grid cells around Nome using SLURM and multiprocessing

import cdsapi

def get_data(year):
	c = cdsapi.Client()
	c.retrieve(
		'reanalysis-era5-single-levels',
	    {
	        'product_type':'reanalysis',
	        'variable':'snowfall',
	        'year':str(year),
	        'month':[
	            '01','02','03',
	            '04','05','06',
	            '07','08','09',
	            '10','11','12'
	        ],
	        'day':[
	            '01','02','03',
	            '04','05','06',
	            '07','08','09',
	            '10','11','12',
	            '13','14','15',
	            '16','17','18',
	            '19','20','21',
	            '22','23','24',
	            '25','26','27',
	            '28','29','30',
	            '31'
	        ],
	        'time':[
	            '00:00','01:00','02:00',
	            '03:00','04:00','05:00',
	            '06:00','07:00','08:00',
	            '09:00','10:00','11:00',
	            '12:00','13:00','14:00',
	            '15:00','16:00','17:00',
	            '18:00','19:00','20:00',
	            '21:00','22:00','23:00'
	        ],
	        'area':[64.75, 194.50, 64.25, 195],
	        'format':'netcdf'
	    },
	    '/workspace/UA/kmredilla/Nome_Mets/data/testing/ERA5_snowfall_Nome_' + year + '.nc')

if __name__ == '__main__':
	from multiprocessing import Pool
	import time
	# operate on years 2010-2018
	years = list(range(2014, 2019))
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