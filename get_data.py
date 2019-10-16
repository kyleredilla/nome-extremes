# Download ERA5 data snowfall and snow depth for four grid cells around Nome using SLURM
# full time span request too large, need to breakup into chunks

import cdsapi

def get_data(years):
    var = years.pop(-1)
    c = cdsapi.Client()
    c.retrieve(
        'reanalysis-era5-single-levels',
        {
            'product_type':'reanalysis',
            'variable':var,
            'year':years,
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
        '/workspace/UA/kmredilla/Nome_Mets/data/ERA5_' + var + '_Nome_quad_' + years[0] + '-' + year[-1] + '.nc')

if __name__ == '__main__':
    from multiprocessing import Pool

    # operate on years 1979-2018, broken into 4 chunks
    years = [list(range(1979, 1991)),
             list(range(1991, 2003)),
             list(range(2003, 2015)),
             list(range(2015, 2019))]
    years = [[str(j) for j in i] for i in years]

    # multiprocessing run
    pool = Pool()
    # snowfall
    years_sf = [i.append('snowfall') for i in years]
    pool.map(get_data, years_sf)
    # Snow depth
    years_sd = [i.append('snow_depth') for i in years]
    pool.map(get_data, years_sd)
