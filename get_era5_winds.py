# Download ERA5 wind output: 
#   10 m wind u component,
#   10 m wind v component, 
# for nine grid cells around Nome using SLURM

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
            'area':[64.75, 194.25, 64.25, 194.75],
            'format':'netcdf'
        },
        '/workspace/UA/kmredilla/data-raw/ERA5/ERA5_10u_10v_Nome_sector_' + years[0] + '-' + years[-1] + '.nc')

if __name__ == '__main__':
    import copy
    from multiprocessing import Pool

    # operate on years 1979-2018, broken into 10 chunks to satisfy request limit
    years = [list(range(1979, 1983)),
             list(range(1983, 1987)),
             list(range(1987, 1991)),
             list(range(1991, 1995)),
             list(range(1995, 1999)),
             list(range(1999, 2003)),
             list(range(2003, 2007)),
             list(range(2007, 2011)),
             list(range(2011, 2015)),
             list(range(2015, 2019)),]
    years = [[str(j) for j in i] for i in years]
    # add var names to year lists
    for i in range(10):
            years[i].append(['10m_u_component_of_wind', '10m_v_component_of_wind'])
    # multiprocessing run
    pool = Pool()
    # get all data
    pool.map(get_data, years)
    
