import cdsapi

c = cdsapi.Client()

c.retrieve(
    'reanalysis-era5-single-levels',
    {
        'product_type':'reanalysis',
        'variable':'snowfall',
        'year':'2018',
        'month':'01',
        'day':'01',
        'time':[
            '00:00'
        ],
        'format':'netcdf'
    },
    '/workspace/UA/kmredilla/Nome_Mets/data/test_download.nc')