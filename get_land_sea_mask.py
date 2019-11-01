# Download ERA5 land sea mask

import cdsapi

def get_data():
    c = cdsapi.Client()
    c.retrieve(
        'reanalysis-era5-single-levels',
        {
            'product_type':'reanalysis',
            'variable':'land_sea_mask',
            'year':'2018',
            'month':'01',
            'day':'01',
            'time':'01',
            'area':[64.5, 194.5, 64, 195],
            'format':'netcdf'
        },
        '/workspace/UA/kmredilla/Nome_Mets/data/ERA5_land_sea_mask_Nome_sector.nc')

if __name__ == '__main__':
    import copy
    from multiprocessing import Pool
    # multiprocessing run
    pool = Pool()
    # get all data
    pool.map(get_data)
    