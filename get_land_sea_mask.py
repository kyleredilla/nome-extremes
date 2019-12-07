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
            'area':[65, 194, 64, 195],
            'format':'netcdf'
        },
        '/workspace/UA/kmredilla/Nome_Mets/data/ERA5_land_sea_mask_Nome_sector.nc')

if __name__ == '__main__':
    import copy
    get_data()
    