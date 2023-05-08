# %%
import ee
import pandas as pd
import numpy as np
import xarray as xr 
import itertools
from ee.imagecollection import ImageCollection
# ! pip install geemap
import geemap
from calendar import monthrange

# %%
# Trigger the authentication flow.
ee.Authenticate()

# %%
# Initialize the library.
ee.Initialize()

# %%
lakeinfo_new = pd.read_csv('land_xy.csv')
lakeinfo_new
lats = lakeinfo_new['lat'].to_numpy().flatten()
lons = lakeinfo_new['lon'].to_numpy().flatten()
coords = list(zip(lons, lats))
lakeids = lakeinfo_new['LakeID'].to_numpy().flatten()

# %%
listOfFeatures_fc = ee.FeatureCollection(
    [
    ee.Feature(ee.Geometry.Point(coords[i]), {'LakeID': str(lakeids[i])})
    for i in range(len(lakeids))
    ]
)
# %%
listOfFeatures_fc.getInfo()
# %%
lakenumber = lake_shp_test.size().getInfo() # number of lakes

# %%
meteos_monthly = ee.ImageCollection('ECMWF/ERA5_LAND/MONTHLY_AGGR').select([
    'surface_solar_radiation_downwards_sum',
    'surface_thermal_radiation_downwards_sum',
    'surface_sensible_heat_flux_sum',
    'surface_latent_heat_flux_sum',
    'temperature_2m',
    'dewpoint_temperature_2m',
    'u_component_of_wind_10m',
    'v_component_of_wind_10m'
    ])  # 1981-2023
meteos_monthly.first().bandNames().getInfo()
meteo_monthly_scale = meteos_monthly.first().projection().nominalScale().getInfo()

# %%
def image_reduceRegions(image):
    return image.reduceRegions(listOfFeatures_fc, ee.Reducer.first(), scale=11132, crs='EPSG:4326').getInfo()

for iyr in np.arange(1978, 2018):
    for imonth in [1, 2, 7, 8]:
        prefix_str = '{}-{:02d}'.format(iyr, imonth)
        if os.path.isfile('./land_new/{}.nc'.format(prefix_str)):
            continue
        else:
            print(prefix_str)
            dates_monthly = pd.date_range(start=prefix_str+'-01', periods=1, freq='M')
            empty_2darray = np.full((len(dates_monthly), lakenumber), np.nan)
            empty_1darray = np.full(lakenumber, np.nan)
            output_ds = xr.Dataset(
                {
                    't2m': (["time", 'id'], empty_2darray),
                    'd2m': (["time", 'id'], empty_2darray),
                    'strd': (["time", 'id'], empty_2darray),
                    'ssrd': (["time", 'id'], empty_2darray),
                    'sshf': (["time", 'id'], empty_2darray),
                    'slhf': (["time", 'id'], empty_2darray),
                    'u10': (["time", 'id'], empty_2darray),
                    'v10': (["time", 'id'], empty_2darray),
                    'lake_lat': (['id'], empty_1darray),
                    'lake_lon': (['id'], empty_1darray),
                    'lake_id': (['id'], empty_1darray),
                },
                coords={
                    "time": dates_monthly,
                    "id": np.arange(lakenumber),
                },
            )

            subset_meteos2 = meteos_monthly.filterDate(prefix_str)
            listOfMeteos2 = subset_meteos2.toList(subset_meteos2.size())
            lengthOfMeteos2 = subset_meteos2.size().getInfo()

            output_monthly = list(map(image_reduceRegions, [ee.Image(listOfMeteos2.get(i)) for i in range(lengthOfMeteos2)]))
            output_ds['lake_id'] = xr.DataArray(np.array([output_monthly[0]['features'][i]['properties']['LakeID'] for i in range(lakenumber)]), dims=['id'])
            output_ds['t2m'] = xr.DataArray(np.reshape(np.array([output_monthly[0]['features'][i]['properties']['temperature_2m'] for i in range(lakenumber)]), (1, lakenumber)), dims=['time', 'id'])
            output_ds['u10'] = xr.DataArray(np.reshape(np.array([output_monthly[0]['features'][i]['properties']['u_component_of_wind_10m'] for i in range(lakenumber)]), (1, lakenumber)), dims=['time', 'id'])
            output_ds['v10'] = xr.DataArray(np.reshape(np.array([output_monthly[0]['features'][i]['properties']['v_component_of_wind_10m'] for i in range(lakenumber)]), (1, lakenumber)), dims=['time', 'id'])
            output_ds['ssrd'] = xr.DataArray(np.reshape(np.array([output_monthly[0]['features'][i]['properties']['surface_solar_radiation_downwards_sum'] for i in range(lakenumber)]), (1, lakenumber)), dims=['time', 'id'])
            output_ds['strd'] = xr.DataArray(np.reshape(np.array([output_monthly[0]['features'][i]['properties']['surface_thermal_radiation_downwards_sum'] for i in range(lakenumber)]), (1, lakenumber)), dims=['time', 'id'])
            output_ds['sshf'] = xr.DataArray(np.reshape(np.array([output_monthly[0]['features'][i]['properties']['surface_sensible_heat_flux_sum'] for i in range(lakenumber)]), (1, lakenumber)), dims=['time', 'id'])
            output_ds['slhf'] = xr.DataArray(np.reshape(np.array([output_monthly[0]['features'][i]['properties']['surface_latent_heat_flux_sum'] for i in range(lakenumber)]), (1, lakenumber)), dims=['time', 'id'])
            output_ds['d2m'] = xr.DataArray(np.reshape(np.array([output_monthly[0]['features'][i]['properties']['dewpoint_temperature_2m'] for i in range(lakenumber)]), (1, lakenumber)), dims=['time', 'id'])
            output_ds.to_netcdf('./land_new/{}.nc'.format(prefix_str))