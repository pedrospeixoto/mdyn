import pandas as pd
import geopandas as gpd
import numpy as np
from matplotlib import pyplot as plt  #contains both numpy and pyplot
from shapely import geometry, ops
from shapely.geometry import Polygon, LineString
from mdyn_extras import state_name2abrv

import sys

df_cities = gpd.read_file("maps/br_municipios/br_mun_with_uf_region.shp") #Shape file municipios br with uf maps/br_municipios/br_mun_with_uf_region.shp
df_cities["uf"] = df_cities["Nome_UF"].map(state_name2abrv)
print(df_cities)
print(df_cities.columns)


df_brs = gpd.read_file("maps/rodovias_dnit/ST_DNIT_Rodovias_SNV2015_03.shp") #highway shape file maps/rodovias_dnit/ST_DNIT_Rodovias_SNV2015_03.shp
print(df_brs)
print(df_brs.columns)

#df_brs = df_brs[df_brs['uf'] == "RN"]
df_brs["cities"] = df_brs["uf"]
df_brs["cities_id"] = df_brs["uf"]

print()
print("BR    COD     UF       Cidades      ")
for row in df_brs.itertuples():
    city_list = []
    cityid_list = []
    df_tmp = df_cities[df_cities["uf"] == row.uf ]
    
    line = row.geometry
    for city in df_cities.itertuples():
        poly = city.geometry 
        if line.intersects(poly):
            city_list.append(city.Nome_Munic)
            cityid_list.append(city.CD_GEOCMU)

    print(row.br, row.codigo, row.uf, city_list, cityid_list)        
    df_brs.at[row.Index, 'cities'] = city_list
    df_brs.at[row.Index, 'cities_id'] = cityid_list
    #print(row)
    #print(df_brs.iloc[row.Index] )
    
df_brs = df_brs.drop(columns=['geometry'])
df_brs.to_csv('maps/rodovias_dnit/ST_DNIT_Rodovias_SNV2015_03_with_cities.csv')

df_final = df_brs.groupby('br').agg({'cities': 'sum', 'cities_id':'sum'})
print(df_final)
df_final.to_csv('maps/rodovias_dnit/Rodovias_to_cities.csv')
