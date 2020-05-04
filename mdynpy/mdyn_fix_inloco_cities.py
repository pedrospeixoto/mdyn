#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
import numpy as np
import pandas as pd
import math
import geopandas as gpd

from shapely.geometry import Point, Polygon

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

import geopy.distance
import difflib 
import tqdm as tqdm

import mdyn_extras as mex

#Garbage collection
import gc

#Input parameters - dir name
#-----------------------------
filename = sys.argv[1]
shape_file = sys.argv[2]

df = pd.read_csv(filename)
print(df)
df_shp = gpd.read_file(shape_file)
print(df_shp)


#Check states names
states=df['state_name'].unique()
df['state_abrv'] = df['state_name'].map(mex.state_name2abrv)
states_abrv=df['state_abrv'].unique()

print("States check")
print(states)
print(len(states))
print(states_abrv)
print(len(states_abrv))

#Check city names
fix_municip_name = {
   "BOCAIUVA": "BOCAIÚVA",
   "AMPARO DA SERRA" : 'AMPARO DO SERRA',
   "SANTANA DO LIVRAMENTO" : "SANT'ANA DO LIVRAMENTO",
   "FLORÍNIA" : "FLORÍNEA",
   "ITAÓCA" : 'ITAOCA',
   "SÃO TOMÉ DAS LETRAS" : 'SÃO THOMÉ DAS LETRAS',
   "SÃO JOÃO DEL-REI" : 'SÃO JOÃO DEL REI',
   "GUATAMBU" : "GUATAMBÚ",
   "SÃO CRISTOVÃO DO SUL" : 'SÃO CRISTÓVÃO DO SUL',
    "BIRITIBA-MIRIM" : 'BIRITIBA MIRIM',
    "ÁGUAS CLARAS" : 'ÁGUA CLARA',
    "BRAZLÂNDIA" : 'BRASILÂNDIA',
    "MUQUÉM DE SÃO FRANCISCO" : 'MUQUÉM DO SÃO FRANCISCO',
    "LUÍS ALVES" : 'LUIZ ALVES',
    "PINGO-D'ÁGUA" : "PINGO D'ÁGUA",
    "RESTINGA SECA" : 'RESTINGA SÊCA',
    "LAURO MULLER" : 'LAURO MÜLLER',
    "ITAETÊ" : 'ITAETÉ',
    "SAMAMBAIA" : "SAMBAÍBA",
    "ITAPOÃ" : "ITAPOÁ",
    "IUIÚ": "IUIU"
   }

df['city_name']=df['city_name'].str.upper()
df['city_name'] = df['city_name'].replace(fix_municip_name)

cities=df['city_name'].unique()
cities_shp=df_shp['NM_MUNICIP'].unique()

print(cities, len(cities))
print(cities_shp, len(cities_shp))

city_prob = set(cities) - set(cities_shp)
print(city_prob, len(city_prob))
print(df[df['city_name'].isin(city_prob)])
print()
for i, city in enumerate(city_prob):
    matches=difflib.get_close_matches(city, cities_shp)
    print(i, city, matches)

df=df.rename(columns={"isolated": "iso", "dt": "day", "city_name":"reg_name"})
df['reg_state'] = df.apply (lambda row: row.reg_name + "_"+row.state_abrv, axis=1)

#print(len(df))
#df=df.groupby('reg_state', as_index=False).mean()
#df.drop_duplicates(['reg_state'], keep=False, inplace=True)  #does not work!!!
#print(len(df))

#City with duplicated info issues
if False:
    dftmp=df[df['day']=="2020-04-26"]
    dftmp=dftmp.groupby('reg_state').count() 
    dftmp=dftmp[dftmp['iso']>1]
    dup_cities = dftmp.index.values
    print(dup_cities)
    print(df[df['reg_state'].isin(dup_cities) ])
    sys.exit()

for state in states_abrv:
    print(state)
    df_tmp=df[df['state_abrv']==state]
    print(len(df_tmp))
    #df_tmp=df_tmp.drop(['state_name'], axis=1)
    filename="inloco/"+state.upper()+"_Municipios_2020-05-01_iso_index.csv"
    df_tmp.to_csv(filename)
    

