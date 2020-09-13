#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
import numpy as np
import pandas as pd
import geopandas as gpd

import ast 

import math
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.cm as mplcm
from matplotlib.ticker import ScalarFormatter
from matplotlib.ticker import FormatStrFormatter

import statsmodels.api as sm

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

import geopy.distance

import tqdm as tqdm

import mdyn_extras as mex

#Garbage collection
import gc



print("-------------------------------")
print("Covid vs highways analysis")
print("-------------------------------")


#Input parameters - dir name
#-----------------------------
covid_file = "covid/caso_full.csv" #sys.argv[1]
highways_file = "maps/rodovias_dnit/Rodovias_to_cities.csv" #ys.argv[2]
mun_file = "maps/br_municipios/br_mun_with_uf_region.shp"
highways_shp = "maps/rodovias_dnit/ST_DNIT_Rodovias_SNV2015_03.shp" #ys.argv[2]

dump_dir = "covid/figures/"


df_covid = pd.read_csv(covid_file)
print(df_covid)
print(df_covid.columns)

df_brs = pd.read_csv(highways_file)
print(df_brs)
print(df_brs.columns)

df_map = gpd.read_file(mun_file)
print(df_map)
print(df_map.columns)
cities = df_map['CD_GEOCMU'].values
cities_names = df_map['Nome_Munic'].values
ncities = len(cities)

df_brs_map = gpd.read_file(highways_shp)
print(df_brs_map)
print(df_brs_map.columns)
#df_brs_map.plot()
#plt.show()


#---highways pre-process ---#
#---------------------------#

br_list = [ ]
br_city_list = []
br_len = 100 #num of municipalities (200 gets the top 10!)

#Map Brs to city lists
for i, row in df_brs.iterrows():
    #convert string list to list of ints
    local_city_list = ast.literal_eval(row['cities_id']) 
    
    if len(local_city_list) > br_len:
        #Create vector of 0 and 1 for full city list based on highway path
        city_set = set(local_city_list)
        full_city_list = [i in city_set for i in cities]
        br_city_vec = np.array(full_city_list).astype(int)
        #print(br_city_vec)
        
        br_list.append(str(row['br']))
        br_city_list.append(br_city_vec)

print(br_list)
print(br_city_list)
br_matrix = np.transpose(np.stack( br_city_list, axis=0 ))
print(br_matrix.shape)

#filter map of highways - to plot
df_brs_map =  df_brs_map[df_brs_map.br.isin(br_list)]
#print(df_brs_map)
#df_brs_map.plot()
#plt.show()


#---covid pre-process ---#
#------------------------#

#Map covid to city lists
date = "2020-09-12"
variable = "last_available_confirmed" # last_available_confirmed_per_100k_inhabitants
#remove full states, keep only cities
df_covid = df_covid[~pd.isnull(df_covid.city_ibge_code)]
df_covid = df_covid[~pd.isnull(df_covid.city)]
#filter date
df_covid = df_covid[df_covid['date']==date]
#conver codes to int
df_covid = df_covid.astype({"city_ibge_code": int})
print(df_covid.columns)

covid_vec = np.zeros( len(cities))
for i, city in enumerate(cities):
    city_covid = df_covid[df_covid["city_ibge_code"] == city ]
    try:
        covid_var = city_covid[variable].values[0]
    except:
        covid_var = 0
    #print(city, cities_names[i], covid_var)
    covid_vec[i]=covid_var

#Build linear model!!#
# ------------------#

X = sm.add_constant(br_matrix)
y = covid_vec
#log(acum_Cases)=a+b*log(day)
model = sm.OLS(y, X)
results = model.fit()
fitted = results.fittedvalues
print(br_list, len(br_list))
print(results.summary(xname=['a']+br_list))

#intercept[i] = results.params[0]
#slopes[i] = results.params[1]
#r[i] = np.sqrt(results.rsquared)
#results.pvalues[1]))
