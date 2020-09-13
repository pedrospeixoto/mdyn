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
print("Covid analysis")
print("-------------------------------")


#Input parameters - dir name
#-----------------------------
covid_file = "covid/caso_full.csv" #sys.argv[1]
mun_file = "maps/br_municipios/br_mun_with_uf_region.shp"

dump_dir = "covid/figures/"


df_covid = pd.read_csv(covid_file)
print(df_covid)
print(df_covid.columns)

df_map = gpd.read_file(mun_file)
print(df_map)
print(df_map.columns)
cities = df_map['CD_GEOCMU'].values
cities_names = df_map['Nome_Munic'].values
ncities = len(cities)


#---covid pre-process ---#
#------------------------#

#Map covid to city lists
date = "2020-09-12"
 # last_available_confirmed_per_100k_inhabitants #"last_available_deaths"
#remove full states, keep only cities
df_covid = df_covid[~pd.isnull(df_covid.city_ibge_code)]
df_covid = df_covid[~pd.isnull(df_covid.city)]
df_covid = df_covid[~pd.isnull(df_covid.last_available_death_rate)]
df_covid = df_covid[~pd.isnull(df_covid.last_available_confirmed_per_100k_inhabitants)]
df_covid = df_covid[df_covid.last_available_death_rate > 0]

#filter date
df_covid = df_covid[df_covid['date']==date]
#conver codes to int
df_covid = df_covid.astype({"city_ibge_code": int})
print(df_covid.columns)
varx = "last_available_confirmed_per_100k_inhabitants"
varx = "last_available_confirmed"
vary = "last_available_death_rate"
vary = "last_available_deaths"
x = df_covid[varx].values
y = df_covid[vary].values*100
print(x,y)
print()
print("Cases vs deaths")
print()

exp = True
if exp:
    ylog = np.log(y)
    xlog = np.log(x)
    print(xlog,ylog)
    X = sm.add_constant(xlog)
    model = sm.OLS(ylog, X)
else:
    X = sm.add_constant(x)
    model = sm.OLS(y, X)

#log(acum_Cases)=a+b*log(day)

results = model.fit()
if exp:
    fitted = np.exp(results.fittedvalues)
else:
    fitted = results.fittedvalues
#print(results.summary())

fig = plt.figure(figsize=(10, 10))
ax = plt.gca() 

plt.scatter(x, y, s=10)
if exp:
    ax.set_yscale('log')
    ax.set_xscale('log')
#plt.tick_params(axis='y', which='both', labelsize=14)
#ax.yaxis.set_minor_formatter(FormatStrFormatter("%.1f"))
#plt.tick_params(axis='x', which='both', labelsize=14)
#ax.xaxis.set_minor_formatter(FormatStrFormatter("%.0f"))
vecinds = x.argsort()
vec_sort = x[vecinds]
fitted_sort = fitted[vecinds]
plt.plot(vec_sort, fitted_sort, marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0)

if results.params[1]>0:
    pm_sign = "+"
else:
    pm_sign = "-"    

if exp:
    stats="\nr = "+pm_sign+str(np.round(np.sqrt(results.rsquared),10))+ \
        " ,  R2 = "+str(np.round(results.rsquared,10))+ \
        " ,  p = "+str(results.pvalues[1]) + \
        " ,  log(y) = "+str(np.round(results.params[0], 6))+pm_sign+str(np.round(np.abs(results.params[1]), 6))+"log(x)  "
else:
    stats="\nr = "+pm_sign+str(np.round(np.sqrt(results.rsquared),6))+ \
        " ,  R2 = "+str(np.round(results.rsquared,6))+ \
        " ,  p = "+str(np.round(results.pvalues[1], 6)) + \
        " ,  y = "+str(np.round(results.params[0], 6))+pm_sign+str(np.round(np.abs(results.params[1]), 6))+"x    "

#plt.annotate(stats, (vec_sort[-1]-10, fitted_sort[-1]-2), fontsize=14)
plt.gcf().text(0.02, 0.02, stats, fontsize=10)

plt.xlabel(varx, fontsize=14)
plt.ylabel(vary, fontsize=14)

plt.savefig(dump_dir+"covid_"+varx+"_"+vary+".pdf", dpi=300)
plt.close()
