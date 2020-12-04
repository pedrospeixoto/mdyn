#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
import numpy as np
import pandas as pd
import geopandas as gpd

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

import pyreadr 
#pip install pyreadr

import mdyn_extras as mex

#Garbage collection
import gc



print("-------------------------------")
print("Covid vs hospitalization analysis")
print("-------------------------------")


#Input parameters - dir name
#-----------------------------
covid_file = "data_covid/covid-brasl-municp-caso_full.csv" #sys.argv[1]
hosp_file = "data_covid/covid_brazil_leitos_mun.csv" #ys.argv[2]
mun_file = "maps/br_municipios/br_mun_with_uf_region.shp"
pop_file = "maps/population/population_mun_br_2019.csv"
dump_dir = "data_covid/figures/"

df_hosp = pd.read_csv(hosp_file)
print(df_hosp)
print(df_hosp.columns)


df_covid = pd.read_csv(covid_file)
print(df_covid)
print(df_covid.columns)


df_map = gpd.read_file(mun_file)
print(df_map)
print(df_map.columns)
cities = df_map['CD_GEOCMU'].values
cities_names = df_map['Nome_Munic'].values
ncities = len(cities)

df_pop = pd.read_csv(pop_file)
print(df_pop)
print(df_pop.columns)


#---pre-process data---#
#------------------------#

#covid
#Map covid to city lists
date = "2020-09-12"
variable = "last_available_confirmed" #"last_available_deaths" # "last_available_confirmed_per_100k_inhabitants" #"last_available_deaths" "last_available_confirmed" #
varx2 = variable
vary = "last_available_deaths"

#remove full states, keep only cities
df_covid = df_covid[~pd.isnull(df_covid.city_ibge_code)]
df_covid = df_covid[~pd.isnull(df_covid.city)]
#filter date
df_covid = df_covid[df_covid['date']==date]
#conver codes to int
df_covid = df_covid.astype({"city_ibge_code": int})
print(df_covid.columns)

#df_hosp = df_hosp[~pd.isnull(df_hosp.mun_cod)]
df_hosp = df_hosp.astype({"City": int})
varx = "leitos"

covid_vec = np.zeros( len(cities))
covid_vec2 = np.zeros( len(cities))
hosp_vec = np.zeros( len(cities))
pop_vec = np.zeros( len(cities))
uf_vec = [None]*len(cities)

for i, city in enumerate(cities):
    city_covid = df_covid[df_covid["city_ibge_code"] == city ]
    city_hosp = df_hosp[df_hosp["City"] == city ]
    city_pop = df_pop[df_pop["CD_GEOCMU"] == str(city) ]
    pop_var = city_pop["POP2019"].values[0]
    uf_var = city_pop["UF"].values[0]
    try:
        covid_var = city_covid[vary].values[0]
        covid_var2 = city_covid[varx2].values[0]
    except:
        covid_var = 0    
        covid_var2 = 0    
        #print()
        
    hosp_var = city_hosp[varx].values[0]
    if np.isnan(hosp_var):
        hosp_var = 0
    #print(city, cities_names[i], covid_var, hosp_var, pop_var)
    covid_vec[i]=covid_var
    covid_vec2[i]=covid_var2
    hosp_vec[i]=hosp_var
    pop_vec[i]=pop_var
    uf_vec[i]=uf_var

df = pd.DataFrame({"mun_id":cities, "uf":uf_vec, "mun_name":cities_names, vary:covid_vec, varx:hosp_vec, varx2:covid_vec2, "pop":pop_vec})

print("Sum of leitos:", np.sum(hosp_vec))
varxinc=varx+"_inc"
varyinc=vary+"_inc"
df[vary+"_inc"]=100000*df[vary]/df["pop"]
df[varx+"_inc"]=100000*df[varx]/df["pop"]

#filter df
print(df.describe())
factor = 430.457898/768.044691 #mean
factor=12162.162162/38636.363636 #max
#df=df[df[varyinc]+factor*df[varxinc] > 1000]
#df
#df=df[df[varxinc]> 0]
#df=df[df[varyinc]> 0]
#df=df[df["pop"]<1000000]
#df=df[df["pop"]>10000]

print(df)
df.to_csv(dump_dir+"covid_hosp_cities"+date+".csv")
#df.plot(varx, vary, kind='scatter')
#plt.show()
#sys.exit()
power_coef=-1
shift_coef = 1000
#x=df[varxinc].values
#y=df[varyinc].values
x=df[varx].values
y=df[vary].values
print(np.corrcoef(x, y))

#X=np.column_stack((np.transpose(df[varx].values), np.transpose(df[varx2].values)))
X=x

exp = False
if exp:
    ylog = np.log(y+0.001)
    xlog = np.log(x+0.001)
    X = sm.add_constant(xlog)
    model = sm.OLS(ylog, X)
else:
    #X = sm.add_constant(np.power(df[varxinc].values+shift_coef,power_coef))
    X = sm.add_constant(X)
    model = sm.OLS(y, X)

#log(acum_Cases)=a+b*log(day)

results = model.fit()
if exp:
    fitted = np.exp(results.fittedvalues)
else:
    fitted = results.fittedvalues
#names=["const", "leitos", "casos"]
names=["const", "leitos"]
print(results.summary(xname=names))

fig = plt.figure(figsize=(10, 7.0))
ax = plt.gca() 
#ax.set_aspect('equal', 'box')
plt.scatter(x, y, s=1)
if exp:
    a=1
ax.set_yscale('log')
ax.set_xscale('log')
#plt.tick_params(axis='y', which='both', labelsize=14)
#ax.yaxis.set_minor_formatter(FormatStrFormatter("%.1f"))
#plt.tick_params(axis='x', which='both', labelsize=14)
#ax.xaxis.set_minor_formatter(FormatStrFormatter("%.0f"))
vecinds = x.argsort()
vec_sort = x[vecinds]
fitted_sort = fitted[vecinds]
#fitted_sort = np.power((fitted_sort - results.params[0])/results.params[1] ,-power_coef)-shift_coef
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
        " ,  y = "+str(np.round(results.params[0], 6))+pm_sign+str(np.round(np.abs(results.params[1]), 6))+"x  "
        #" ,  y = "+str(np.round(results.params[0], 6))+pm_sign+str(np.round(np.abs(results.params[1]), 6))+"*(x+"+str(shift_coef)+")^("+str(power_coef)+")"

plt.gcf().text(0.02, 0.01, stats, fontsize=8)

plt.xlabel(varx , fontsize=14)
plt.ylabel(vary+date, fontsize=14)
#plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
plt.savefig(dump_dir+"covid_"+varx+"_"+vary+".pdf", dpi=300)
plt.close()
