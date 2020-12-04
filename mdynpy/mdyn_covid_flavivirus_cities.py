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
import curve_fit as cf

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
print("Covid vs flavivirus analysis")
print("-------------------------------")


#Input parameters - dir name
#-----------------------------
covid_file = "data_covid/covid-brasl-municp-caso_full.csv" #sys.argv[1]
denv_file = "data_dengue/denv_cases_brasil_mun2020.csv" #ys.argv[2]
mun_file = "maps/br_municipios/br_mun_with_uf_region.shp"
pop_file = "maps/population/population_mun_br_2019.csv"
area_file = "maps/area/mun_areas.csv"
dump_dir = "data_dengue/figures/"

df_covid = pd.read_csv(covid_file)
print(df_covid)
print(df_covid.columns)

df_denv = pd.read_csv(denv_file, sep=";")
print(df_denv)
print(df_denv.columns)

df_area = pd.read_csv(area_file, sep=",")
print(df_area)
print(df_area.columns)

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
date = "2020-07-01"
variable = "last_available_confirmed" # "last_available_confirmed_per_100k_inhabitants" #"last_available_deaths" "last_available_confirmed" #
vary = variable

#remove full states, keep only cities
df_covid = df_covid[~pd.isnull(df_covid.city_ibge_code)]
df_covid = df_covid[~pd.isnull(df_covid.city)]
#filter date
df_covid = df_covid[df_covid['date']==date]
#conver codes to int
df_covid = df_covid.astype({"city_ibge_code": int})
print(df_covid.columns)

df_denv = df_denv[~pd.isnull(df_denv.mun_cod)]
df_denv = df_denv.astype({"mun_cod": int})
varx = "denv_total_2020"

covid_vec = np.zeros( len(cities))
denv_vec = np.zeros( len(cities))
pop_vec = np.zeros( len(cities))
uf_vec = [None]*len(cities)
area_vec = [None]*len(cities)

for i, city in enumerate(cities):
    city_covid = df_covid[df_covid["city_ibge_code"] == city ]
    city_denv = df_denv[df_denv["mun_cod"] == int(city/10) ]
    city_pop = df_pop[df_pop["CD_GEOCMU"] == str(city) ]
    city_map = df_map[df_map["CD_GEOCMU"] == city ]
    city_area=df_area[df_area["CD_GCMUN"] == city ].AR_MUN_2019.values[0]
    #print(city, city_area)

    try:
        pop_var = city_pop["POP2019"].values[0]
        uf_var = city_pop["UF"].values[0]
        covid_var = city_covid[variable].values[0]
        denv_var = city_denv["Total"].values[0]
        
    except:
        pop_var = 1
        covid_var = 0    
        denv_var = 0
        uf_var = "na"
    #print(city, cities_names[i], covid_var, denv_var, pop_var)
    covid_vec[i]=covid_var
    denv_vec[i]=denv_var
    pop_vec[i]=pop_var
    uf_vec[i]=uf_var
    area_vec[i] = city_area

df = pd.DataFrame({"mun_id":cities, "uf":uf_vec, "mun_name":cities_names, vary:covid_vec, varx:denv_vec, "pop":pop_vec, "area":area_vec})

varxinc=varx+"_inc"
varyinc=vary+"_inc"
df[vary+"_inc"]=100000*df[vary]/df["pop"]
df[varx+"_inc"]=100000*df[varx]/df["pop"]
df["dens"]=df["pop"]/df["area"]

#filter df
print(df.describe())
#factor = 430.457898/768.044691 #mean
factor=12162.162162/38636.363636 #max
#df=df[df[varyinc]+(factor)*df[varxinc] > 500]
#df
uf = ""
#df=df[df["uf"]> uf]

#df=df[df[varxinc]> 0]
#df=df[df[varyinc]> 0]
#df=df[df["pop"]<1000000]
#df=df[df["pop"]>100000]

print(df)
df.to_csv(dump_dir+"covid_cities"+date+".csv")
#df.plot(varx, vary, kind='scatter')
#plt.show()
#sys.exit()
power_coef=-1
shift_coef = 5000
#x=df["dens"].values
x=df["dens"].values
#x=df["pop"].values
#x=df[varxinc].values
#y=df[varyinc].values
#y=df["dens"].values
#y=df["pop"].values
#y=df[varyinc].values/df[varxinc].values
y=df[varxinc].values/(df[varyinc].values+1)

cf.nonlinear_adjust(x,y)
#X=np.column_stack((np.transpose(x), np.transpose(df["dens"].values)))
X=x

exp = False
if exp:
    ylog = np.log(y+0.001)
    xlog = np.log(x+0.001)
    X = sm.add_constant(xlog)
    model = sm.OLS(ylog, X)
else:
    #X = sm.add_constant(np.power(x+shift_coef,power_coef))
    X = sm.add_constant(X)
    model = sm.OLS(y, X)

#log(acum_Cases)=a+b*log(day)

results = model.fit()
if exp:
    fitted = np.exp(results.fittedvalues)
else:
    fitted = results.fittedvalues
print(results.summary())

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
        " ,  y = "+str(np.round(results.params[0], 6))+pm_sign+str(np.round(np.abs(results.params[1]), 6))+"x"
        #" ,  y = "+str(np.round(results.params[0], 6))+pm_sign+str(np.round(np.abs(results.params[1]), 6))+"*(x+"+str(shift_coef)+")^("+str(power_coef)+")"
        

plt.gcf().text(0.02, 0.01, stats, fontsize=8)

#plt.xlabel("denv 2020 acumulated incidence" , fontsize=14)
plt.xlabel("population density (city pop/ city area)" , fontsize=14)
#plt.xlabel("population " , fontsize=14)
#plt.ylabel("sarscov2 2020 acumulated incidence to "+date, fontsize=14)
#plt.ylabel("sarscov2 2020 acumulated incidence to "+date, fontsize=14)
plt.ylabel("denv 2020 / sarscov2 2020", fontsize=14)
#plt.ylabel("poplation density (city pop/ city area)" , fontsize=14)
#plt.ylabel("poplation" , fontsize=14)
#plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
#name = varxinc+"_"+varyinc
#name = "dens_covid"
#name = "denv_dens"
#name = "pop_covid"
#name = "pop_denv_div_covid"
name = "dens_denv_div_covid"
#name = "denv_pop"
plt.savefig(dump_dir+name+uf+".jpg", dpi=300)
plt.close()
