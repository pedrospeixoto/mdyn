#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
import numpy as np
import pandas as pd
import geopandas as gpd
import scipy as scp

import math
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.cm as mplcm
from matplotlib.ticker import ScalarFormatter
from matplotlib.ticker import FormatStrFormatter

import statsmodels.api as sm
import curve_fit as cf
import mdyn_lift as lift

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


#Map covid to city lists
date_ref = "2020-07-01"
dump_dir = "covid/figures/"

#covid
vary = "last_available_confirmed" # "last_available_confirmed_per_100k_inhabitants" #"last_available_deaths" "last_available_confirmed" #

#denv
#varx = "denv_total_2020"
varx = "dengue.2019.2020"

proc_file="covid/data/covid_cities_"+vary+"_"+varx+"_"+date_ref+".csv"
if os.path.exists(proc_file):    
    df = pd.read_csv(proc_file)
else:

    #Input parameters - dir name
    #-----------------------------
    covid_file = "covid/caso_full.csv" #sys.argv[1]
    denv_file = "covid/denv_cases_mun2020.csv" #ys.argv[2]
    mun_file = "maps/br_municipios/br_mun_with_uf_region.shp"
    pop_file = "maps/population/population_mun_br_2019.csv"
    dump_dir = "covid/figures/"


    df_covid = pd.read_csv(covid_file)
    print(df_covid)
    print(df_covid.columns)

    df_denv = pd.read_csv(denv_file, sep=";")
    print(df_denv)
    print(df_denv.columns)

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

    #remove full states, keep only cities
    df_covid = df_covid[~pd.isnull(df_covid.city_ibge_code)]
    df_covid = df_covid[~pd.isnull(df_covid.city)]
    #filter date
    df_covid = df_covid[df_covid['date']==date_ref]
    #conver codes to int
    df_covid = df_covid.astype({"city_ibge_code": int})
    print(df_covid.columns)

    df_denv = df_denv[~pd.isnull(df_denv.mun_cod)]
    df_denv = df_denv.astype({"mun_cod": int})
    
    covid_vec = np.zeros( len(cities))
    denv_vec = np.zeros( len(cities))
    pop_vec = np.zeros( len(cities))
    uf_vec = [None]*len(cities)

    for i, city in enumerate(cities):
        city_covid = df_covid[df_covid["city_ibge_code"] == city ]
        city_denv = df_denv[df_denv["mun_cod"] == int(city/10) ]
        city_pop = df_pop[df_pop["CD_GEOCMU"] == str(city) ]
        uf_var = city_pop["UF"].values[0]
        pop_var = city_pop["POP2019"].values[0]
        try:
            covid_var = city_covid[vary].values[0]
            denv_var = city_denv["Total"].values[0]
        except:
            covid_var = 0    
            denv_var = 0
        #print(city, cities_names[i], covid_var, denv_var, pop_var)
        covid_vec[i]=covid_var
        denv_vec[i]=denv_var
        pop_vec[i]=pop_var
        uf_vec[i]=uf_var

    df = pd.DataFrame({"mun_id":cities, "uf":uf_vec, "mun_name":cities_names, vary:covid_vec, varx:denv_vec, "pop":pop_vec})

    varxinc=varx+"_inc"
    varyinc=vary+"_inc"
    df[vary+"_inc"]=100000*df[vary]/df["pop"]
    df[varx+"_inc"]=100000*df[varx]/df["pop"]

    df["state"]=df["uf"].map(mex.state_abrv2name)
    df["region"]=df["state"].map(mex.state_reg)
    #filter df
    print(df.describe())
    uf = ""
    #df=df[df["uf"]> uf]

    #df=df[df[varxinc]> 0]
    #df=df[df[varyinc]> 0]
    #df=df[df["pop"]<1000000]
    #df=df[df["pop"]>10000]

    print(df)
    print(df[df.isnull().any(axis=1)])
    
    df.to_csv(proc_file)

varxinc=varx+"_inc"
varyinc=vary+"_inc"
print(df)

df_tmp =df
x=df_tmp[varxinc].values
y=df_tmp[varyinc].values



namex="denv 2020+2019 acumulated incidence"
namey="sarscov2 2020 acumulated incidence to "+date_ref


nquart = 6
quartiles=[0, 50, 60, 70, 80, 90, 100]

print("Brazil")
mat, p, xquartiles, yquartiles = lift.lift(x,y, quartiles)
lift.plot(x,y, namex, namey, title="Brazil")
lift.plot_lift(mat,xquartiles,yquartiles, namex, namey, title="Brazil", stats="Chi-Sqr - p="+str(p))


if p<0.05:
    print("Covid and denv are related/dependent with p=", p)
    print("High dengue (inc > "+str(np.round(xquartiles[nquart-1],2))+") implies a change in "+str(np.round(mat[nquart-1,nquart-1]*100,2))+ \
        "% probability of having a high covid (inc > "+str(np.round(yquartiles[nquart-1],2))+")")
    print("High dengue (inc > "+str(np.round(xquartiles[nquart-1],2))+") implies a change in "+str(np.round(mat[0,nquart-1]*100,2))+ \
        "% probability of having a low covid (inc < "+str(np.round(yquartiles[1],2))+")")
    print("Low dengue (inc < "+str(np.round(xquartiles[1],2))+") implies a change in "+str(np.round(mat[nquart-1,0]*100,2))+ \
        "% probability of having a high covid (inc > "+str(np.round(yquartiles[nquart-1],2))+")")
else:
    print("Covid and denv may be independent (not related/dependent), since p=", p)
print()

regions = df.region.unique()
nquart = 4
quartiles=[0, 50, 75, 100]

for reg in regions:
    df_tmp = df[df["region"]==reg]
    x=df_tmp[varxinc].values
    y=df_tmp[varyinc].values

    print("Region: ", reg)
    mat, p, xquartiles, yquartiles = lift.lift(x,y, quartiles)
    lift.plot(x,y, namex, namey, title=reg)
    lift.plot_lift(mat,xquartiles,yquartiles, namex, namey, title=reg, stats="Chi-Sqr - p="+str(p))
    #if p<0.05:
    #    print("Covid and denv are related/dependent with p=", p)
    #    print("High dengue (inc > "+str(np.round(xquartiles[nquart-1],2))+") implies a change in "+str(np.round(mat[nquart-1,nquart-1]*100,2))+ \
    #        "% probability of having a high covid (inc > "+str(np.round(yquartiles[nquart-1],2))+")")
    #    print("High dengue (inc > "+str(np.round(xquartiles[nquart-1],2))+") implies a change in "+str(np.round(mat[0,nquart-1]*100,2))+ \
    #        "% probability of having a low covid (inc < "+str(np.round(yquartiles[1],2))+")")
    #    print("Low dengue (inc < "+str(np.round(xquartiles[1],2))+") implies a change in "+str(np.round(mat[nquart-1,0]*100,2))+ \
    #        "% probability of having a high covid (inc > "+str(np.round(yquartiles[nquart-1],2))+")")
    #else:
    #    print("Covid and denv may be independent (not related/dependent), since p=", p)
    print()


regions = df.uf.unique()
nquart = 2
quartiles=[0, 50, 100]
for reg in regions:
    df_tmp = df[df["uf"]==reg]
    x=df_tmp[varxinc].values
    y=df_tmp[varyinc].values
    print()
    print("Region: ", reg)
    mat, p, xquartiles, yquartiles = lift.lift(x,y, quartiles)
    lift.plot(x,y, namex, namey, title=reg)
    lift.plot_lift(mat,xquartiles,yquartiles, namex, namey, title=reg, stats="Fisher Exact - p="+str(p))
    print()
