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
print("Covid vs denv India    analysis")
print("-------------------------------")


#Input parameters - dir name
#-----------------------------
covid_file = "covid/india.csv" #sys.argv[1]
dump_dir = "covid/figures/"

df = pd.read_csv(covid_file)
print(df)
print(df.columns)
df["density"]=df["area"]/df["Pop2020"]

varx1="Pop2020"
#varx2="Dengue15-20Inc"
#varx2="rainfall"
varx2="density"
#varx2="Dengue15-20"
vary="CovidInc"
#vary="Dengue15-20Inc"
#vary="Dengue19Inc"
#vary="CasesCovid19"

#df=df[df["Pop2020"]<15400000]
#df=df[df["rainfall"]<1000]
#df=df[df["rainfall"]> 0]
#filter df
print(df.describe())

#x=df[varxinc].values
#y=df[varyinc].values
x1=df[varx1].values
x2=df[varx2].values
y=df[vary].values
X=x2 #np.column_stack((np.transpose(x1), np.transpose(x2)))
#X=np.column_stack((np.transpose(x1), np.transpose(x2)))

X = sm.add_constant(X)
model = sm.OLS(y, X)

results = model.fit()

fitted = results.fittedvalues
#names=["const", varx1, varx2]
names=["const", varx2]
print(results.summary(xname=names, yname=vary))

fig = plt.figure(figsize=(10, 7.0))
ax = plt.gca() 
#ax.set_aspect('equal', 'box')
plt.scatter(x2, y, s=1)
#ax.set_yscale('log')
#ax.set_xscale('log')
#plt.tick_params(axis='y', which='both', labelsize=14)
#ax.yaxis.set_minor_formatter(FormatStrFormatter("%.1f"))
#plt.tick_params(axis='x', which='both', labelsize=14)
#ax.xaxis.set_minor_formatter(FormatStrFormatter("%.0f"))
vecinds = x2.argsort()
vec_sort = x2[vecinds]
fitted_sort = fitted[vecinds]
#fitted_sort = np.power((fitted_sort - results.params[0])/results.params[1] ,-power_coef)-shift_coef
plt.plot(vec_sort, fitted_sort, marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0)

for i, state in enumerate(df.IndiaState):  
    plt.annotate(state, (x2[i], y[i]))

if results.params[1]>0:
    pm_sign = "+"
else:
    pm_sign = "-"    


stats="\nr = "+pm_sign+str(np.round(np.sqrt(results.rsquared),6))+ \
    " ,  R2 = "+str(np.round(results.rsquared,6))+ \
    " ,  p = "+str(np.round(results.pvalues[1], 6)) + \
    " ,  y = "+str(np.round(results.params[0], 6))+pm_sign+str(np.round(np.abs(results.params[1]), 6))+"x  "
        #" ,  y = "+str(np.round(results.params[0], 6))+pm_sign+str(np.round(np.abs(results.params[1]), 6))+"*(x+"+str(shift_coef)+")^("+str(power_coef)+")"

plt.gcf().text(0.02, 0.01, stats, fontsize=8)

plt.xlabel(varx2 , fontsize=14)
plt.ylabel(vary, fontsize=14)
#plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
plt.savefig(dump_dir+"covid_"+varx2+"_"+vary+".jpg", dpi=300)
plt.close()
