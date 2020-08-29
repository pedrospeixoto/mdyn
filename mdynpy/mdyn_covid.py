#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
import numpy as np
import pandas as pd
import math
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.cm as mplcm

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

#Input parameters - dir name
#-----------------------------
covid_file = sys.argv[1]

#Load data
base_name = "data"

covid = pd.read_csv(covid_file)

print(covid)
print(covid.columns)
ref_date = "2020-02-25"
ref_date = datetime.strptime(ref_date, "%Y-%m-%d")
print(ref_date)

covid['datetime']=pd.to_datetime(covid['date'])
covid['days']=(covid['datetime']-ref_date).dt.days

n = len(mex.state_abrv2name)

slopes = np.zeros((n))
intercept = np.zeros((n))
covid_cases_compare = np.zeros((n))
r = np.zeros((n))

# Initialize the figure
plt.style.use('seaborn-darkgrid')
 
# create a color palette
palette = plt.get_cmap('Set1')
colors=palette(np.linspace(0, 1.0, n))

fig, axs = plt.subplots(6,5, figsize=(15, 15), squeeze=False)
axs = axs.ravel()
covid_cases_compare_line = 1000
covid_cases_cut_min = 10
time_cut_max = 90
for i, state in enumerate(mex.state_abrv2name):   
    print(state) 

    #Get data
    covid_state = covid[covid['state'] == state]
    acum_cases = covid_state['totalCases'].values
    days = covid_state['days'].values

    #Filter time and cases
    filtercases = acum_cases > covid_cases_cut_min
    days_reg = days[filtercases]
    acum_cases_reg = acum_cases[filtercases]
    days_reg=days_reg[:time_cut_max]
    acum_cases_reg= acum_cases_reg[:time_cut_max]

    #Set regression
    X = np.log2(days_reg[:time_cut_max])
    y = np.log2(acum_cases_reg[:time_cut_max])
    X = sm.add_constant(X)
    #log(acum_Cases)=a+b*log(day)
    model = sm.OLS(y, X)
    results = model.fit()
    fitted = results.fittedvalues
    print(results.summary())

    intercept[i] = results.params[0]
    slopes[i] = results.params[1]
    r[i] = np.sqrt(results.rsquared)

    covid_cases_compare[i] = np.power(2.0, (np.log2(covid_cases_compare_line)-intercept[i])/slopes[i])

    #day_int = 
    for j in range(n):
        
        #plt.subplot(6,5,j+1)
        axs[j].plot(days, acum_cases, marker='', color='grey', linewidth=0.6, alpha=0.3)
    
    # Plot the lineplot
    #plt.subplot(6,5,i+1)
    axs[i].plot(days, acum_cases, marker='', color=colors[i], linewidth=1.5, alpha=0.9, label=state)
    axs[i].plot(days_reg, np.power(2.0, fitted), marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0, label=state)
    axs[i].set_yscale('log')
    axs[i].set_xscale('log')
    axs[i].set_xlim(left=10)
    

    axs[i].set_title(state, loc='left', fontsize=12, fontweight=0, color=colors[i] )

fig.delaxes(axs[27])
fig.delaxes(axs[28])
fig.delaxes(axs[29])

fig.tight_layout(pad=3.0)
 #Save density plot to folder "dir"
plt.savefig("covid/Covid_acum_cases_states.png", dpi=300)
plt.close()

fig = plt.figure(figsize=(10, 10))
plt.scatter(intercept, slopes)
for i, state in enumerate(mex.state_abrv2name):  
    plt.annotate(state, (intercept[i], slopes[i]))

plt.xlabel("intercept")
plt.ylabel("slope")

plt.savefig("covid/Covid_acum_cases_states_reg_adj.png", dpi=300)
plt.close()

fig = plt.figure(figsize=(10, 10))
plt.scatter(covid_cases_compare, slopes)
for i, state in enumerate(mex.state_abrv2name):  
    plt.annotate(state, (covid_cases_compare[i], slopes[i]))

plt.xlabel("day to reach "+str(covid_cases_compare_line)+" cases")
plt.ylabel("slope")

plt.savefig("covid/Covid_acum_cases_states_cut_line.png", dpi=300)
plt.close()


#Dengue data!
