#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
import numpy as np
import pandas as pd
pd.options.mode.chained_assignment = None  # default='warn'

import math
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.cm as mplcm
from matplotlib.ticker import ScalarFormatter
from matplotlib.ticker import FormatStrFormatter
from adjustText import adjust_text

import statsmodels.api as sm

import difflib

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

import geopy.distance

import tqdm as tqdm

import mdyn_extras as mex

#Garbage collection
import gc

#font = {'family' : 'normal',
#        'weight' : 'normal',
#        'size'   : 22}

#mpl.rc('font', **font)
import matplotlib.pyplot as plt

plt.rc('xtick',labelsize=22)
plt.rc('ytick',labelsize=22)
#plt.xticks(fontsize=14)

print("-------------------------------")
print("Covid World Evolution Analysis")
print("-------------------------------")


#Input parameters - dir name
#-----------------------------
covid_file = "covid/covid_world_evol.csv" #sys.argv[1]
dengue_file = "covid/dengue_world.csv"
dump_dir = "covid/figures/"

#Covid analysis

#Load data
covid = pd.read_csv(covid_file)

print(covid)
print(covid.columns)
covid['date'] = pd.to_datetime(covid["dateRep"])
country_names = covid.countriesAndTerritories.unique()

print(covid)
print(covid.columns)
print(country_names)


dengue = pd.read_csv(dengue_file)


print(dengue)
print(dengue.columns)

country_names_dengue = dengue.Subregion.unique()
print(country_names_dengue)

for i, country in enumerate(country_names_dengue):
    c = country.replace(" ", "_")
    if c not in country_names:
        #print(i, c, "found it!")
        matches = difflib.get_close_matches(c, country_names)
        print(i, c, matches, "can't find this country on list")
        sys.exit()

print(dengue)
print(dengue.columns)

print()
print()

#filter Africa
covid = covid[covid['continentExp'] != "Africa"]
country_names = covid.countriesAndTerritories.unique()

#filter population
pop_lim = 00000
covid = covid[covid['popData2019']>pop_lim]
country_names = covid.countriesAndTerritories.unique()

#clean dengue data with filters
for i, country in enumerate(country_names_dengue):
    c = country.replace(" ", "_")
    if c not in country_names:
        indexNames = dengue[ dengue['Subregion'] == country ].index
        dengue.drop(indexNames , inplace=True)

country_names_dengue = dengue.Subregion.unique()
print(country_names_dengue)

# Initialize the figure
#plt.style.use('seaborn-darkgrid')
plt.style.use('seaborn-paper')
 
# create a color palette
palette = plt.get_cmap('Set1')
colors=palette #(np.linspace(0, 1.0, 5))
#colors=palette(np.linspace(0, 5, endpoint=True))
#print(colors)
#sys.exit()

n = len(country_names_dengue)
slopes = np.zeros((n))
intercept = np.zeros((n))
covid_cases_compare = np.zeros((n))
r = np.zeros((n))
acum_cases_todate = np.zeros((n))
days_to_cases_line = np.zeros((n))
    
covid_cases_cut_min = 0.01
time_cut_min = 0
time_cut_max = 90
covid_cases_compare_line = 100



#Set colors
cmap = plt.cm.rainbow
norm = mpl.colors.LogNorm(vmin=0.1, vmax=np.max(dengue['dengue_inc'].values))

fig, axs = plt.subplots(1,1, figsize=(20, 20), squeeze=False)
axs = axs.ravel()

i_ax = 0
texts = []
for i, c in enumerate(country_names):   
            
    #Get data
    covid_country = covid[covid['countriesAndTerritories'] == c]
    var = "Cumulative_number_for_14_days_of_COVID-19_cases_per_100000"

    acum_cases = covid_country[var].values
    days_date = covid_country['date'].values

    #Search for initial threshhold
    with np.errstate(invalid='ignore'):
        ilimit = np.argwhere(acum_cases > covid_cases_cut_min)

    if any(ilimit):
        ilim = ilimit[0].astype('int')[0]
    else:
        ilim = len(acum_cases)-1
        print(c, "Warning!!! Did not reach number of cases threshold!!!")
        continue
    ini_date = days_date = days_date[ilim]
    
    covid_country['days']=covid_country['date']
    covid_country['days']=(covid_country['date']-ini_date).dt.days
    covid_country = covid_country[covid_country['days'] > 0]

    acum_cases = covid_country[var].values/14

    for i, val in enumerate(acum_cases):
        if i == 0:
            pass
        else:
            acum_cases[i] = acum_cases[i] + acum_cases[i-1]

    days_date = covid_country['date'].values
    days = covid_country['days'].values

    #filter time
    days_reg = days[time_cut_min:time_cut_max]
    acum_cases_reg = acum_cases[time_cut_min:time_cut_max]
    print(c, days_reg[0], days_date[0], acum_cases[0])
    
    c_spaces = c.replace("_", " ")
    if c_spaces in country_names_dengue:
        i_dengue = list(country_names_dengue).index(c_spaces)
        
        # Plot the lineplot
        #plt.subplot(6,5,i+1)
        #axs[i].plot(days, acum_cases, marker='', color=colors(region[state]), linewidth=1.5, alpha=0.9, label=state)
        dengue_val = dengue[dengue['Subregion']==c_spaces].dengue_inc.values[0]
        
        if dengue_val < 0.001:
            axs[i_ax].plot(days_reg, acum_cases_reg, marker='', color='grey', linewidth=0.6, alpha=0.6, label=None)    
        else:
            print(i, i_dengue, c_spaces, dengue_val)
            axs[i_ax].plot(days_reg, acum_cases_reg, marker='', linewidth=3.0, label=c, color=cmap(norm(dengue_val)), alpha=0.9)
            #axs[i].plot(days_reg, np.power(2.0, fitted), marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0, label=state)
            axs[i_ax].set_yscale('log')
            axs[i_ax].set_xscale('log')
            #axs[i_dengue].set_xlim(left=10)
            if len(c) > 10 :
                c_tmp=c_spaces[0:10]
            else:
                c_tmp=c_spaces
            #axs[i_ax].set_title(c_tmp, loc='left', fontsize=18, fontweight=0)
            axs[i_ax].tick_params(axis = 'both', which = 'major', labelsize = 14)
            dengue_val_string = "{:.2f}".format(dengue_val)

            texts.append(plt.text(days[-1]+90, 
                acum_cases_reg[-1], c+" "+dengue_val_string, fontsize=10, color=cmap(norm(dengue_val)), alpha=0.9))
    else:
        axs[i_ax].plot(days_reg, acum_cases_reg, marker='', color='grey', linewidth=0.6, alpha=0.6, label=None)

axs[i_ax].spines['right'].set_visible(False) 
axs[i_ax].spines['top'].set_visible(False) 

plt.xlim(1, time_cut_max+150)
plt.xlabel("Number of days from incidence of "+str(covid_cases_cut_min), fontsize=16)
plt.ylabel("Acumulated COVID-19 incidence with 14 days moving average", fontsize=16)
textstr = "Country - Dengue 2019+2020 incidence"
plt.gcf().text(0.9, 0.95, textstr, fontsize=12, horizontalalignment='center', 
    verticalalignment='center', transform=axs[i_ax].transAxes)

#plt.legend(loc='best', fontsize=16, ncol=2)
#fig.delaxes(axs[27])
#fig.delaxes(axs[28])
#fig.delaxes(axs[29])


adjust_text(texts, autoalign="y", only_move={ #'points': 'y',
    'text':'y', 'objects':'y'})
fig.tight_layout(pad=3.0)
#Save density plot to folder "dir"
plt.savefig(dump_dir+"world_covid_acum_join_"+var+".png", dpi=300)
plt.close()

sys.exit()

fig, axs = plt.subplots(9,9, figsize=(30, 30), squeeze=False)
axs = axs.ravel()

for i, c in enumerate(country_names):   
            
    #Get data
    covid_country = covid[covid['countriesAndTerritories'] == c]
    var = "Cumulative_number_for_14_days_of_COVID-19_cases_per_100000"

    acum_cases = covid_country[var].values
    days_date = covid_country['date'].values

    #Search for initial threshhold
    with np.errstate(invalid='ignore'):
        ilimit = np.argwhere(acum_cases > covid_cases_cut_min)

    if any(ilimit):
        ilim = ilimit[0].astype('int')[0]
    else:
        ilim = len(acum_cases)-1
        print(c, "Warning!!! Did not reach number of cases threshold!!!")
        continue
    ini_date = days_date = days_date[ilim]
    
    covid_country['days']=covid_country['date']
    covid_country['days']=(covid_country['date']-ini_date).dt.days
    covid_country = covid_country[covid_country['days'] > 0]

    acum_cases = covid_country[var].values/14

    for i, val in enumerate(acum_cases):
        if i == 0:
            pass
        else:
            acum_cases[i] = acum_cases[i] + acum_cases[i-1]

    days_date = covid_country['date'].values
    days = covid_country['days'].values

    #filter time
    days_reg = days[time_cut_min:time_cut_max]
    acum_cases_reg = acum_cases[time_cut_min:time_cut_max]
    print(c, days_reg[0], days_date[0], acum_cases[0])
    
    if False:
        #Set regression
        X = np.log2(days_reg[:time_cut_max])
        y = np.log2(acum_cases_reg[:time_cut_max])
        X = sm.add_constant(X)
        #log(acum_Cases)=a+b*log(day)
        model = sm.OLS(y, X)
        results = model.fit()
        fitted = results.fittedvalues
        #print(results.summary())

        intercept[i] = results.params[0]
        slopes[i] = results.params[1]
        r[i] = np.sqrt(results.rsquared)
        
        covid_cases_compare[i] = np.power(2.0, (np.log2(covid_cases_compare_line)-intercept[i])/slopes[i])
        print(var, state, covid_cases_compare[i], intercept[i], slopes[i], acum_cases_todate[i], days_date[-1], days_to_cases_line[i], r[i], results.rsquared, results.pvalues[1] ) 
        f.write(" %s , %d, %s, %d, %d, %d, %s, %d, %f , %f, %f, %f, %f, %f, %f , %f\n" % (var, pop, last_date_str, time_cut_min, time_cut_max, covid_cases_cut_min, \
            state, covid_cases_compare_line, covid_cases_compare[i], intercept[i], slopes[i], acum_cases_todate[i], days_to_cases_line[i], r[i], results.rsquared, results.pvalues[1]))

    for j in range(n):
        
        #plt.subplot(6,5,j+1)
        acum_cases_filt = acum_cases
        #acum_cases_filt[ acum_cases==0 ] = np.nan
        axs[j].plot(days, acum_cases_filt, marker='', color='grey', linewidth=0.6, alpha=0.2)
    
    c_spaces = c.replace("_", " ")
    if c_spaces in country_names_dengue:
        i_dengue = list(country_names_dengue).index(c_spaces)
        print(i, i_dengue, c_spaces)
        # Plot the lineplot
        #plt.subplot(6,5,i+1)
        #axs[i].plot(days, acum_cases, marker='', color=colors(region[state]), linewidth=1.5, alpha=0.9, label=state)
        axs[i_dengue].plot(days, acum_cases, marker='', linewidth=3.0, label=c)
        #axs[i].plot(days_reg, np.power(2.0, fitted), marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0, label=state)
        axs[i_dengue].set_yscale('log')
        axs[i_dengue].set_xscale('log')
        #axs[i_dengue].set_xlim(left=10)
        if len(c) > 10 :
            c_tmp=c_spaces[0:10]
        else:
            c_tmp=c_spaces
        axs[i_dengue].set_title(c_tmp, loc='left', fontsize=18, fontweight=0)
        axs[i_dengue].tick_params(axis = 'both', which = 'major', labelsize = 14)

#fig.delaxes(axs[27])
#fig.delaxes(axs[28])
#fig.delaxes(axs[29])

fig.tight_layout(pad=3.0)
#Save density plot to folder "dir"
plt.savefig(dump_dir+"world_covid_acum_"+var+".png", dpi=300)
plt.close()
        
