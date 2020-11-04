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
import scipy as sp
import difflib

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

import geopy.distance

import tqdm as tqdm

import mdyn_extras as mex

import seaborn as sns
sns.set()
sns.set_style("white")

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

#print(covid)
#print(covid.columns)
covid['date'] = pd.to_datetime(covid["dateRep"])
country_names = covid.countriesAndTerritories.unique()

print(covid)
print(covid.columns)
print(country_names)


dengue = pd.read_csv(dengue_file)


# Match names between dengue and covid
#print(dengue)
#print(dengue.columns)

country_names_dengue = dengue.Subregion.unique()
for i, country in enumerate(country_names_dengue):
    c = country.replace(" ", "_")
    if c not in country_names:
        #print(i, c, "found it!")
        matches = difflib.get_close_matches(c, country_names)
        print(i, c, matches, "can't find this country on list")
        sys.exit()

print(dengue)
print(dengue.columns)
print(country_names_dengue)

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
colors=sns.color_palette()

# create a color palette
#palette = plt.get_cmap('Set1')
#colors=palette #(np.linspace(0, 1.0, 5))
#colors=palette(np.linspace(0, 5, endpoint=True))
#print(colors)
#sys.exit()

#processing data
data = []

    
covid_cases_cut_min = 10
time_cut_min = 0
time_cut_max = 30
covid_cases_compare_line = 100
dengue_cut_line = 200
r2_cut_line = 0.6

#Set colors
cmap = plt.cm.rainbow
norm = mpl.colors.LogNorm(vmin=0.1, vmax=np.max(dengue['dengue_inc'].values))

fig, axs = plt.subplots(1,2, figsize=(12, 8), squeeze=False)
axs = axs.ravel()

i_ax = 0
texts = []
for i, c in enumerate(country_names):   
    local_data = []
    local_data.append(i)
    local_data.append(c)

    #Get data
    covid_country = covid[covid['countriesAndTerritories'] == c]
    var = "Cumulative_number_for_14_days_of_COVID-19_cases_per_100000"

    
    if np.min(covid_country['popData2019'].values) == np.max(covid_country['popData2019'].values):
        pop = np.max(covid_country['popData2019'].values)
    else:
        print("error in population: ", )
        print(c, "Pop:", np.min(covid_country['popData2019'].values), np.max(covid_country['popData2019'].values))
        sys.exit()

    cases_orig = covid_country['cases'].values
    acum_cases_inc_orig = covid_country[var].values/14
    days_date = covid_country['date'].values
    acum_cases = cases_orig
    acum_cases_inc = acum_cases_inc_orig

    for i, (val, val_inc) in enumerate(zip(cases_orig, acum_cases_inc_orig)):
        if i ==0:

            if np.isnan(val):
                acum_cases[0] = 0
            else:
                acum_cases[0] = cases_orig[0]

            if np.isnan(val_inc):
                acum_cases_inc[0] = 0
            else:
                acum_cases_inc[0] = acum_cases_inc_orig[0]

        else: 
            if np.isnan(val):
                acum_cases[i] = acum_cases[i-1]
            else:
                acum_cases[i] = acum_cases[i-1] + val

            if np.isnan(val_inc):
                acum_cases_inc[i] = acum_cases_inc[i-1]
            else:
                acum_cases_inc[i] = acum_cases_inc[i-1] + val_inc
        #print(c, i, days_date[i], acum_cases_inc[i], 100000*acum_cases[i]/pop, acum_cases[i])

    #sys.exit()
    #Search for initial threshhold
    #with np.errstate(invalid='ignore'):
    ilimit = np.argwhere(acum_cases > covid_cases_cut_min-1)

    if any(ilimit):
        ilim = ilimit[0].astype('int')[0]
    else:
        ilim = len(acum_cases)-1
        print(c, "Warning!!! Did not reach number of cases threshold!!!")
        continue
    ini_date = days_date = days_date[ilim]

    days_date = covid_country['date'].values
    days = (covid_country['date']-ini_date).dt.days
    days = days.values

    #filter time
    days = days + 1
    time_filt = (days > 0) & (days < time_cut_max+1)
    days_date_filt = days_date[time_filt]
    days_filt = days[time_filt]
    acum_cases_filt = acum_cases[time_filt]

    days_reg = days_filt[time_cut_min:time_cut_max]
    
    days_date_reg = days_date_filt[time_cut_min:time_cut_max]
    acum_cases_reg = 100000*acum_cases_filt[time_cut_min:time_cut_max]/pop
    #print(c, days_reg[0], days_date_reg[0], acum_cases_reg[0]) #, len(days_reg), len(days_date_reg), len(acum_cases_reg))
    
    #print()
    #print(c, days_reg[0], days_date_reg[0], acum_cases_reg[0], acum_cases_reg[-1]) #, len(days_reg), len(days_date_reg), len(acum_cases_reg))
    #acum_cases_reg = acum_cases_reg / acum_cases_reg[0]
    #print(c, days_reg[0], days_date_reg[0], acum_cases_reg[0], acum_cases_reg[-1])
    
    #Adjust linear model
    X = np.log2(days_reg[time_cut_min:time_cut_max])
    #X = days_reg[time_cut_min:time_cut_max]
    y = np.log2(acum_cases_reg[time_cut_min:time_cut_max])
    X = sm.add_constant(X)
    #log(acum_Cases)=a+b*log(day)
    model = sm.OLS(y, X)
    results = model.fit()
    fitted = results.fittedvalues
    #print(results.summary())

    #intercept
    local_data.append(results.params[0])
    #slope
    local_data.append(results.params[1])
    #R2
    local_data.append(results.rsquared)
    
    #last covid incidence
    local_data.append(acum_cases_reg[-1])

    #acum_cases_reg = acum_cases_reg/acum_cases_reg[0]
    c_spaces = c.replace("_", " ")
    if c_spaces in country_names_dengue:
        i_dengue = list(country_names_dengue).index(c_spaces)
        
        # Plot the lineplot
        dengue_val = dengue[dengue['Subregion']==c_spaces].dengue_inc.values[0]
        local_data.append(dengue_val)

        if dengue_val < dengue_cut_line:
            #axs[i_ax].plot(days_reg, acum_cases_reg, marker='', color=colors[0], linewidth=1.0, alpha=0.3, label=None) 
            local_data.append(False)
            local_data.append(acum_cases_reg)
            data.append(local_data)
            
        else:
            print("     ", i, i_dengue, c_spaces, dengue_val)
            ##axs[i_ax].plot(days_reg, acum_cases_reg, marker='', linewidth=3.0, label=c, color=cmap(norm(dengue_val)), alpha=0.9)
            #axs[i_ax].plot(days_reg, acum_cases_reg, marker='', linewidth=1.5, label=c, color=colors[1], alpha=0.8)
            ##axs[i].plot(days_reg, np.power(2.0, fitted), marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0, label=state)
            axs[i_ax].set_yscale('log')
            axs[i_ax].set_xscale('log')
            ##axs[i_dengue].set_xlim(left=10)
            #if len(c) > 15 :
            #    c_tmp=c_spaces[0:10]
            #else:
            #    c_tmp=c_spaces
            ##axs[i_ax].set_title(c_tmp, loc='left', fontsize=18, fontweight=0)
            ##axs[i_ax].tick_params(axis = 'both', which = 'major', labelsize = 14)
            dengue_val_string = "{:.0f}".format(dengue_val)

            ##texts.append(plt.text(time_cut_max+15, 
            ##    acum_cases_reg[-1], c[0:14]+" "+dengue_val_string, fontsize=10, color=cmap(norm(dengue_val)), alpha=0.9))
            ##texts.append(plt.text(0.7, 
            ##    acum_cases_reg[0], c[0:14], fontsize=10, color=cmap(norm(dengue_val)), alpha=0.9))
            local_data.append(True)
            local_data.append(acum_cases_reg)
            data.append(local_data)
    else:
        local_data.append(0.0) #dengue data
        local_data.append(False)
        local_data.append(acum_cases_reg)
        #axs[i_ax].plot(days_reg, acum_cases_reg, marker='', color=colors[0], linewidth=1.0, alpha=0.3, label=None) 
        ##axs[i_ax].plot(days_reg, acum_cases_reg, marker='', color='grey', linewidth=0.6, alpha=0.6, label=None)
        data.append(local_data)

    print(local_data)

##axs[i_ax].spines['right'].set_visible(False) 
##axs[i_ax].spines['top'].set_visible(False) 
#axs[i_ax].set_xticks([1, 5, 10, 20, 30])
#axs[i_ax].get_xaxis().set_major_formatter(mpl.ticker.ScalarFormatter())

##plt.xlim(2, time_cut_max)
#axs[i_ax].set_xlabel("Number of days from "+str(covid_cases_cut_min)+" covid cases", fontsize=12)
#axs[i_ax].set_ylabel("Acumulated COVID-19 cases per 100k inhabitants", fontsize=12)
##axs[i_ax].tick_params(axis = 'both', which = 'major', labelsize = 14)

##textstr = "Country - Dengue 2019+2020 incidence"
##plt.gcf().text(0.85, 0.98, textstr, fontsize=12, horizontalalignment='center', 
##    verticalalignment='center', transform=axs[i_ax].transAxes)

##adjust_text(texts, autoalign="y", only_move={ 'points': 'y',
##    'text':'y', 'objects':'y'})

##adjust_text(texts, autoalign="y")
##, autoalign="y", only_move={ #'points': 'y',
##    'text':'y', 'objects':'y'})

df = pd.DataFrame(data, columns=['i', 'country', 'intercept', 'slope', 'r2', 'last_covid_inc', 'dengue_inc', 'dengue', "evolution"])

df = df[df['r2']>0.6]

evol = df['evolution'].values

df_dengue = df[df['dengue']==True]
df_nodengue = df[df['dengue']==False]

df_evol_dengue=pd.DataFrame(df_dengue['evolution'].values.tolist(), index=df_dengue['country'] )
stats_dengue = df_evol_dengue.describe()

df_evol_nodengue=pd.DataFrame(df_nodengue['evolution'].values.tolist(), index=df_nodengue['country'] )
stats_nodengue = df_evol_nodengue.describe(include="all")


print(stats_dengue)
print(stats_nodengue)
i_ax = 0
axs[i_ax].plot(days_reg, df_evol_nodengue.quantile(0.95).values, linestyle=":",  marker='', color=colors[0], linewidth=1.0, alpha=0.5, label="95%") 
axs[i_ax].plot(days_reg, stats_nodengue.loc['75%'], marker='', linestyle="--", color=colors[0], linewidth=1.0, alpha=0.3, label="75%") 
axs[i_ax].plot(days_reg, stats_nodengue.loc['50%'], marker='', linestyle="-", color=colors[0], linewidth=2.0, alpha=1.0, label="50% Low/No Dengue") 
axs[i_ax].plot(days_reg, stats_nodengue.loc['25%'], marker='', linestyle="--", color=colors[0], linewidth=1.0, alpha=0.3, label="25%") 
axs[i_ax].plot(days_reg, df_evol_nodengue.quantile(0.05).values, linestyle=":",  marker='', color=colors[0], linewidth=1.0, alpha=0.5, label=" 5%") 

axs[i_ax].plot(days_reg, df_evol_dengue.quantile(0.95).values, linestyle=":",  marker='', color=colors[1], linewidth=1.0, alpha=0.5, label="95%") 
axs[i_ax].plot(days_reg, stats_dengue.loc['75%'], marker='', linestyle="--", color=colors[1], linewidth=1.0, alpha=0.3, label="75%") 
axs[i_ax].plot(days_reg, stats_dengue.loc['50%'], marker='', linestyle="-", color=colors[1], linewidth=2.0, alpha=1.0, label="50% High Dengue") 
axs[i_ax].plot(days_reg, stats_dengue.loc['25%'], marker='', linestyle="--", color=colors[1], linewidth=1.0, alpha=0.3, label="25%") 
axs[i_ax].plot(days_reg, df_evol_dengue.quantile(0.05).values, linestyle=":",  marker='', color=colors[1], linewidth=1.0, alpha=0.5, label=" 5%") 


axs[i_ax].fill_between(days_reg, stats_nodengue.loc['25%'], stats_nodengue.loc['75%'], facecolor=colors[0], alpha=0.2)
#axs[i_ax].fill_between(days_reg, df_evol_nodengue.quantile(0.05), df_evol_nodengue.quantile(0.95), facecolor=colors[0], alpha=0.06)

axs[i_ax].fill_between(days_reg, stats_dengue.loc['25%'], stats_dengue.loc['75%'], facecolor=colors[1], alpha=0.2)
#axs[i_ax].fill_between(days_reg, df_evol_dengue.quantile(0.05), df_evol_dengue.quantile(0.95), facecolor=colors[1], alpha=0.06)


axs[i_ax].set_yscale('log')
axs[i_ax].set_xscale('log')
axs[i_ax].set_xticks([1, 2, 3, 4, 5, 10, 20, 30])
axs[i_ax].get_xaxis().set_major_formatter(mpl.ticker.ScalarFormatter())
#axs[i_ax].get_xaxis().set_minor_formatter(mpl.ticker.ScalarFormatter())
axs[i_ax].set_xlabel("Number of days from "+str(covid_cases_cut_min)+" covid cases", fontsize=12)
axs[i_ax].set_ylabel("Acumulated COVID-19 cases per 100k (Median and IQ range)", fontsize=12)
axs[i_ax].legend(loc="best",  ncol=2)
#--------------------------boxplots-------------------#

i_ax = 1
print(df.describe())
n_dengue = sum(df['dengue'].values)
print("Dengue:", n_dengue, "Non-dengue:", len(df)-n_dengue)

df_dengue=df[df['dengue']==True]
print(df_dengue.describe())

df_no_dengue=df[df['dengue']==False]
print(df_no_dengue.describe())

mwstat, mwp = sp.stats.mannwhitneyu(df_dengue['slope'], df_no_dengue['slope'])
print(mwstat, mwp)

t_stat, t_p = sp.stats.ttest_ind(df_dengue['slope'], df_no_dengue['slope'], equal_var = False)
t_p=t_p/2 #unilateral
print(t_stat, t_p)

#fig = plt.figure(figsize=(8, 8))

sns.boxplot(x="dengue", y="slope", data=df, ax=axs[i_ax] )
#sns.stripplot(x="dengue", y="slope", data=df, size=4, alpha=0.3, color='black')
sns.swarmplot(x="dengue", y="slope", data=df, size=4, alpha=0.3, color='black', ax=axs[i_ax] )

plt.xlabel("Incidence of Dengue > "+str(dengue_cut_line)+"cases/100k" , fontsize=12)
plt.ylabel("Covid-19 estimated slope - "+str(time_cut_max)+" days", fontsize=12)

textstr = "t-test p="+str(np.round(t_p,4))
plt.gcf().text(0.8, 0.90, textstr, fontsize=10, horizontalalignment='center', 
    verticalalignment='center')

textstr = "Mann-Whitney p="+str(np.round(mwp,4))
plt.gcf().text(0.8, 0.95, textstr, fontsize=10, horizontalalignment='center', 
    verticalalignment='center')

fig.tight_layout()

plt.savefig(dump_dir+"world_covid_evol_boxplot.png", dpi=300)






