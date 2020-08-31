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
flaviv_file = sys.argv[2]

dump_dir = "covid/figures/"

#Covid analysis

#Load data
covid = pd.read_csv(covid_file)

print(covid)
print(covid.columns)
ref_date = "2020-02-25"
ref_date = datetime.strptime(ref_date, "%Y-%m-%d")
print(ref_date)

covid['datetime']=pd.to_datetime(covid['date'])
covid['days']=(covid['datetime']-ref_date).dt.days

n = len(mex.state_abrv2name)

# Initialize the figure
plt.style.use('seaborn-darkgrid')
 
# create a color palette
palette = plt.get_cmap('Set1')
colors=palette(np.linspace(0, 1.0, n))

variables = ["cases", "deaths"]

for var in variables:

    slopes = np.zeros((n))
    intercept = np.zeros((n))
    covid_cases_compare = np.zeros((n))
    r = np.zeros((n))

    fig, axs = plt.subplots(6,5, figsize=(15, 15), squeeze=False)
    axs = axs.ravel()
    
    if var == "cases":
        covid_cases_cut_min = 10
        time_cut_max = 120
        time_cut_min = 0
        covid_cases_compare_line = 10000
    else:
        covid_cases_cut_min = 1
        time_cut_min = 0
        time_cut_max = 90
        covid_cases_compare_line = 200

    for i, state in enumerate(mex.state_abrv2name):   
        print(state) 

        #Get data
        covid_state = covid[covid['state'] == state]
        if var == "cases":
            acum_cases = covid_state['totalCases'].values
        else:
            acum_cases = covid_state['deaths'].values

        days = covid_state['days'].values

        #Filter time and cases
        filtercases = acum_cases >= covid_cases_cut_min

        days_reg = days[filtercases]
        acum_cases_reg = acum_cases[filtercases]

        days_reg=days_reg[time_cut_min:time_cut_max]
        acum_cases_reg= acum_cases_reg[time_cut_min:time_cut_max]

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
    plt.savefig(dump_dir+"covid_acum_"+var+"_states.png", dpi=200)
    plt.close()

    fig = plt.figure(figsize=(10, 10))
    plt.scatter(intercept, slopes)
    for i, state in enumerate(mex.state_abrv2name):  
        plt.annotate(state, (intercept[i], slopes[i]))

    plt.annotate("$log_2(covid) = a + b * log_2(days)$", (np.max(intercept)-10, (slopes[0]+slopes[3])/2))

    plt.xlabel("covid intercept (a) " +var)
    plt.ylabel("covid slope (b) "+var)

    plt.savefig(dump_dir+"covid_acum_"+var+"_states_slope_vs_intercept.png", dpi=300)
    plt.close()


    fig = plt.figure(figsize=(10, 10))
    plt.scatter(covid_cases_compare, slopes)
    for i, state in enumerate(mex.state_abrv2name):  
        plt.annotate(state, (covid_cases_compare[i], slopes[i]))

    plt.xlabel("days to reach "+str(covid_cases_compare_line)+" "+ var)
    plt.ylabel("slope")

    plt.savefig(dump_dir+"covid_acum_"+var+"_states_cut_line"+str(covid_cases_compare_line)+".png", dpi=300)
    plt.close()


    fig = plt.figure(figsize=(10, 10))
    intercept_power = np.power(2.0, intercept)
    plt.scatter(np.power(2.0, intercept), slopes)
    for i, state in enumerate(mex.state_abrv2name):  
        plt.annotate(state, (intercept_power[i], slopes[i]))

    plt.xlabel("Intercept: Estimated "+var+" at day 1")
    plt.ylabel("covid slope "+var)

    plt.savefig(dump_dir+"covid_acum_"+var+"_states_slope_intercept_Day1.png", dpi=300)
    plt.close()

    #Dengue data!
    #--------------------------------------------------

    #Load data
    flaviv_df = pd.read_csv(flaviv_file)

    print(flaviv_df.columns)

    flaviv_df.sort_values(by=['state'], inplace = True)
    states = flaviv_df["state"].values
    for i, state in enumerate(mex.state_abrv2name):  
            if state != states[i]:
                print(state, states[i])
                print("Warning: states not matching")
                sys.exit()

    print(flaviv_df)
    #i_nan = np.where( (states == 'PR') | (states == 'SC') | (states == 'PA'))
     
    denv_soro = flaviv_df['denv_soro'].values
    ckg_soro = flaviv_df['ckg_soro'].values
    zika_soro = flaviv_df['zika_soro'].values

    #denv_soro[i_nan] = np.nan
    #ckg_soro[i_nan] = np.nan
    #zika_soro[i_nan] = np.nan

    falviv = [denv_soro, ckg_soro, zika_soro]
    falviv_name = ["denv", "ckg", "zika"]

    exp_cases = [True, False]

    for exp in exp_cases:
        if exp:
            exp_str="_exp"
        else:
            exp_str=""

        for i, vec in enumerate(falviv):
            name = falviv_name[i]
            print()
            print("------------------------------")
            print("Flavivirus: " , name)
            print("------------------------------")
            filter_nan = ~np.isnan(vec)
            vec_filt=vec[filter_nan]
            states_filt = states[filter_nan]
            

            ##### --------  Covid slope vs flaviv -----------##############3
            print()
            print("SLOPE vs "+name)
            print()
            #Set regression
            slopes_filt=slopes[filter_nan]

            X = vec_filt
            if exp:
                y = np.log(slopes_filt)
            else:
                y = slopes_filt
            X = sm.add_constant(X)
            #log(acum_Cases)=a+b*log(day)
            model = sm.OLS(y, X)
            results = model.fit()
            if exp:
                fitted = np.exp(results.fittedvalues)
            else:
                fitted = results.fittedvalues
            print(results.summary())

            fig = plt.figure(figsize=(10, 10))
            plt.scatter(vec_filt, slopes_filt)
            vecinds = vec_filt.argsort()
            vec_sort = vec_filt[vecinds]
            fitted_sort = fitted[vecinds]
            plt.plot(vec_sort, fitted_sort, marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0)

            for i in range(len(vec_filt)):  
                plt.annotate(states_filt[i], (vec_filt[i]+1, slopes_filt[i]))

            if results.params[1]>0:
                pm_sign = "+"
            else:
                pm_sign = "-"    

            stats="r = "+pm_sign+str(np.round(np.sqrt(results.rsquared),2))+"\n p = "+str(np.round(results.pvalues[1], 2))
            #plt.annotate(stats, (vec_sort[-1]-10, fitted_sort[-1]-2), fontsize=14)
            plt.gcf().text(0.02, 0.02, stats, fontsize=10)
            plt.xlabel(name+"_soro")
            plt.ylabel("covid "+var+" slope")

            plt.savefig(dump_dir+"covid_"+var+"_slope_vs_"+name+exp_str+".png", dpi=300)
            plt.close()

            ##### --------  Covid-case limit vs flaviv -----------##############3  
            print()
            print("Covid limit case vs "+name)
            print()
            covid_cases_compare_filt=covid_cases_compare[filter_nan]

            #Set regression
            
            X = vec_filt
            if exp:
                y = np.log(covid_cases_compare_filt)
            else:
                y = covid_cases_compare_filt
            X = sm.add_constant(X)
            #log(acum_Cases)=a+b*log(day)
            model = sm.OLS(y, X)
            results = model.fit()
            if exp:
                fitted = np.exp(results.fittedvalues)
            else:
                fitted = results.fittedvalues
            print(results.summary())

            fig = plt.figure(figsize=(10, 10))
            plt.scatter(vec_filt, covid_cases_compare_filt)
            vecinds = vec_filt.argsort()
            vec_sort = vec_filt[vecinds]
            fitted_sort = fitted[vecinds]
            plt.plot(vec_sort, fitted_sort, marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0)

            for i in range(len(vec_filt)):  
                plt.annotate(states_filt[i], (vec_filt[i]+2, covid_cases_compare_filt[i]))

            if results.params[1]>0:
                pm_sign = "+"
            else:
                pm_sign = "-"
            stats="r = "+pm_sign+str(np.round(np.sqrt(results.rsquared),2))+"\n p = "+str(np.round(results.pvalues[1], 2))
            #plt.annotate(stats, (vec_sort[-1]-10, fitted_sort[-1]-2), fontsize=14)
            plt.gcf().text(0.02, 0.02, stats, fontsize=10)

            plt.xlabel(name+"_soro")
            plt.ylabel("days to reach "+str(covid_cases_compare_line)+" covid "+var)

            plt.savefig(dump_dir+"covid_"+var+"_cutline"+str(covid_cases_compare_line)+"_vs_"+name+exp_str+".png", dpi=300)
            plt.close()

