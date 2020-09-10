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
print("Covid vs flavivirus analysis")
print("-------------------------------")


#Input parameters - dir name
#-----------------------------
covid_file = sys.argv[1]
flaviv_file = sys.argv[2]
states_file = "maps/ufebrasil_input_info_all.csv"

dump_dir = "covid/figures/"

#states
states_df = pd.read_csv(states_file)
population = pd.Series(states_df.population.values,index=states_df.abrv).to_dict()
region = pd.Series(states_df.id.values,index=states_df.abrv).to_dict()
region_colors = pd.Series(states_df.cor.values,index=states_df.abrv).to_dict()

for i, reg in enumerate(region):
    region[reg] = math.trunc(region[reg]/10)
    
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
#plt.style.use('seaborn-darkgrid')
plt.style.use('seaborn-paper')
 
# create a color palette
palette = plt.get_cmap('Set1')
colors=palette #(np.linspace(0, 1.0, 5))
#colors=palette(np.linspace(0, 5, endpoint=True))
#print(colors)
#sys.exit()
variables = ["cases", "deaths"]

f= open(dump_dir+"output.csv", "w+")
f.write("covid_var, per100kpop, time_cut_min, time_cut_max, covid_cases_cut_min, \
    state, covid_cases_compare_line, days_reach_cases, intercept, slope, acum_Cases_todate \n")


pop = True
if pop:
    pop_str="100k"
else:
    pop_str=""

for var in variables:

    slopes = np.zeros((n))
    intercept = np.zeros((n))
    covid_cases_compare = np.zeros((n))
    r = np.zeros((n))
    acum_cases_todate = np.zeros((n))
    
    if var == "cases":
        covid_cases_cut_min = 10
        time_cut_min = 0
        time_cut_max = 90
        covid_cases_compare_line = 100
    else:
        covid_cases_cut_min = 1
        time_cut_min = 0
        time_cut_max = 90
        covid_cases_compare_line = 200

    #covid_cases_compare_lines = [1, 10, 50, 100, 1000, 2000, 5000, 10000, 20000]
    covid_cases_compare_lines = [2000]
    #covid_cases_compare_lines = [covid_cases_compare_line]

    for covid_cases_compare_line in covid_cases_compare_lines:
        print(var, covid_cases_compare_line )
        print()
        print("var,  state,  cut_line, intercept, slope, acum_cases, last_day")

        fig, axs = plt.subplots(6,5, figsize=(15, 15), squeeze=False)
        axs = axs.ravel()

        for i, state in enumerate(mex.state_abrv2name):   
            
            #Get data
            covid_state = covid[covid['state'] == state]
            if var == "cases":
                acum_cases = covid_state['totalCases'].values
            else:
                acum_cases = covid_state['deaths'].values

            days = covid_state['days'].values
            days_date = covid_state['date'].values

            acum_cases_todate[i]=acum_cases[-1]

            #Filter time and cases
            filtercases = acum_cases >= covid_cases_cut_min

            days_reg = days[filtercases]
            acum_cases_reg = acum_cases[filtercases]

            days_reg = days_reg[time_cut_min:time_cut_max]
            acum_cases_reg = acum_cases_reg[time_cut_min:time_cut_max]

            if pop:
                acum_cases_reg = 100000.0*acum_cases_reg/population[state]
                acum_cases = 100000.0*acum_cases/population[state]
                acum_cases_todate[i] = 100000.0*acum_cases_todate[i]/population[state]

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
            print(var, state, covid_cases_compare[i], intercept[i], slopes[i], acum_cases_todate[i], days_date[-1] ) 
            f.write("%s , %d, %d, %d, %d, %s, %d, %f , %f, %f, %f \n" % (var, pop, time_cut_min, time_cut_max, covid_cases_cut_min, \
                state, covid_cases_compare_line, covid_cases_compare[i], intercept[i], slopes[i], acum_cases_todate[i]))

            #day_int = 
            for j in range(n):
                
                #plt.subplot(6,5,j+1)
                axs[j].plot(days, acum_cases, marker='', color='grey', linewidth=0.6, alpha=0.3)
            
            # Plot the lineplot
            #plt.subplot(6,5,i+1)
            #axs[i].plot(days, acum_cases, marker='', color=colors(region[state]), linewidth=1.5, alpha=0.9, label=state)
            axs[i].plot(days, acum_cases, marker='', color=region_colors[state], linewidth=2.0, alpha=0.9, label=state)
            axs[i].plot(days_reg, np.power(2.0, fitted), marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0, label=state)
            axs[i].set_yscale('log')
            axs[i].set_xscale('log')
            axs[i].set_xlim(left=10)
            

            axs[i].set_title(state, loc='left', fontsize=12, fontweight=0, color=region_colors[state] )

        fig.delaxes(axs[27])
        fig.delaxes(axs[28])
        fig.delaxes(axs[29])

        fig.tight_layout(pad=3.0)
        #Save density plot to folder "dir"
        plt.savefig(dump_dir+"covid_acum_"+var+pop_str+"_states.pdf", dpi=300)
        plt.close()
       
        if False:
            fig = plt.figure(figsize=(10, 10))
            plt.scatter(intercept, slopes)
            for i, state in enumerate(mex.state_abrv2name):  
                plt.annotate(state, (intercept[i], slopes[i]))

            plt.annotate("$log_2(covid) = a + b * log_2(days)$", (np.max(intercept)-10, (slopes[0]+slopes[3])/2))

            plt.xlabel("covid intercept (a) " +var)
            plt.ylabel("covid slope (b) "+var)

            plt.savefig(dump_dir+"covid_acum_"+var+pop_str+"_states_slope_vs_intercept.pdf", dpi=300)
            plt.close()


            fig = plt.figure(figsize=(10, 10))
            plt.scatter(covid_cases_compare, slopes)
            for i, state in enumerate(mex.state_abrv2name):  
                plt.annotate(state, (covid_cases_compare[i], slopes[i]))

            plt.xlabel("days to reach "+str(covid_cases_compare_line)+" "+ var+"/"+pop_str)
            plt.ylabel("slope")

            plt.savefig(dump_dir+"covid_acum_"+var+pop_str+"_states_cut_line"+str(covid_cases_compare_line)+".pdf", dpi=300)
            plt.close()


            fig = plt.figure(figsize=(10, 10))
            intercept_power = np.power(2.0, intercept)
            plt.scatter(np.power(2.0, intercept), slopes)
            for i, state in enumerate(mex.state_abrv2name):  
                plt.annotate(state, (intercept_power[i], slopes[i]))

            plt.xlabel("Intercept: Estimated "+var+"/"+pop_str+" at day 1")
            plt.ylabel("covid slope "+var)

            plt.savefig(dump_dir+"covid_acum_"+"_"+var+pop_str+"_states_slope_intercept_day1.pdf", dpi=300)
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

        exp_cases = [True] #, False]

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
                
                color_filt = [region_colors.get(st) for st in states_filt]
                #print(color_filt)
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
                #print(results.summary())

                fig = plt.figure(figsize=(10, 10))
                ax = plt.gca() 

                plt.scatter(vec_filt, slopes_filt, c=color_filt, s=50)

                ax.set_yscale('log')
                plt.tick_params(axis='y', which='both', labelsize=14)
                ax.yaxis.set_minor_formatter(FormatStrFormatter("%.1f"))
                plt.tick_params(axis='x', which='both', labelsize=14)
                ax.xaxis.set_minor_formatter(FormatStrFormatter("%.0f"))

                vecinds = vec_filt.argsort()
                vec_sort = vec_filt[vecinds]
                fitted_sort = fitted[vecinds]
                plt.plot(vec_sort, fitted_sort, marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0)

                for i in range(len(vec_filt)):  
                    plt.annotate(states_filt[i], (vec_filt[i]+1, slopes_filt[i]), fontsize=14)

                if results.params[1]>0:
                    pm_sign = "+"
                else:
                    pm_sign = "-"    

                stats="\nr = "+pm_sign+str(np.round(np.sqrt(results.rsquared),2))+ \
                    "  R2 = "+str(np.round(results.rsquared,2))+ \
                    "  p = "+str(np.round(results.pvalues[1], 2))
                #plt.annotate(stats, (vec_sort[-1]-10, fitted_sort[-1]-2), fontsize=14)
                plt.gcf().text(0.02, 0.02, stats, fontsize=10)

                plt.xlabel(name+" sorology", fontsize=14)
                plt.ylabel("covid "+var+" per "+pop_str+" slope", fontsize=14)

                plt.savefig(dump_dir+"covid_"+var+pop_str+"_slope_vs_"+name+exp_str+".pdf", dpi=300)
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
                #print(results.summary())

                fig = plt.figure(figsize=(10, 10))
                ax = plt.gca() 

                plt.scatter(vec_filt, covid_cases_compare_filt, c=color_filt, s=50)
                vecinds = vec_filt.argsort()
                vec_sort = vec_filt[vecinds]
                fitted_sort = fitted[vecinds]
                plt.plot(vec_sort, fitted_sort, marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0)
                ax.set_yscale('log')
                plt.tick_params(axis='y', which='both', labelsize=14)
                ax.yaxis.set_minor_formatter(FormatStrFormatter("%.0f"))
                plt.tick_params(axis='x', which='both', labelsize=14)
                ax.xaxis.set_minor_formatter(FormatStrFormatter("%.0f"))

                for i in range(len(vec_filt)):  
                    plt.annotate(states_filt[i], (vec_filt[i]+1, covid_cases_compare_filt[i]), fontsize=14)

                if results.params[1]>0:
                    pm_sign = "+"
                else:
                    pm_sign = "-"
                #stats="r = "+pm_sign+str(np.round(np.sqrt(results.rsquared),2))+"\n p = "+str(np.round(results.pvalues[1], 2))
                stats="\nr = "+pm_sign+str(np.round(np.sqrt(results.rsquared),2))+ \
                    "  R2 = "+str(np.round(results.rsquared,2))+ \
                    "  p = "+str(np.round(results.pvalues[1], 2))
                #plt.annotate(stats, (vec_sort[-1]-10, fitted_sort[-1]-2), fontsize=14)
                plt.gcf().text(0.02, 0.02, stats, fontsize=10)
                
                
                plt.xlabel(name+" sorology", fontsize=14)
                plt.ylabel("days to reach "+str(covid_cases_compare_line)+" covid "+var+" per "+pop_str, fontsize=14, labelpad=20)

                plt.savefig(dump_dir+"covid_"+var+pop_str+"_cutline"+str(covid_cases_compare_line)+"_vs_"+name+exp_str+".pdf", dpi=300)
                plt.close()

                print()
                print("Covid acum case vs "+name)
                print()
                acum_cases_todate_filt = acum_cases_todate[filter_nan]

                #Set regression
                
                X = vec_filt
                if exp:
                    y = np.log(acum_cases_todate_filt)
                else:
                    y = acum_cases_todate_filt
                X = sm.add_constant(X)
                #log(acum_Cases)=a+b*log(day)
                model = sm.OLS(y, X)
                results = model.fit()
                if exp:
                    fitted = np.exp(results.fittedvalues)
                else:
                    fitted = results.fittedvalues
                #print(results.summary())

                fig = plt.figure(figsize=(10, 10))
                
                ax = plt.gca() 

                plt.scatter(vec_filt, acum_cases_todate_filt, c=color_filt, s=50)
                vecinds = vec_filt.argsort()
                vec_sort = vec_filt[vecinds]
                fitted_sort = fitted[vecinds]
                plt.plot(vec_sort, fitted_sort, marker='', color="black", linestyle='-.', linewidth=1.0, alpha=1.0)

                ax.set_yscale('log')
                plt.tick_params(axis='y', which='both', labelsize=14)
                ax.yaxis.set_minor_formatter(FormatStrFormatter("%.0f"))
                plt.tick_params(axis='x', which='both', labelsize=14)
                ax.xaxis.set_minor_formatter(FormatStrFormatter("%.0f"))

                for i in range(len(vec_filt)):  
                    plt.annotate(states_filt[i], (vec_filt[i]+1, acum_cases_todate_filt[i]), fontsize=14)
                
                if results.params[1]>0:
                    pm_sign = "+"
                else:
                    pm_sign = "-"
                #stats="r = "+pm_sign+str(np.round(np.sqrt(results.rsquared),2))+"\n p = "+str(np.round(results.pvalues[1], 2))
                stats="\nr = "+pm_sign+str(np.round(np.sqrt(results.rsquared),2))+ \
                    "  R2 = "+str(np.round(results.rsquared,2))+ \
                    "  p = "+str(np.round(results.pvalues[1], 2))
                #plt.annotate(stats, (vec_sort[-1]-10, fitted_sort[-1]-2), fontsize=14)
                plt.gcf().text(0.02, 0.02, stats, fontsize=10)

                plt.xlabel(name+" sorology", fontsize=14)
                plt.ylabel("Acumulated covid "+var+" per "+pop_str+" on "+days_date[-1], fontsize=14)

                plt.savefig(dump_dir+"covid_"+var+pop_str+"_acum_vs_"+name+exp_str+".pdf", dpi=300)
                plt.close()


                print("--------------------------------")
