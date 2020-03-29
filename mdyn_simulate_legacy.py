#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
import warnings
warnings.filterwarnings("ignore")

from mdyn_network import Network
from mdyn_main import MobileDynamics
import mdyn_extras as mex

from datetime import datetime
from datetime import date
from datetime import timedelta

import matplotlib.pyplot as plt

import numpy as np

import calendar

# RUN mdyn_build_model first !!!!

#Initialize mobile data and load data to dataframe 
data_dir = "data/visit_journey_sao_paulo_2019/"
date_ini = "2019-12-01"
date_end = "2019-12-01"
load_data = True 

mdyn = MobileDynamics(
    data_dir = data_dir,
    date_ini = date_ini,
    date_end = date_end,
    load = load_data
    )

#Domain definitions
domain = "SÃO PAULO" #SP or RJ
subdomains = "SP-Mesoreg"
name = domain+"_"+subdomains

#ilog = True
ilog = False
figext = ".jpg"

if ilog:
    ilogstr="_log"
else:
    ilogstr=""

#Read transition matrices
tmat_all = [] #List transition matrices
tmat_dow = [[] for Null in range(7)]   #trans mats split by day of the week

days_all = [] 
days_dow = [[] for Null in range(7)] 
dow_all = []

dow_names = list(calendar.day_abbr)

dates = mdyn.date_ini+"_"+mdyn.date_end
ndays = mdyn.days

markers = mex.markers
thinmarkers = mex.thinmarkers
daterange = mex.daterange

mdyn.collect_move_mat(domain, subdomains)

print(mdyn.days_all)
print(mdyn.movemats)
sys.exit(1)

#Loop work with transitions matrices
for i, day in mdyn.days_all:
    print(i, day)
    print(mdyn.movemats_norm[i])

for day in daterange(mdyn.date_ini_obj, mdyn.date_end_obj+timedelta(days=1)):
    print(day)
    #Load data for this day
    days_all.append(day.strftime("%Y-%m-%d"))
    dow_all.append(day.weekday())
    days_dow[day.weekday()].append(day.strftime("%Y-%m-%d"))

    local_dir = mdyn.data_dir+"dt="+day.strftime("%Y-%m-%d")+"/"

    tmat_local = np.genfromtxt(local_dir+'move_mat_'+name+'.csv')
    print(tmat_local)
    (nlocal, mlocal) = tmat_local.shape
    print(nlocal, mlocal)
    if not ilog:
        #Clean diagonal
        for i in range(nlocal):
            tmat_local[i,i] = np.nan
    tmat_all.append(tmat_local)
    tmat_dow[day.weekday()].append(tmat_local)

print("Days separated by day of the week:")    
print(days_dow)


(m,n)=tmat_all[1].shape
print("Number of regions: ", m,n)
nreg = n

#Initialize generic matrices
zeromat = np.zeros([m, n], dtype = float) 
idmat = np.eye(n, dtype = float) 

#Init stat matrices
mean_dow_mats = [np.copy(zeromat)]*7
mean_all_mats = np.copy(zeromat)

for idow in range(7):
    ndow = len(tmat_dow[idow])

    print("----------------")
    print(dow_names[idow], " : n = "+str(ndow))
    print("----------------")

    figrows = 4
    figcols = 3
    fig, ax = plt.subplots(figrows, figcols, figsize=(12, 10)) #, sharex="all", sharey="all")
    
    for iday, day in enumerate(days_dow[idow]): 
        print(iday, day)
        
        tmat_local = tmat_dow[idow][iday]
        matprint(tmat_local)
        mean_dow_mats[idow] = mean_dow_mats[idow] + tmat_local/ndow

        #Overall mean mat
        mean_all_mats = mean_all_mats + tmat_local/ndays
        
        lines = []
        for col in range(nreg):
            i=int(col/figcols)
            j=col%figcols
            line , = ax[i,j].plot(tmat_local[:,col], label=day, linestyle="None", marker=markers[iday], markersize="5")
            lines.append(line)
        

    #Put mean values in graphs
    for col in range(nreg):
        i=int(col/figcols)
        j=col%figcols
        
        ax[i, j].plot(mean_dow_mats[idow][:, col], label="Mean", marker="_", markersize=20, color="black", linestyle = "None")

        if ilog:
            ax[i, j].set(xlim=(0, 9), ylim=(0.0000001, 1.0))


        ax[i, j].spines['top'].set_visible(False)
        ax[i, j].spines['right'].set_visible(False)
        if ilog:
            ax[i, j].set_yscale('log')       
        ax[i, j].set_ylabel('Prob - Origin: '+str(col))
        if i==figrows-1 :
            ax[i, j].set_xlabel('Destination')
        #if i==0 and j==figcols-1:
        #    ax[i,j].legend(bbox_to_anchor=(1.0, 1.0))
        #plt.sca(ax[i, j])
        plt.xticks(range(nreg))

    ax[figrows-1,figcols-3].legend(bbox_to_anchor=(1.3, 1.2), ncol=4)
    plt.sca(ax[figrows-1,figcols-2])
    ax[figrows-1,figcols-2].axis('off')
    ax[figrows-1,figcols-1].axis('off')

    regions="Regions: 0:GrandeSP  1:Campinas  2:Jundiai  3:SJC  4:Santos  5:Sorocaba  6:Pira  7:RP  8:Franca  9:SJRP"
    #, 10: 'MG', 11: 'RJ', 12: 'PR', 13: 'MS'
    fig.suptitle("Probability of transition between regions from "+dow_names[idow]+" to "+dow_names[(idow+1)%7]+"\n\n"+regions)
    #plt.annotate(regions, xy=(0, 0), xytext=(0.01, 0.001), fontsize=12)
    plt.tight_layout()
    plt.subplots_adjust(top=0.85, bottom=0.08, left=0.1, right=0.95, hspace=0.55,
                    wspace=0.25)
    
    filename="dump/Transitions_"+dow_names[idow]+"_Dates"+dates+ilogstr+figext
    plt.savefig(filename)

#Plot means for each dow

print("----------------")
print("Mean transition matrix")
print("----------------")

matprint(mean_all_mats)
print("")

fig, ax = plt.subplots(figrows, figcols, figsize=(12, 10)) #, sharex="all", sharey="all")

for idow in range(7): 
    print("Mean matrix for dow: ", idow, " ", dow_names[idow])
    tmat_local = mean_dow_mats[idow]
    #mean_all_mats = mean_all_mats + tmat_local/7.0
    matprint(tmat_local)

    lines = []
    for col in range(nreg):
        i=int(col/figcols)
        j=col%figcols
        line , = ax[i,j].plot(tmat_local[:,col], label=dow_names[idow], linestyle="None", marker=markers[idow])
        lines.append(line)
    

#Put mean values in graphs
for col in range(nreg):
    i=int(col/figcols)
    j=col%figcols
    if ilog:
        ax[i, j].set(xlim=(0, 9), ylim=(0.0000001, 1.0))
    ax[i, j].plot(mean_all_mats[:, col],label="Mean", marker="_", markersize=20, color="black", linestyle = "None")
    if ilog:
        ax[i, j].set_yscale('log')       
    ax[i, j].set_ylabel('Prob - Origin: '+str(col))
    if i==figrows-1 :
        ax[i, j].set_xlabel('Destination')
    #if i==0 and j==figcols-1:
    #    ax[i,j].legend(bbox_to_anchor=(1.5, 1.0))
    #plt.sca(ax[i, j])
    plt.xticks(range(nreg))

ax[figrows-1,figcols-3].legend(bbox_to_anchor=(1.3, 1.2), ncol=4)
plt.sca(ax[figrows-1,figcols-2])
ax[figrows-1,figcols-2].axis('off')
ax[figrows-1,figcols-1].axis('off')

fig.suptitle("Probability of transition between regions"+"\n\n"+regions)
plt.tight_layout()
plt.subplots_adjust(top=0.85, bottom=0.08, left=0.1, right=0.95, hspace=0.55,
                    wspace=0.25)

filename="dump/Transitions_Dates"+dates+ilogstr+figext
plt.savefig(filename)



