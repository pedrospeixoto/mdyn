#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
import warnings
warnings.filterwarnings("ignore")

from mdyn_main import MobileDynamics
from mdyn_extras import daterange, matprint

from datetime import datetime
from datetime import date
from datetime import timedelta

import matplotlib.pyplot as plt

import numpy as np

import calendar

# RUN mdyn_run first !!!!
#Initialize mobile data and load data to dataframe 
mdyn=MobileDynamics(sys.argv)

#Read transition matrices
tmat_all = [] #List transition matrices
tmat_dow = [[] for Null in range(7)]   #trans mats split by day of the week

days_all = [] 
days_dow = [[] for Null in range(7)] 
dow_all = []

dow_names = list(calendar.day_abbr)


#Loop over folders with days and collect transition matrices
for day in daterange(mdyn.date_ini_obj, mdyn.date_end_obj+timedelta(days=1)):
    #print(day)
    #Load data for this day
    days_all.append(day.strftime("%Y-%m-%d"))
    dow_all.append(day.weekday())
    days_dow[day.weekday()].append(day.strftime("%Y-%m-%d"))

    local_dir = mdyn.data_dir+"dt="+day.strftime("%Y-%m-%d")+"/"
    tmat_local = np.genfromtxt(local_dir+'trans_mat.csv')

    tmat_all.append(tmat_local)
    tmat_dow[day.weekday()].append(tmat_local)

print("Days separated by day of the week:")    
print(days_dow)

(m,n)=tmat_all[1].shape
print("Numeber of regions: ", m,n)
nreg = n

#Initialize generic matrices
zeromat = np.zeros([m, n], dtype = float) 
idmat = np.eye(n, dtype = float) 

#Init stat matrices
mean_dow_mats = [zeromat]*7
dev_dow_mats = [zeromat]*7
count_dow_mats = [0]*7

for idow in range(7):
    ndow = len(tmat_dow[idow])

    print("----------------")
    print(dow_names[idow], " : n = "+str(ndow))
    print("----------------")

    figrows = 2
    figcols = 5
    fig, ax = plt.subplots(figrows, figcols, figsize=(28, 6))
    
    for i, day in enumerate(days_dow[idow]): 
        print(i, day)
        
        tmat_local = tmat_dow[idow][i]
        matprint(tmat_local)
        mean_dow_mats[idow] = mean_dow_mats[idow] + tmat_local/ndow

        lines = []
        for col in range(nreg):
            i=int(col/figcols)
            j=col%figcols
            line , = ax[i,j].plot(tmat_local[:,col], label=day)
            lines.append(line)
            ax[i, j].set_yscale('log')       
            ax[i, j].set_ylabel('P of Trans from reg: '+str(col))
            ax[i, j].set_xlabel('Destination region')
            if i==figrows-1 and j==figcols-1:
                ax[i,j].legend(bbox_to_anchor=(1.1, 1.0))
    
    #Put mean values in graphs
    for col in range(nreg):
        i=int(col/figcols)
        j=col%figcols
        ax[i, j].plot(mean_dow_mats[0][:, col], '--', label="Mean",  linewidth=5)
        ax[i, j].set_yscale('log')       

    regions="Regions: 0:GrandeSP  1:Campinas  2:Jundiai  3:SJC  4:Santos  5:Sorocaba  6:Pira  7:RP  8:Franca  9:SJRP"
    #, 10: 'MG', 11: 'RJ', 12: 'PR', 13: 'MS'
    fig.suptitle(dow_names[idow]+"->"+dow_names[(idow+1)%7]+"\n\n"+regions)
    #plt.annotate(regions, xy=(0, 0), xytext=(0.01, 0.001), fontsize=12)
    plt.tight_layout(pad=20.1)
    plt.savefig("dump/Transitions_"+dow_names[idow]+".eps")


#Check matrices
sys.exit(0)

print("Mean mats")
for i, mat in enumerate(mean_dow_mats):
    print(i)
    matprint(mat)

print("Dev mats")
for i, mat in enumerate(dev_dow_mats):
    print(i)
    matprint(mat)

#Periodic matrices
permats = [idmat]*7
for i in range(7):
    print(i)
    for j in range(7):
        #print(i, j)
        k=(i+j) % 7
        permats[i]=np.matmul(permats[i], mean_dow_mats[k])
        #matprint(permats[i])
    matprint(permats[i])

