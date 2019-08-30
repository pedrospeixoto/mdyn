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

# RUN mdyn_run first !!!!
#Initialize mobile data and load data to dataframe 
mdyn=MobileDynamics(sys.argv)

#Read transition matrices
tmat = [] #List transition matrices
days = []               
dayoftheweek = []

fig, ax = plt.subplots(3, 3)


#Loop over folders with days and collect transition matrices
for day in daterange(mdyn.date_ini_obj, mdyn.date_end_obj+timedelta(days=1)):
    #print(day)
    #Load data for this day
    days.append(day.strftime("%Y-%m-%d"))
    dayoftheweek.append(day.weekday())
    #DayData(day_str, self.data_dir)

    local_dir = mdyn.data_dir+"dt="+day.strftime("%Y-%m-%d")+"/"
    tmat_local = np.genfromtxt(local_dir+'trans_mat.csv')
    if day.weekday() == 0:
        print("Monday", day, tmat_local[1,2])
        matprint(tmat_local)
        lines = []
        for col in range(9):
            i=int(col/3)
            j=col%3
            line , = ax[i,j].plot(tmat_local[:,col], label=day.strftime("%d-%m-%Y"))
            lines.append(line)
            ax[i, j].set_yscale('log')        
            ax[i, j].set_ylabel('P of Trans from reg: '+str(col))
            ax[i, j].set_xlabel('Destination region')
            if i==2 and j==2:
                ax[i,j].legend(bbox_to_anchor=(1.1, 1.0))
    tmat.append(tmat_local)

regions="{0: 'GrandeSP', 1: 'Campinas', 2: 'Jundiai', 3: 'SJC', 4: 'Santos', 5: 'Sorocaba', 6: 'Pira', 7: 'RP', 8: 'Franca', 9: 'SJRP', 10: 'MG', 11: 'RJ', 12: 'PR', 13: 'MS'}"
fig.suptitle("Monday->Tuesday "+regions)

plt.annotate(regions, xy=(0, 0), xytext=(0.01, 0.001), fontsize=12)

#Initialize generic matrices
(m,n)=tmat[1].shape
print(m,n)
zeromat = np.zeros([m, n], dtype = float) 
idmat = np.eye(n, dtype = float) 

#Init stat matrices
mean_dow_mats = [zeromat]*7
dev_dow_mats = [zeromat]*7
count_dow_mats = [0]*7

#Mean values per week days
for (mat, weekday) in zip(tmat, dayoftheweek):
    mean_dow_mats[weekday] = mean_dow_mats[weekday] + mat
    count_dow_mats[weekday] = count_dow_mats[weekday] + 1    
for i, j, mat in zip(range(7), count_dow_mats, mean_dow_mats):
    i, j, mat
    mean_dow_mats[i]=mat/j

for col in range(9):
    i=int(col/3)
    j=col%3
    ax[i,j].plot(mean_dow_mats[0][:, col], '--', label="Mean",  linewidth=5)
    ax[i, j].set_yscale('log')        

plt.show()

#Std dev
for (mat, weekday) in zip(tmat, dayoftheweek):
    dev_dow_mats[weekday] = np.square(np.divide(mean_dow_mats[weekday] - mat, mean_dow_mats[weekday])) + dev_dow_mats[weekday]
for i, j, mat in zip(range(7), count_dow_mats, dev_dow_mats):
    i, j, mat
    dev_dow_mats[i]=np.sqrt(mat/j)

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

