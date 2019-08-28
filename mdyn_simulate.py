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

import numpy as np

# RUN mdyn_run first !!!!
#Initialize mobile data and load data to dataframe 
mdyn=MobileDynamics(sys.argv)

#Read transition matrices
tmat = [] #List transition matrices
days = []               
dayoftheweek = []

#Loop over folders with days 
for day in daterange(mdyn.date_ini_obj, mdyn.date_end_obj+timedelta(days=1)):
    print(day)
    #Load data for this day
    days.append(day.strftime("%Y-%m-%d"))
    dayoftheweek.append(day.weekday())
    #DayData(day_str, self.data_dir)

    local_dir = mdyn.data_dir+"dt="+day.strftime("%Y-%m-%d")+"/"
    tmat_local = np.genfromtxt(local_dir+'trans_mat.csv')
    #print(tmat_local)
    tmat.append(tmat_local)

(m,n)=tmat[1].shape
print(m,n)
zeromat = np.zeros([m, n], dtype = float) 
idmat = np.eye(n, dtype = float) 
meanmats = [zeromat]*7
devmats = [zeromat]*7
count = [0]*7


#Mean values
for (mat, weekday) in zip(tmat, dayoftheweek):
    meanmats[weekday] = meanmats[weekday] + mat
    count[weekday] = count[weekday] + 1
    
for i, j, mat in zip(range(7), count, meanmats):
    i, j, mat
    meanmats[i]=mat/j

#Std dev
for (mat, weekday) in zip(tmat, dayoftheweek):
    devmats[weekday] = np.square(np.divide(meanmats[weekday] - mat, meanmats[weekday])) + devmats[weekday]

for i, j, mat in zip(range(7), count, devmats):
    i, j, mat
    devmats[i]=np.sqrt(mat/j)

#Check matrices
print("Mean mats")
for i, mat in enumerate(meanmats):
    print(i)
    matprint(mat)

print("Dev mats")
for i, mat in enumerate(devmats):
    print(i)
    matprint(mat)

#Periodid matrices
permats = [idmat]*7
for i in range(7):
    print(i)
    for j in range(7):
        #print(i, j)
        k=(i+j) % 7
        permats[i]=np.matmul(permats[i], meanmats[k])
        #matprint(permats[i])
    matprint(permats[i])

