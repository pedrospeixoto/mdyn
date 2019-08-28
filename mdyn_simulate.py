#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
import warnings
warnings.filterwarnings("ignore")

from mdyn_main import MobileDynamics
from mdyn_extras import daterange

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

meanmats = {}
devmats = {}
for (mat, weekday) in zip(tmat, dayoftheweek):
    if weekday in meanmats:
        local_mat = meanmats.get(weekday)
        local_mat = local_mat + mat
    else:
        meanmats[weekday]= mat
    
    
print(meanmats)
