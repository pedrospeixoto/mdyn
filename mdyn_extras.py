#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os

import numpy as np
import pandas as pd
import math

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

import geopy.distance

#Garbage collection
import gc

#General functions
#--------------------------------

def round_up(n, decimals=0): 
    multiplier = 10 ** decimals 
    return math.ceil(n * multiplier) / multiplier

def round_up(n, decimals=0): 
    multiplier = 10 ** decimals 
    return np.round(math.ceil(n * multiplier) / multiplier ,  decimals)
    
def round_down(n, decimals=0):
    multiplier = 10 ** decimals
    return np.round(math.floor(n * multiplier) / multiplier, decimals)

#Range over date variable
def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)


#Convert timestamp to datetime
def timestamp2datetime(ts):
    try:
        tmp=int(int(ts)/1000)
        return datetime.utcfromtimestamp(tmp)
    except:
        return np.nan

#Get picle files
def list_files_pkl(directory):
    return (f for f in os.listdir(directory) if f.endswith('.pkl'))

def distance(lon, lat, lon1,lat1):
    return np.array([geopy.distance.geodesic([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)

def distance_lon(lon, lat, lon1,lat1):
    dist = np.array([geopy.distance.geodesic([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)
    signdistlon = np.sign(lon1-lon)
    return signdistlon*dist

def distance_lat(lon, lat, lon1,lat1):
    dist = np.array([geopy.distance.geodesic([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)
    signdistlat = np.sign(lat1-lat)
    return signdistlat*dist

def del_df(df):
        
        del [df]
        gc.collect()
        df=pd.DataFrame()
        #df=df_tmp.copy()
        

def mem_usage(pandas_obj):
    if isinstance(pandas_obj,pd.DataFrame):
        usage_b = pandas_obj.memory_usage(deep=True).sum()
    else: # we assume if not a df it's a series
        usage_b = pandas_obj.memory_usage(deep=True)
    usage_mb = usage_b / 1024 ** 2 # convert bytes to megabytes
    return "{:03.2f} MB".format(usage_mb)
        
def matprint(mat, fmt="g"):
    col_maxes = [max([len(("{:"+fmt+"}").format(x)) for x in col]) for col in mat.T]
    for x in mat:
        for i, y in enumerate(x):
            print(("{:"+str(col_maxes[i])+fmt+"}").format(y), end="  ")
        print("")