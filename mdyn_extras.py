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

#General functions
#--------------------------------

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
    return np.array([geopy.distance.vincenty([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)

def distance_lon(lon, lat, lon1,lat1):
    dist = np.array([geopy.distance.vincenty([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)
    signdistlon = np.sign(lon1-lon)
    return signdistlon*dist

def distance_lat(lon, lat, lon1,lat1):
    dist = np.array([geopy.distance.vincenty([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)
    signdistlat = np.sign(lat1-lat)
    return signdistlat*dist


