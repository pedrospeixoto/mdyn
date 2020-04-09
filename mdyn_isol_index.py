import sys
import os


import statistics
import numpy as np
import pandas as pd
import math

from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.colors as colors
import matplotlib.cm as cm

import tqdm as tqdm

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

from mdyn_map import Map
import mdyn_extras as mex

def isol_index(network, ipar):
    
    data_dir = ipar.data_dir 

    #Load data
    base_name = network.domain+"_"+network.subdomains
    df = mex.read_orc2df(data_dir, base_name, True)
    
    df = org_data(df, network, data_dir)
    
    vars = ['total_users', 'fixed_users', 'IsoIndex']

    ipar.date_ini_obj = datetime.strptime(ipar.date_ini, '%Y-%m-%d')
    ipar.date_end_obj = datetime.strptime(ipar.date_end, '%Y-%m-%d')
    ipar.days = (ipar.date_end_obj - ipar.date_ini_obj).days + 1
    
    for day in mex.daterange(ipar.date_ini_obj, ipar.date_end_obj+timedelta(days=1)):
        #Filter data frame per day
        day_str=day.strftime("%Y-%m-%d")
        print("Calculating isolation index for day: ", day_str)
        day_filter = df['day'] == day_str 
        df_local = df[day_filter]

        for var in vars:
            table = df_local.pivot_table(values=var, index=['lat'],\
                            columns=['lon'], aggfunc=np.average, fill_value=0, dropna=False)

            map = Map(network)  
            title = var+"_"+day_str
            filename = "dump/"+var+"_"+day_str
            lon = np.array(table.columns)
            lat = np.array(table.index)
            mat = table.as_matrix(columns=None)
            map.map_lat_lon_z_data(lat, lon, mat, title, filename)


def org_data(dflocal, network, data_dir):
        #Organize data into nice dataframe

        n=len(dflocal)
        
        pklfile = data_dir+network.domain+"_"+network.subdomains+"_data_proc.pkl"
        
        if not os.path.exists(pklfile):
            #Convert lat lon from dict to numpy array
            loc = dflocal.location
            lat = np.array([list(iloc.values())[0] for iloc in loc])
            lon = np.array([list(iloc.values())[1] for iloc in loc])
            dflocal['lat']=lat
            dflocal['lon']=lon
            
            #add regions
            lonnan=np.isnan(lon)
            latnan=np.isnan(lat)
            nan = lonnan*latnan
            ilon=((lon[~nan]- network.minlons)/network.dlon).astype(int)
            ilat=((lat[~nan]- network.minlats)/network.dlat).astype(int)
            
            reg = np.zeros(n).astype(int)
            reg[nan] = -1
            reg[~nan]=network.region_grid[ilat, ilon]
            dflocal['reg']=reg

            #Calc isolation index
            dflocal['IsoIndex'] = dflocal['fixed_users']/dflocal['total_users']


            filename = data_dir+network.domain+"_"+network.subdomains+"_data_proc.pkl"
            print("Saving pre-processed data-file for future use:", filename)
            dflocal.to_pickle (filename) 

        return dflocal