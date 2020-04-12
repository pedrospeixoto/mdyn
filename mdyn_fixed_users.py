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

class fixed_users():
    def __init__(self, network, ipar):
        
        data_dir = ipar.data_dir 

        #Load data
        base_name = network.domain+"_"+network.subdomains
        #df = mex.read_orc2df(data_dir, base_name, True)
        df = mex.read_pq2df(data_dir, base_name, True)
        df = self.org_data(df, network, data_dir)
        self.df = df
        print(df)

        vars = ['left_home', 'active_users_in_month', 'MoveIndex']
        #vars = ['MoveIndex']
        
        ipar.date_ini_obj = datetime.strptime(ipar.date_ini, '%Y-%m-%d')
        ipar.date_end_obj = datetime.strptime(ipar.date_end, '%Y-%m-%d')
        ipar.days = (ipar.date_end_obj - ipar.date_ini_obj).days + 1

        dates = mex.daterange(ipar.date_ini_obj, ipar.date_end_obj+timedelta(days=1))
        for i, day in enumerate(dates):
            #Filter data frame per day
            day_str=day.strftime("%Y-%m-%d")
            dow = day.weekday()
            print("Calculating move index for day: ", day_str)
            day_filter = df['day'] == day_str 
            df_local = df[day_filter]

            for var in vars:
                if "Index" not in var:
                    table = df_local.pivot_table(values=var, index=['reg'],\
                                columns=[], aggfunc=np.sum, fill_value=0, dropna=False)
                else:
                    table = df_local.pivot_table(values=var, index=['reg'],\
                                columns=[], aggfunc=np.average, fill_value=0, dropna=False)
                #print(table.describe())
                map = Map(network)  
                title = var+"_by_reg_"+day_str+"_"+mex.weekdays[dow]
                filename = ipar.dump_dir+var+"_by_reg_"+day_str
                map.map_reg_var(table[var], network.regions, network, title, filename)

            for var in vars:
                table = df_local.pivot_table(values=var, index=['lat'],\
                                columns=['lon'], aggfunc=np.average, fill_value=0, dropna=False)
                
                map = Map(network)  
                title = var+"_"+day_str+"_"+mex.weekdays[dow]
                filename = ipar.dump_dir+var+"_"+day_str
                lon = np.array(table.columns)
                lat = np.array(table.index)
                mat = table.as_matrix(columns=None)
                map.map_lat_lon_z_data(lat, lon, mat, title, filename)

            
    def org_data(self, dflocal, network, data_dir):
            #Organize data into nice dataframe

            n=len(dflocal)
            
            pklfile = data_dir+network.domain+"_"+network.subdomains+"_data_proc.pkl"
            
            if not os.path.exists(pklfile):
                #Convert lat lon from dict to numpy array
                loc = dflocal.house_location
                lat = np.array([list(iloc.values())[0] for iloc in loc])
                lon = np.array([list(iloc.values())[1] for iloc in loc])

                #Remove data that is outside of the box
                lon=np.where((lon>network.maxlons)|(lon<network.minlons), np.nan, lon)
                lat=np.where((lat>network.maxlats)|(lat<network.minlats), np.nan, lat)

                lonnan=np.isnan(lon)
                latnan=np.isnan(lat)
                nan = lonnan | latnan

                ilon=((lon[~nan]-network.minlons)/network.dlon).astype(int)
                ilat=((lat[~nan]-network.minlats)/network.dlat).astype(int)

                dflocal['lat']=lat
                dflocal['lon']=lon
                
                reg = np.zeros(n).astype(int)
                reg[nan] = -1
                reg[~nan]=network.region_grid[ilat, ilon]
                dflocal['reg']=reg

                #Calc isolation index
                dflocal['MoveIndex'] = dflocal['left_home']/dflocal['active_users_in_month']

                print("Saving pre-processed data-file for future use:", pklfile)
                dflocal.to_pickle(pklfile) 
            else:
                dflocal = pd.read_pickle(pklfile)
                print("Loading pre-processed data-file for future use:", pklfile)

            return dflocal