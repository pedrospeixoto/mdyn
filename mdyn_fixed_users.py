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

class fixed_users_by_date():
    def __init__(self, network, ipar):
        
        data_dir = ipar.data_dir
        data_format = ipar.data_format
        date_ini = ipar.date_ini
        date_end = ipar.date_end
        dump_dir = ipar.dump_dir
        load = ipar.load_data
        
        print("")
        print(" Isolation index Data Analysis")
        print("-----------------------------")
        if not os.path.exists(data_dir):
            print("Directory doesnt exist. Did you type it wrong? ", data_dir)
            sys.exit(1)

        #Ensure we have the "/"
        if data_dir[-1]!="/":
            self.data_dir = data_dir+"/"
        else:
            self.data_dir=data_dir

        self.date_ini = date_ini
        if date_end == None:
            self.date_end = date_ini
        else:
            self.date_end = date_end

        self.date_ini_obj = datetime.strptime(self.date_ini, '%Y-%m-%d')
        self.date_end_obj = datetime.strptime(self.date_end, '%Y-%m-%d')
        self.days = (self.date_end_obj - self.date_ini_obj).days + 1
        
        print("Folder with data:", self.data_dir)
        print("Initial date:", self.date_ini)
        print("Final date:", self.date_end)
        print("Number of days to analyse:", self.days)

        self.load = load
        self.data_format = data_format
        self.dump_dir = dump_dir
        
        if not os.path.exists(self.dump_dir):
            os.makedirs(self.dump_dir)
            
        print("Output folder:", self.dump_dir)

        self.base_name = network.domain+"_"+network.subdomains

        self.proc_data(network)

    def proc_data(self, network):

        #Data is separated by date
        self.days_all = [] #List of dates per day
        self.dates_dirs = [] #directory of day data
        self.dfs = []
        print()
        #Loop over folders with days 
        for day in mex.daterange(self.date_ini_obj, self.date_end_obj+timedelta(days=1)):
            print("Processing day: ", day)

            self.days_all.append(day)

            self.dates_dirs.append(self.data_dir+"day="+day.strftime("%Y-%m-%d")+"/")
            sday=day.strftime("%Y-%m-%d")
            if self.data_format == "Parquet":
                local_dir = self.data_dir+"day="+sday+"/"
                df = mex.read_pq2df(local_dir, "original", True)
                df = self.org_data(df, network, local_dir)
                df['day']=sday
                print(df)
                self.dfs.append(df)
            else:
                print("Invalid file format")

            print()

        df_all = pd.concat(self.dfs)
        outfile = self.dump_dir+network.domain+"_"+network.subdomains+"_"+ \
            self.date_ini+" "+self.date_end+"_iso_index.csv"
        df_all.to_csv(outfile) 
        print(df_all)


            
    def org_data(self, dflocal, network, data_dir):
            #Organize data into nice dataframe

            n=len(dflocal)
            
            pklfile = data_dir+network.domain+"_"+network.subdomains+"_data_proc.pkl"
            csvfile = data_dir+network.domain+"_"+network.subdomains+"_data_proc.csv"

            if not os.path.exists(pklfile):
                #Convert lat lon from dict to numpy array
                lat = dflocal['lat0']
                lon = dflocal['lng0']

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

                dflocal.drop('lat0', axis=1, inplace=True)
                dflocal.drop('lng0', axis=1, inplace=True)

                reg = np.zeros(n).astype(int)
                reg[nan] = -1
                reg[~nan]=network.region_grid[ilat, ilon]
                dflocal['reg']=reg
                
                #Drop place out of region of interest
                dflocal.dropna(subset = ["lat"], inplace=True)
                dflocal.dropna(subset = ["lon"], inplace=True)
                
                dflocal['reg_name']=dflocal['reg']
                dflocal['reg_name']=dflocal['reg_name'].map(network.regions).fillna(dflocal['reg_name'])
                
                dflocal = dflocal.groupby('reg_name', as_index=False).agg({'lat':'mean', 'lon':'mean',
                    'reg':'mean', 'left_home':'sum', 'active_users_in_month':'sum'})
                
                dflocal['move'] = dflocal['left_home']/dflocal['active_users_in_month']
                dflocal['iso'] = 1.0 - dflocal['move']
                print(dflocal)
                print("Saving pre-processed data-file for future use:", pklfile, csvfile)
                dflocal.to_pickle(pklfile) 
                dflocal.to_csv(csvfile) 
            else:
                dflocal = pd.read_pickle(pklfile)
                print("Loading pre-processed data-file:", pklfile)

            return dflocal

class fixed_users_general():
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
        df['date'] = pd.to_datetime(df['day'])
        start_date = '01-02-2020'
        mask = df['date'] >  start_date
        df_local = df.loc[mask]
        table_users = df_local.pivot_table(values=['active_users_in_month'], index=['date'],\
                                columns=['reg'], aggfunc=np.sum, fill_value=0, dropna=False)
        table_move = df_local.pivot_table(values=['left_home'], index=['day'],\
                                columns=['reg'], aggfunc=np.sum, fill_value=0, dropna=False)
        table_index = table_move['left_home']/table_users['active_users_in_month']
        
        print(table_index)
        
        table_index.plot()
        plt.show()
        sys.exit()
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
                title = base_name+"_"+var+"_by_reg_"+day_str+"_"+mex.weekdays[dow]
                filename = ipar.dump_dir+base_name+"_"+var+"_by_reg_"+day_str
                map.map_reg_var(table[var], network.regions, network, title, filename)

            for var in vars:
                table = df_local.pivot_table(values=var, index=['lat'],\
                                columns=['lon'], aggfunc=np.average, fill_value=0, dropna=False)
                
                map = Map(network)  
                title = base_name+"_"+var+"_"+day_str+"_"+mex.weekdays[dow]
                filename = ipar.dump_dir+base_name+"_"+var+"_"+day_str
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