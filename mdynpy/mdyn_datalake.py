#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import numpy as np
import pandas as pd
import dask.dataframe as dd

import math
import glob

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

import tqdm as tqdm

import mdynpy.mdyn_extras as mex

#Garbage collection
import gc


class datalake:
    def __init__(self, network, ipar):
        print("")
        print("Processing Datalake Data")
        print("------------------------ ")
        self.data_format = ipar.data_format # "datalake" 
        self.data_dir = ipar.data_dir

        if ipar.data_format == "datalake":
            self.data_file = ipar.data_format
        else:
            print("Cant read this file format with datalake methods. Please check format parameter.")
            sys.exit()
        
        files = glob.glob(self.data_dir+self.data_file+"*")
        latest_file = max(files, key=os.path.getctime)
        #print(latest_file, self.data_dir+self.data_file+"*", self.data_dir, self.data_file)

        self.date_ini = ipar.date_ini
        if ipar.date_end == None:
            self.date_end = ipar.date_ini
        else:
            self.date_end = ipar.date_end

        self.date_ini_obj = datetime.strptime(self.date_ini, '%Y-%m-%d')
        self.date_end_obj = datetime.strptime(self.date_end, '%Y-%m-%d')
        self.days = (self.date_end_obj - self.date_ini_obj).days + 1
        
        print("Folder with data:", self.data_dir)
        print("Initial date:", self.date_ini)
        print("Final date:", self.date_end)
        print("Number of days to analyse:", self.days)

        self.dump_dir = ipar.dump_dir
        
        if not os.path.exists(self.dump_dir):
            os.makedirs(self.dump_dir)
            
        print("Output folder:", self.dump_dir)

        self.data_file=os.path.basename(latest_file)
        print("Reading data from latest data file available in datalake dir: " , self.data_file)

        df = dd.read_csv(self.data_dir+self.data_file)
        print("Data columns:", df.columns)
        print()
        #Loop over folders with days 
        for day in mex.daterange(self.date_ini_obj, self.date_end_obj+timedelta(days=1)):
            day_str=day.strftime("%Y-%m-%d")
            print("Extracting day: ", day_str)
            
            # define filtering logic
            df_day = df[df['date'] == day_str]
            df_day = df_day.compute()
            if df_day.empty:
                print("no data for this day, skipping")
                print()
                continue

            day_dir = self.data_dir+"date0="+day_str+"/"
            if not os.path.exists(day_dir):
                os.makedirs(day_dir)
            #print(df_day)

        #df_city = df.groupby(['home_state', 'home_city', 'home_city_code', 
        #    'dest_state', 'dest_city', 'dest_city_code']).sum()
            table = pd.pivot_table(df_day, values='ids_weighted', index=['dest_city_code'],
                            columns=['home_city_code'], aggfunc=np.sum, fill_value=0, dropna=False)
            orig_names = table.columns
        
            dest_names = table.index
            mat = table.as_matrix(columns=None)

            #print(mat)
            mat, mat_normed, orig_names, dest_names = network.fix_transition_matrix(mat, orig_names, dest_names)

            #now add fixed users to diagonal
            name = "move_mat_"+network.domain+"_"+network.subdomains
        
            np.savetxt( day_dir+name+".csv", mat)
            np.save( day_dir+name+".npy", mat)
            reg=list(network.regions.values())
            with open(day_dir+name+"_reg_names.txt", "w") as output:
                for r in reg:
                    output.write("%s\n" % r)

            print("Created matrices and saved them here: " , day_dir)
            print()