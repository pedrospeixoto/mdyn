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
        
        files = glob.glob(self.data_dir+self.data_file+"*mobility*")
        mobility_file = max(files, key=os.path.getctime)
        files = glob.glob(self.data_dir+self.data_file+"*diagonal*")
        diagonal_file = max(files, key=os.path.getctime)
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

        self.data_mobility_file=os.path.basename(mobility_file)
        self.data_diagonal_file=os.path.basename(diagonal_file)
        print("Reading data from latest data file available in datalake dir: " , self.data_mobility_file, self.data_diagonal_file)

        dfmob = dd.read_csv(self.data_dir+self.data_mobility_file)
        print("Data columns mobility:", dfmob.columns)
        dfdiag = dd.read_csv(self.data_dir+self.data_diagonal_file)
        print("Data diagonal columns:", dfdiag.columns)
        print()
        #Loop over folders with days 
        for day in mex.daterange(self.date_ini_obj, self.date_end_obj+timedelta(days=1)):
            day_str=day.strftime("%Y-%m-%d")
            print("Extracting day: ", day_str)
            
            # define filtering logic
            df_mob_day = dfmob[dfmob['date'] == day_str]
            df_diag_day = dfdiag[dfdiag['date'] == day_str]
            df_mob_day = df_mob_day.compute()
            df_diag_day = df_diag_day.compute()
            if df_mob_day.empty or df_diag_day.empty:
                print("no data for this day, skipping")
                print()
                continue

            day_dir = self.data_dir+"date0="+day_str+"/"
            if not os.path.exists(day_dir):
                os.makedirs(day_dir)
            #print(df_day)

            #df_city = df.groupby(['home_state', 'home_city', 'home_city_code', 
            #    'dest_state', 'dest_city', 'dest_city_code']).sum()
            table_mob = pd.pivot_table(df_mob_day, values='ids_moved_weighted', index=['dest_city_code'],
                            columns=['home_city_code'], aggfunc=np.sum, fill_value=0, dropna=False)
            orig_names = table_mob.columns
        
            dest_names = table_mob.index
            mat = table_mob.as_matrix(columns=None)
            
            
            mat, mat_normed, orig_names, dest_names = network.fix_transition_matrix(mat, orig_names, dest_names)

            n , m = mat.shape
            if n!=m:
                print("Matrix not square!!!")
                sys.exit(1)

            #Fix diagonal be one where zero, just to avoid null divisions
            #now add fixed users to diagonal
            print("Fixing diagonal...wait a while")
            for i in  range(n):
                
                line=df_diag_day[df_diag_day['home_city_code'] == orig_names[i]]
                if line.empty:
                    print("Warning: City with no data, ignoring", i, orig_names[i])
                elif len(line) > 1:
                    print("Warning: City with redundant data, ignoring", i, orig_names[i], line.size, line.home_city.values)
                else:
                    stay_origin=line.stay_home_ids.values[0]
                    moved_out=line.total_ids_went_outside.values[0]
                    total_ids=line.total_ids.values[0]
                    city=line.home_city.values[0]
                    mat[i,i] = mat[i,i] + stay_origin
                    if moved_out/total_ids > 0.5 and total_ids > 1000 :
                        print("City with a lot of movement:", i, city, orig_names[i], mat[i,i], stay_origin, moved_out, moved_out/total_ids)
                    if stay_origin > 1000000:
                        print("Big city:" , i, city, orig_names[i], mat[i,i], stay_origin, moved_out, moved_out/total_ids)
                
            name = "move_mat_"+network.domain+"_"+network.subdomains
        
            np.savetxt( day_dir+name+".csv", mat)
            np.save( day_dir+name+".npy", mat)
            reg=list(network.regions.values())
            with open(day_dir+name+"_reg_names.txt", "w") as output:
                for r in reg:
                    output.write("%s\n" % r)

            print("Created matrices and saved them here: " , day_dir)
            print()