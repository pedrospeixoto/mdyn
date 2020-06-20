#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import numpy as np
import pandas as pd
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
        
        self.data_file=os.path.basename(latest_file)
        print("Reading data from latest data file available in datalake dir: " , self.data_file)

        df = pd.read_csv(self.data_dir+self.data_file)
        print(df.columns)

        #loop over desired dates

        
        #df_city = df.groupby(['home_state', 'home_city', 'home_city_code', 
        #    'dest_state', 'dest_city', 'dest_city_code']).sum()
        table = pd.pivot_table(df, values='ids', index=['dest_city_code'],
                            columns=['home_city_code'], aggfunc=np.sum, fill_value=0, dropna=False)
        orig_names = table.columns
        #print(reg0)
        dest_names = table.index
        mat = table.as_matrix(columns=None)

        print(mat)
        mat, mat_normed, orig_names, dest_names = network.fix_transition_matrix(mat, orig_names, dest_names)

        name = "move_mat_"+network.domain+"_"+network.subdomains
        
        np.savetxt( self.data_dir+name+".csv", mat)
        np.save( self.data_dir+name+".npy", mat)
        #np.savetxt( day_data.local_dir+name+"_norm.csv", day_data.tmat_norm)
        #np.savetxt( day_data.local_dir+name+"_reg0.csv", day_data.reg0)
        #np.savetxt( day_data.local_dir+name+"_reg1.csv", day_data.reg1)
        #np.save( day_data.local_dir+name+"_reg_names.npy", network.regions)
        reg=list(network.regions.values())
        with open(self.data_dir+name+"_reg_names.txt", "w") as output:
            for r in reg:
                output.write("%s\n" % r)

