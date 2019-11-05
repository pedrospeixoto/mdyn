#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os


import numpy as np
import pandas as pd
import math
import time

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

from mdyn_daydata import DayData
from mdyn_network import Network
from mdyn_domain import Domain, Map
from mdyn_extras import daterange

import matplotlib.pyplot as plt
from matplotlib import animation

class Incidence:

    def __init__(self, argv):
        self.read_arguments(argv)    

    def read_arguments(self, argv):
        # input directory path
        data_dir = ''
        if len(argv) < 3 :
            print("Arguments required:")
            print("1) A folder name containing the dataset")
            print("2) filename")
            sys.exit(1)
            
        data_dir = argv[1]
        self.filename = argv[2]

        if not os.path.exists(data_dir):
            print("Directory doesnt exists. Did you type it wrong? ", data_dir)
            sys.exit(1)

        #Ensure we have the "/"
        if data_dir[-1]!="/":
            self.data_dir = data_dir+"/"
        else:
            self.data_dir=data_dir

        self.filename=self.data_dir+self.filename
        self.dump_dir = 'dump/'
        
        if not os.path.exists(self.dump_dir):
            os.makedirs(self.dump_dir)
            
        print("Output folder:", self.dump_dir)
               

    def read_data(self):

        #Main dataframe list
        data = pd.read_csv(self.filename, sep=';',  header=None ) 
        self.lat=data.iloc[0]
        #print(self.lat)
        self.lat=self.lat.drop([0]).values
        
        self.lon=data.iloc[1]
        self.lon=self.lon.drop([0]).values

        self.data=data.drop([0,1,2])
        #self.dates=data[0]
        #self.data=data.drop([0], axis=1)
        self.data=self.data.reset_index(drop=True)


    def map(self):

        self.dom = Domain(state="SP", precompdomain = True)
        self.dom.set_global_domain()
        print("ola")
        #print(self.dom.lat_bins_c)
        
        #print(self.data)
        #for i, date in enumerate(self.dates):
        for week_index, row in self.data.iterrows():
            week_data=row[1:-1].values
            #matrix=np.zeros((self.dom.nlat+2, self.dom.nlon+2))
            matrix=np.zeros((self.dom.nlat+2, self.dom.nlon+2))
            print(matrix.shape)
            #print(row[0], week_data)
            for j, lat in enumerate(self.dom.lat_bins_c):
                for i, lon in enumerate(self.dom.lon_bins_c):
                    index=self.get_table_index(lon, lat)
                    if index > 0:
                        print(i,j,lon,lat)
                        print(index, self.lon[index], self.lat[index],week_data[index] )
                        local_data=week_data[index]
                        matrix[j,i]=local_data
                        print("")
            
            

            map = Map(self.dom)
            map.map_data(matrix, "teste", self.dump_dir)
            #print(index, row)
            exit(1)


    def get_table_index(self, lon, lat):
        
        i = -1
        lats=self.lat
        lons=self.lon
        if lon in lons:
            
            ilon_min = np.argmax(lons==lon)
            inv_lons=lons[::-1]
            ilon_max = len(lons) - np.argmax(inv_lons==lon)-1
            imax=min(ilon_max+1,len(lons)) 
            local_lats=lats[ilon_min:ilon_max]
            if lat in local_lats:
                print("teste")
                print(lon, lat)
                print(ilon_min, ilon_max)
                print(local_lats)
                ilat=np.argmax(local_lats==lat)
                i = ilat+ilon_min
                print(lon, lat, ilon_min, ilat, i)
                print()
        return i

        
        
        
        
    


        
