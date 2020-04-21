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
        #print(self.dom.lat_bins_c)
        
        #Get 1d indices for 2d lat/lon table
        self.convert1d_2d_latlong()
                
        for week_index, row in self.data.iterrows():
            print("Week:", week_index, " Date: ", row[0])
            week_data=row[1:-1].values
            matrix_data=np.zeros((self.dom.nlat+2, self.dom.nlon+2))
            for j, lat in enumerate(self.dom.lat_bins_c):
                for i, lon in enumerate(self.dom.lon_bins_c):
                    if self.data_avail[j,i] > 0:
                        local_data=week_data[self.pos_index[j,i]]
                        matrix_data[j,i]=local_data
                        
            
            map = Map(self.dom)
            map.map_data(matrix_data, "Incidence"+str(week_index)+"Week-"+str(row[0]), self.dump_dir)
            #print(index, row)


    def convert1d_2d_latlong(self):
        #Organize lat-longs as 2d-table instead of 1d vector
        pos_index=np.zeros((self.dom.nlat+2, self.dom.nlon+2)).astype(int)
        data_avail=np.zeros((self.dom.nlat+2, self.dom.nlon+2)).astype(int)
        for j, lat in enumerate(self.dom.lat_bins_c):
                for i, lon in enumerate(self.dom.lon_bins_c):
                    index=self.get_table_index(lon, lat)
                    pos_index[j,i]=int(index)
                    if index > 0:
                        #print(i,j,lon, lat, pos_index[j,i])
                        data_avail[j,i]=1
        self.pos_index=pos_index
        self.data_avail=data_avail

        map = Map(self.dom)
        map.map_data(pos_index, "Index of Position", self.dump_dir)
        map2 = Map(self.dom)
        map2.map_data(data_avail, "Available Data Positions", self.dump_dir)


    def get_table_index(self, lon, lat):
        #This assumes long/lat are ordered following longitudes then latitudes!!!
        i = -1
        lats=self.lat
        lons=self.lon
        if lon in lons:
            #Get indeces of this long in vector 
            ilon_min = np.argmax(lons==lon)
            inv_lons=lons[::-1]
            ilon_max = len(lons) - np.argmax(inv_lons==lon)-1
            #imax=min(ilon_max+1,len(lons)) 
            #Get list of lats for this lon
            local_lats=lats[ilon_min:ilon_max]
            if lat in local_lats:
                #print("teste")
                #print(lon, lat)
                #print(ilon_min, ilon_max)
                #print(local_lats)
                ilat=np.argmax(local_lats==lat)
                i = ilat+ilon_min
                #print(lon, lat, ilon_min, ilat, i)
                #print()
        return i

        
        
        
        
    


        
