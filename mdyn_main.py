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

from mdyn_daydata import DayData
from mdyn_network import Network
from mdyn_domain import Domain, Map
from mdyn_extras import daterange

class MobileDynamics:

    def __init__(self, argv):
        self.read_arguments(argv)    
        self.read_data()
        self.set_global_domain()

    def read_arguments(self, argv):
        # input directory path
        data_dir = ''
        if len(argv) < 3 :
            print("Arguments requires:")
            print("1) A folder name containing the dataset")
            print("2) Date of begining of analysis in format: 2018-04-01")
            print("3) The final date - if equal, analyse a single day")
            sys.exit(1)
            
        data_dir = argv[1]
        date_ini = argv[2]

        if len(sys.argv) <= 3 :    
            date_end = argv[2]
        else:
            date_end = argv[3]

        if not os.path.exists(data_dir):
            print("Directory doesnt exists. Did you type it wrong? ", data_dir)
            sys.exit(1)

        #Ensure we have the "/"
        if data_dir[-1]!="/":
            self.data_dir = data_dir+"/"
        else:
            self.data_dir=data_dir

        self.date_ini=date_ini
        self.date_end=date_end
        self.date_ini_obj = datetime.strptime(self.date_ini, '%Y-%m-%d')
        self.date_end_obj = datetime.strptime(self.date_end, '%Y-%m-%d')
        self.days = (self.date_end_obj - self.date_ini_obj).days + 1
    
        print("Folder with data:", self.data_dir)
        print("Initial date:", self.date_ini)
        print("Final date:", self.date_end)
        print("Number of days to analyse:", self.days)

        return self

    def read_data(self):
        #Main dataframe list

        self.data = [] #List of dataframes per day
               
        #Loop over folders with days 
        for day in daterange(self.date_ini_obj, self.date_end_obj+timedelta(days=1)):

            #Load data for this day
            day_str=day.strftime("%Y-%m-%d")
            self.data.append(DayData(day_str, self.data_dir))

    def build_analytics_data(self, mode, state):
        #Mode:
        # local/vel: based on velocities and windrose 
        # network/reg : based on regions
        # all : all modes

        #Basic diagnostics
        for day in self.data:
            day.calc_basic_day_diagnostics()

        if mode == "local" or mode == "vel" or mode == "all":
            for day in self.data:
                day.calc_vel_day_diagnostics()

        if mode == "network" or mode == "reg" or mode == "all":
            self.set_network_grid("SP")


    def set_global_domain(self):
        self.dom = Domain()
        self.dom.set_global_domain(self.data)
        
        
    #Build city network
    def set_network_grid(self, state):

        #Init network
        self.network = Network(state)
        
        #Create grid for network
        self.network.network_grid(self.dom)

        #Map the regions
        map = Map(self.dom)
        title = "Regions"+self.date_ini+"_"+self.date_end
        map.map_data(self.network.region_grid, title, self.data_dir)
        

        
