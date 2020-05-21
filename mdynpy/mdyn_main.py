#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
from pathlib import Path


import numpy as np
import pandas as pd
import math
import time

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

import calendar

import imp

from mdynpy.mdyn_daydata import DayData
from mdynpy.mdyn_network import Network
from mdynpy.mdyn_extras import daterange
from mdynpy.mdyn_map import Map

import matplotlib.pyplot as plt
from matplotlib import animation

class MobileDynamics:

    def __init__(self, ipar): 
        data_dir = ipar.data_dir
        data_format = ipar.data_format
        date_ini = ipar.date_ini
        date_end = ipar.date_end
        dump_dir = ipar.dump_dir
        load = ipar.load_data
        

        print("")
        print("Mobile Dynamics Data Analysis")
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

    def build_model(self, network):

        #Loop over days
        self.data = [] #List of dataframes per day

        #Loop over folders with days 
        for day in daterange(self.date_ini_obj, self.date_end_obj+timedelta(days=1)):
            
            #Load data for this day
            day_str=day.strftime("%Y-%m-%d")

            day_data = DayData(day_str, self.data_dir, self.data_format, network, load=self.load)

            day_data.calc_basic_day_diagnostics() 
            
            #Get useful info from this day (velocities)
            #day_data.calc_vel_day_diagnostics()

            #Update dataframe with network info
            network.add_reg_to_daydf(day_data)           

            day_data.tmat, day_data.tmat_norm, day_data.reg0, day_data.reg1 = \
                network.calc_transition_matrix(day_data)

            name = "move_mat_"+network.domain+"_"+network.subdomains
            #title= "Network "+network.domain+" "+network.subdomains
            #map=Map(network)
            #map.map_network(day_data.tmat, day_data.reg0, title, 
            #    day_data.local_dir+name+"_network_map.jpg")

            np.savetxt( day_data.local_dir+name+".csv", day_data.tmat)
            np.savetxt( day_data.local_dir+name+"_norm.csv", day_data.tmat_norm)
            np.savetxt( day_data.local_dir+name+"_reg0.csv", day_data.reg0)
            np.savetxt( day_data.local_dir+name+"_reg1.csv", day_data.reg1)
            np.save( day_data.local_dir+name+"_reg_names.npy", network.regions)
            reg=list(network.regions.values())
            with open(day_data.local_dir+name+"_reg_names.txt", "w") as output:
                for r in reg:
                    output.write("%s\n" % r)
                            
            #To save ram memory keep only the transition matrix
            day_data.clean_data()

            #Store just useful data, not raw data
            self.data.append(day_data) 

    def read_data(self, load):
        #Main dataframe list

        self.data = [] #List of dataframes per day
               
        #Loop over folders with days 
        for day in daterange(self.date_ini_obj, self.date_end_obj+timedelta(days=1)):
            
            #Load data for this day
            day_str=day.strftime("%Y-%m-%d")
            #DayData(day_str, self.data_dir)
            self.data.append(DayData(day_str, self.data_dir, load))
        
    def collect_move_mat(self, network):

        #Loop over days
        self.movemats = [] #List of matrices per day
        self.movemats_norm = [] #List of matrices per day
        self.movemats_reg0 = [] #Regions t0 (depart from)
        self.movemats_reg1 = [] #Regions t1 (arrive at)
        self.movemats_reg_names = [] #Regions names

        self.days_all = [] #List of dates per day
        self.dates_dirs = [] #directory of day data
        
        name = "move_mat_"+network.domain+"_"+network.subdomains

        np_load_old = np.load
        np.load = lambda *a,**k: np_load_old(*a, allow_pickle=True, **k)

        #Loop over folders with days 
        for day in daterange(self.date_ini_obj, self.date_end_obj+timedelta(days=1)):
            self.days_all.append(day)
            self.dates_dirs.append(self.data_dir+"dt="+day.strftime("%Y-%m-%d")+"/")

            sday=day.strftime("%Y-%m-%d")
            if self.data_format == "ORC":
                local_dir = self.data_dir+"dt="+sday+"/"
            elif self.data_format == "Parquet":
                local_dir = self.data_dir+"date0="+sday+"/"
            
            try:
                self.movemats.append(np.genfromtxt(local_dir+name+'.csv'))
                self.movemats_norm.append(np.genfromtxt(local_dir+name+'_norm.csv'))
                self.movemats_reg0.append(np.genfromtxt(local_dir+name+'_reg0.csv').astype(int))
                self.movemats_reg1.append(np.genfromtxt(local_dir+name+'_reg1.csv').astype(int))
                
                #reg_names = np.load(local_dir+name+'_reg_names.npy')
                filename = local_dir+name+"_reg_names.txt"
                with open(filename) as f:
                    reg_names = f.read().splitlines()            
                self.movemats_reg_names.append(reg_names)
                print("Loaded matrix for :", day )
            except:
                print("Please run mdyn_build_model.py first to generate the movement matrices")
                print(" (run with the same parameter file!)")
                sys.exit(1)
