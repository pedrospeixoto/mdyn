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
from mdyn_extras import daterange

import matplotlib.pyplot as plt
from matplotlib import animation

class MobileDynamics:

    def __init__(self, data_dir=None, date_ini=None, date_end=None, load=False):
            
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
        self.dump_dir = 'dump/'
        
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
            day_data = DayData(day_str, self.data_dir, network, load=self.load)
            day_data.calc_basic_day_diagnostics() 
            
            #Get useful info from this day (velocities)
            #day_data.calc_vel_day_diagnostics()

            #To be defined - to set proper periods of day
            day_data.calc_time_day()

            #Update dataframe with network info
            network.add_reg_to_daydf(day_data)           

            day_data.tmat, day_data.tmat_norm = network.calc_transition_matrix(day_data)

            np.savetxt( day_data.local_dir+"trans_mat.csv", day_data.tmat)
            np.savetxt( day_data.local_dir+"trans_mat_norm.csv", day_data.tmat_norm)

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
        
    def simulate_daily(self, mode):
    
        #initial condition
        
        x = np.zeros([self.network.nreg_in])
        x[3]=1

        fig, ax = plt.subplots(figsize=(12,6))
        labels = list(self.network.regions.values())
        
        ax.plot( x, 'o', label='IC')

        #ax.set_ylim(0,450)
        ax.set_ylabel('Individuals')
        ax.set_title('Zombie Test')
        ax.set_xticks(np.arange(len(x)))
        ax.set_xticklabels(labels)
        ax.set_yscale('log')
        
        
        for i, day in enumerate(self.data):
            #print(day.tmat, day.tmat.shape, x.shape)
            x=np.matmul(day.tmat, x)
            print(x.sum())
            ax.plot( x, 'x', label=day.day)
            ax.legend()
            filename = self.dump_dir+"daily_simulation"+day.day+".jpg"
            plt.savefig(filename, dpi=300)
            del ax.lines[1]   
            
    def simulate_weekly(self, mode, dayoftheweek):

        #initial condition
        
        x = np.zeros([self.network.nreg_in])
        tmatweek = np.identity(self.network.nreg_in)
        tmat_list = [] #List of transiction matrices
        day_list = [] #List of dates used
        for i, day in enumerate(self.data):
            if day.day_weekday == dayoftheweek: #Monday=0
                day_list.append(day.day)
                tmat_list.append(tmatweek)
                print("Transition matrix for Monday to Monday", day.day)
                print(tmatweek)
                tmatweek = np.identity(self.network.nreg_in)

            #print(day.tmat, day.tmat.shape, x.shape)
            tmatweek=np.matmul(day.tmat, tmatweek)

        #Calculate the average matrix
        #basicmat=np.zeros([self.network.nregions,self.network.nregions])
        #for mat in tmat_list:
        #    basicmat = basicmat + mat
        if len(tmat_list) > 0:
            basicmat=sum(tmat_list)/len(tmat_list)
        else:
            basicmat=tmat_list
        print(basicmat)

        #       
        #Simulate 
        x[3]=1

        fig, ax = plt.subplots(figsize=(12,6))
        labels = list(self.network.regions.values())
        
        ax.plot( x, 'o', label='IC')

        #ax.set_ylim(0,450)
        ax.set_ylabel('Individuals')
        ax.set_title('Zombie Test')
        ax.set_xticks(np.arange(len(x)))
        ax.set_xticklabels(labels)
        ax.set_yscale('log')
        for i in range(20):
            #print(day.tmat, day.tmat.shape, x.shape)
            x=np.matmul(basicmat, x)
            print(x.sum())
            ax.plot( x, 'x', label=str(i))
            ax.legend()
            filename = self.dump_dir+"weekly_simulation"+str(i)+".jpg"
            plt.savefig(filename, dpi=300)
            del ax.lines[1]   
        
        

        
