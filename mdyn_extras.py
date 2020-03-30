#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import getopt

import numpy as np
import pandas as pd
import math

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm

import geopy.distance

#Garbage collection
import gc

#import functionality
import imp

#General functions
#--------------------------------

def get_input(args):
    
    param_file = None
    run_opt = None
    print(args)
    #Get input parameters
    try:
        opts, args = getopt.getopt(args[1:],"hf:o:",["param_file=","run_opt="])
    except getopt.GetoptError:
        print(args+ '-f <param_file> -o <run_option>')
        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print(sys.argv[0]+ '-f <param_file> -o <run_option>')
            print("<run_option>=")
            print(" 0) Build model")
            print(" 1) Analyse movement")
            sys.exit()
        elif opt in ("-i", "-f", "--ifile"):
            param_file = arg
        elif opt in ("-o", "--run_opt"):
            run_opt = int(arg)

    #Get parameters for simulation
    ipar = getVarFromFile(param_file)

    return ipar, run_opt

def getVarFromFile(filename):
    f = open(filename)
    global ipar
    ipar = imp.load_source('ipar', '', f)
    f.close()
    return ipar 

def round_up(n, decimals=0): 
    multiplier = 10 ** decimals 
    return np.round(math.ceil(n * multiplier) / multiplier ,  decimals)
    
def round_down(n, decimals=0):
    multiplier = 10 ** decimals
    return np.round(math.floor(n * multiplier) / multiplier, decimals)

#Range over date variable
def daterange(start_date, end_date):
    for n in range(int ((end_date - start_date).days)):
        yield start_date + timedelta(n)


#Convert timestamp to datetime
def timestamp2datetime(ts):
    try:
        tmp=int(int(ts)/1000)
        return datetime.utcfromtimestamp(tmp)
    except:
        return np.nan

#Get picle files
def list_files_pkl(directory):
    return (f for f in os.listdir(directory) if f.endswith('.pkl'))

def distance(lon, lat, lon1,lat1):
    return np.array([geopy.distance.geodesic([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)

def distance_lon(lon, lat, lon1,lat1):
    dist = np.array([geopy.distance.geodesic([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)
    signdistlon = np.sign(lon1-lon)
    return signdistlon*dist

def distance_lat(lon, lat, lon1,lat1):
    dist = np.array([geopy.distance.geodesic([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)
    signdistlat = np.sign(lat1-lat)
    return signdistlat*dist

def del_df(df):
        
        del [df]
        gc.collect()
        df=pd.DataFrame()
        #df=df_tmp.copy()
        

def mem_usage(pandas_obj):
    if isinstance(pandas_obj,pd.DataFrame):
        usage_b = pandas_obj.memory_usage(deep=True).sum()
    else: # we assume if not a df it's a series
        usage_b = pandas_obj.memory_usage(deep=True)
    usage_mb = usage_b / 1024 ** 2 # convert bytes to megabytes
    return "{:03.2f} MB".format(usage_mb)
        
def matprint(mat, fmt="g"):
    col_maxes = [max([len(("{:"+fmt+"}").format(x)) for x in col]) for col in mat.T]
    for x in mat:
        for i, y in enumerate(x):
            print(("{:"+str(col_maxes[i])+fmt+"}").format(y), end="  ")
        print("")

#Ploting helpers
markers = [ 'o', 'v', '^', '<', '>', 's', 'p', '*', 'h', 'H', '+', 'x', 'D', '.', ',', 'o', 'v', '^', '<', '>', '1', '2', '3', '4', '8', 's', 'p', '*', 'h', 'H', '+', 'x', 'D']
thinmarkers = ['.', '1', '2', '3', '4', '8']
colors = ['blue', 'red', 'yellow', 'orange',  'limegreen', \
            'violet', 'purple', 'brown', 'cyan', 'olive', 'coral', 'lightgreen' ,'grey', \
               'blue', 'red', 'yellow', 'orange',  'limegreen' ]

def plot_matrix(mat, title, dir):

    filename=dir+title.replace("\n", "_")+".jpg"
    title = title.replace("_", " ")

    f, ax = plt.subplots(figsize=(6.2,5.6))
    #ax = f.add_axes([0.17, 0.02, 0.72, 0.79])
    #axcolor = f.add_axes([0.90, 0.02, 0.03, 0.79])
    im = ax.matshow(mat, cmap="hot_r", norm=LogNorm())
    #plt.imshow(mat, cmap="hot_r", norm=LogNorm(vmin=0.00000001, vmax=1))
    cbar = plt.colorbar(im, orientation='horizontal', shrink=0.5, aspect=25, fraction=0.1, pad=0.01, \
            spacing='proportional')
    #cbar.set_label("Probability",size=12)
    plt.title(title,  y=1.08)
    plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
    #plt.show()
    plt.savefig(filename)
