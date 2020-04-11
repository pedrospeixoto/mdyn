#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import getopt

import numpy as np
import pandas as pd
import math
import pyarrow.orc as orc
import pyarrow.parquet as pq

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

#General input/output functions
#--------------------------------


weekdays = 'Mon Tue Wed Thu Fri Sat Sun'.split()

def risk_time(A, c):
    n, m = A.shape
    risktime = np.full(n, -1)
    
    for i in range(n):
        for j in range(m):
            if A[i, j] > c:
                risktime[i]=j
                break

    return risktime

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

    if len(opts) < 1:
         print(sys.argv[0]+ ' -f <param_file> -o <run_option>')
         print("<run_option>=")
         print(" 0) Build model")
         print(" 1) Analyse movement")
         print(" 20) Movement Simulation ")
         print(" 30) Isolation Index")
         
         sys.exit()
         
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
    ipar.param_file=param_file
    base_name=os.path.basename(param_file)
    dump_dir="dump/"+os.path.splitext(base_name)[0]+"/"
    
    ipar.dump_dir = dump_dir

    return ipar, run_opt

def getVarFromFile(filename):
    f = open(filename)
    global ipar
    ipar = imp.load_source('ipar', '', f)
    f.close()
    return ipar 

def read_orc2df(local_dir, name_base, load):

    #Ensure we have the "/"
    if local_dir[-1]!="/":
        local_dir = local_dir+"/"

    print("Loading data from ", local_dir )

    if not os.path.exists(local_dir):
        print( " Could not reach directory, stopping here.")
        sys.exit(0)
    
    loaded = False
    pklfiles = list_files_pkl(local_dir)
    #Do we have a pickle file and want to load it?
    if load:
        for f in pklfiles:
            print("Found a pickle file: ", f, ". ", end="")
            if name_base in f:
                df_local = pd.read_pickle(local_dir+f)
                print("Loading data.")
                loaded = True
            else:
                print("But this pickle file does not match base name")

    if not loaded:
        for i, filename in enumerate(os.listdir(local_dir)):

            #Get data and convert to df pandas
            try:
                print(" Reading: ",filename)
                orc_file = orc.ORCFile(local_dir+filename)
                data = orc_file.read()
                df_tmp = pd.DataFrame(data.to_pandas())
            except:
                print(" File not in orc format: ", filename, ". Ignoring.")
                continue
            
            #Save in main data_frame
            if i == 0:
                df_local = pd.DataFrame(data.to_pandas())
            else:
                df_tmp = pd.DataFrame(data.to_pandas())
            
            df_local = pd.concat([df_local, df_tmp], ignore_index=True )

        print(" Saving dataframe for future use as xxx_data.csv and xxx_data.pkl")
        df_local.to_csv (local_dir+name_base+"data.csv", header=True) #Don't forget to add '.csv' at the end of the path
        df_local.to_pickle (local_dir+name_base+"data.pkl") 
    
    return df_local

def read_pq2df(local_dir, name_base, load):

    #Ensure we have the "/"
    if local_dir[-1]!="/":
        local_dir = local_dir+"/"

    print("Loading data from ", local_dir )

    if not os.path.exists(local_dir):
        print( " Could not reach directory, stopping here.")
        sys.exit(0)
    
    loaded = False
    pklfiles = list_files_pkl(local_dir)
    #Do we have a pickle file and want to load it?
    if load:
        for f in pklfiles:
            print("Found a pickle file: ", f, ". ", end="")
            if name_base in f:
                if "proc." not in f: 
                    df_local = pd.read_pickle(local_dir+f)
                    print("Loading data.")
                    loaded = True
                else:
                    print("But this pickle file is a processed data file.")
            else:
                print("But this pickle file does not match base name.")

    if not loaded:
        for i, filename in enumerate(os.listdir(local_dir)):

            #Get data and convert to df pandas
            try:
                print(" Reading: ",filename)
                pq_file = pq.ParquetFile(local_dir+filename)
                print(pq_file.metadata)
                data = pq_file.read()
                #print(data)
                df_tmp = pd.DataFrame(data.to_pandas())
                print(list(df_tmp))
            except:
                print(" File not in parquet format: ", filename, ". Ignoring.")
                continue
            
            #Save in main data_frame
            if i == 0:
                df_local = pd.DataFrame(data.to_pandas())
            else:
                df_tmp = pd.DataFrame(data.to_pandas())
            
            try:
                df_local = pd.concat([df_local, df_tmp], ignore_index=True )
            except:
                df_local = df_tmp

        print(" Saving dataframe for future use as xxx_data.csv and xxx_data.pkl")
        df_local.to_csv (local_dir+name_base+"_data.csv", header=True) #Don't forget to add '.csv' at the end of the path
        df_local.to_pickle (local_dir+name_base+"_data.pkl") 
    
    return df_local

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


#Get picle files
def list_files_pkl(directory):
    return (f for f in os.listdir(directory) if f.endswith('.pkl'))


#General mathy functions
#--------------------------------

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

def distance(lon1, lat1, lon2,lat2):
    dlon = np.radians(lon2) - np.radians(lon1)
    dlat = np.radians(lat2) - np.radians(lat1)
    R = 6378.1 #km 
    a = np.square(np.sin(0.5*dlat)) + \
        np.multiply(np.multiply(np.cos(lat1),  np.cos(lat2)),  np.square(np.sin(0.5*dlon )))
    b = 2.0 * np.arctan2(np.sqrt(a), np.sqrt(1 - a))

    distance = R * b
    return distance
    #Slow but more precise
    #return np.array([geopy.distance.geodesic([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
    #            for i in range(len(lon)) ]).astype(float)

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


#
# General post processing functions
#--------------------------------------

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

def plot_matrix(mat, title, filename):

    if filename[-3:] != "jpg":
        filename=filename+".jpg"

    title = title.replace("_", " ")
    #print("  Plotting : ", filename )
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
