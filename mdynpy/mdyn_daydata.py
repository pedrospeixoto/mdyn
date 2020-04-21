#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
import pyarrow.parquet as pq
import numpy as np
import pandas as pd
import math

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.colors as colors
import matplotlib.cm as cm
import matplotlib.dates as mdates

import geopy.distance
from windrose import WindroseAxes

import tqdm as tqdm

from mdynpy.mdyn_map import Map
from mdynpy.mdyn_extras import distance, distance_lat, distance_lon, daterange, timestamp2datetime, list_files_pkl, del_df, mem_usage

#Garbage collection
import gc


#Main class for dataframe for a single day
class DayData:
    
    #Class for data per day
    def __init__(self, day, data_dir, data_format, network, load = False):
        self.load = load
        self.data_format = data_format
        self.read_day_data(day, data_dir, data_format)
        self.net = network
        
        
        #if not load:
        #    self.clean_data()
                        
    def clean_data(self):
        
        mem1=mem_usage(self.df)
        del [self.df]
        gc.collect()
        self.df=pd.DataFrame()
        print("Cleaning original data ", mem1, mem_usage(self.df))

    def read_day_data(self, day, data_dir, data_format):
        # Input:
        # day is a date in format Y-m-d
        # datadir is the main directory that contains the data
        #Format depends on the type of data to be read

        self.day = day
        if data_format == "ORC":
            local_dir = data_dir+"dt="+day+"/"
        elif data_format == "Parquet":
            local_dir = data_dir+"date0="+day+"/"
        else:
            print("Please hard code the data format style, don't know this one.", data_format)
            sys.exit()

        self.local_dir = local_dir
        self.day_obj = datetime.strptime(self.day, '%Y-%m-%d')
        self.day_weekday = self.day_obj.weekday() # Monday is 0 and Sunday is 6.
        self.month = self.day_obj.strftime("%Y-%m")

        print()
        print("Loading data for day:", day, " from ", local_dir )

        if data_format == "ORC":
            self.col_labels=[ 'time0', 'lng0', 'lat0', 
                'time1', 'lng1','lat1', 
                'time2', 'lng2','lat2']
        elif data_format == "Parquet":
            self.col_labels=[ 'lat0', 'lng0', 'time0',
                'lat1', 'lng1', 'time1','state1', 'date1']
        
        df_local = pd.DataFrame(columns=self.col_labels) 
        
        if not os.path.exists(local_dir):
            print( " Could not reach directory, stopping here.", local_dir)
            sys.exit(0)
            #self.df = df_local
            #return self #empty dataframe
        
        dirContents = os.listdir(local_dir)
        if len(dirContents) == 0:
            print('Folder is Empty!', local_dir)
            sys.exit(0)
        
        loaded = False
        pklfiles = list_files_pkl(local_dir)
        #Do we have a pickle file and want to load it?
        if self.load:
            for f in pklfiles:
                if "data.pkl" in f:
                    print("Found a pickle file with raw data: ", f, ". Loading data.")
                    df_local = pd.read_pickle(local_dir+f)
                    loaded = True

        if not loaded:
            for filename in os.listdir(local_dir):

                #Get data and convert to df pandas
                try:
                    print(" Reading: ",filename)
                    if data_format == "ORC":
                        orc_file = orc.ORCFile(local_dir+filename)
                        data = orc_file.read()
                    elif data_format == "Parquet":
                        pq_file = pq.ParquetFile(local_dir+filename)
                        #print(pq_file.schema)
                        data = pq_file.read()
                except:
                    print("   File not in orc/parquet format: ", filename, ". Ignoring.")
                    continue

                #Organize data and save in main data_frame
                df_tmp = pd.DataFrame(data.to_pandas())
                df_tmp = self.org_day_data(df_tmp)
                df_local = pd.concat([df_local, df_tmp], ignore_index=True )
                
            print(" Saving dataframe for future use as data.pkl")
            #df_local.to_csv (local_dir+"data.csv", header=True) #Don't forget to add '.csv' at the end of the path
            df_local.to_pickle (local_dir+"data.pkl") 
        
        self.df = df_local
        self.n = len(df_local)
        
        return self

    def org_day_data(self, dflocal):
        #Organize data into nice dataframe with standard column names

        n=len(dflocal)
        
        labels=list(self.col_labels)
        dfout = pd.DataFrame(index=np.arange(0, n), columns=labels, dtype='float')
        
        if self.data_format == "ORC":
            #Data has always 3 events
            for idx, d0, d1, d2 in dflocal.itertuples():
                try:
                    dfout.at[idx, 'time0']=timestamp2datetime(d0['timestamp'])
                    dfout.at[idx, 'lng0']=d0['location']['lng']
                    dfout.at[idx, 'lat0']=d0['location']['lat']
                except:
                    pass

                try:
                    dfout.at[idx, 'time1']=timestamp2datetime(d1['timestamp'])
                    dfout.at[idx, 'lng1']=d1['location']['lng']
                    dfout.at[idx, 'lat1']=d1['location']['lat']
                except:
                    pass
                
                try:
                    dfout.at[idx, 'time2']=timestamp2datetime(d2['timestamp'])
                    dfout.at[idx, 'lng2']=d2['location']['lng']
                    dfout.at[idx, 'lat2']=d2['location']['lat']
                except:
                    pass
        
        elif self.data_format == "Parquet":
            #This kind of data has only 2 events, which were pre-processed to columns
            dfout = dflocal
            dfout = dfout.rename(columns={"timestamp0": "time0", "timestamp1": "time1"})
            #dfout.drop(columns=['timestamp0', 'timestamp1'])
        return dfout
    
    def calc_basic_day_diagnostics(self):

            #Create new variables
            #Add to dataframe
            print()
            print("Calculating basic diagnostics for day "+self.day)
        
            #Time step
            time0=pd.to_datetime(self.df['time0'])
            time1=pd.to_datetime(self.df['time1'])
            dt=(time1-time0).astype('timedelta64[h]') 
            #print(time0, time1)
            self.df['dt1']=dt
            #print(dt)
            #print(self.df)
            #Filer dt larger than 24h
            nold = len(self.df)
            print("WARNING: Filtering dt < 24hours")
            timefilter1 = self.df['dt1'] <= 24 
            timefilter2 = self.df['dt1'] > 0
            #timefilter = self.df['dt1'] == 0.0
            self.df = self.df[timefilter1 & timefilter2] 
            #self.df = self.df[timefilter] 
            self.n = len(self.df)
            print("WARNING: Filtered data: previous: ", nold, " now: ", self.n, " lost: ", nold-self.n)
            
            load = self.load
            if False:
                #Distances
                print("Calculating distances...")
                self.df['dist1']=distance(
                    self.df['lng0'].values, self.df['lat0'].values, 
                    self.df['lng1'].values, self.df['lat1'].values)

            filename = self.local_dir+"day_"+self.day+"_proc_data.csv"
            if True: #not os.path.exists(filename):
                #Distances
                print("Calculating distances...")
                self.df['dist1']=distance(
                    self.df['lng0'].values, self.df['lat0'].values, 
                    self.df['lng1'].values, self.df['lat1'].values)

                print(" Saving full dataframe for future use as proc_data.csv")
                self.df.to_csv(filename, header=True) #Don't forget to add '.csv' at the end of the path
            else:
                print("Loading distances...", end="")
                self.df['dist1'] = pd.read_csv(filename, usecols = ['dist1'])
                print("..done.")
            #print(self.df)
            #distfilter = self.df['dist1'] == 0.0
            #self.df = self.df[timefilter1 & timefilter2] 
            #self.df = self.df[distfilter] 

            #Data density - takes time
            for i in tqdm.tqdm(range(3)):
                s=str(i)
                title = "density_"+self.day+" event "+str(i)
                filename = self.local_dir+title+".jpg"
                if not os.path.exists(filename):
                    try: 
                        map = Map(self.net)
                        map.map_density_data(self.df['lng'+s].values, self.df['lat'+s].values, \
                            title, filename)
                    except:
                        pass

            #Data density - join event 0 and 1
            title = "density_"+self.day+" event0+1"
            filename = self.local_dir+title+".jpg"
            if not os.path.exists(filename):
                    print("Ploting density")
                    map = Map(self.net)
                    lons = np.concatenate((self.df['lng0'].values, self.df['lng1'].values))
                    lats = np.concatenate((self.df['lat0'].values, self.df['lat1'].values))
                    map.map_density_data(lons, lats, \
                        title, filename)

            #Statistics
            print(self.df.describe())
            filename = self.local_dir+"day_"+self.day+"_basic_stats.csv"
            if not os.path.exists(filename) and load:
                self.df.describe().to_csv(filename, header=True) #Don't forget to add '.csv' at the end of the path
            
            #Histograms
            filename = self.local_dir+"day_"+self.day+"_basic_stats.jpg"
            if not os.path.exists(filename) and load:
                self.df.hist(bins=8, align='mid')
                fig = plt.gcf()
                fig.set_size_inches(18.5, 18.5)
                plt.savefig(filename, dpi=300)

            filename = self.local_dir+"day_"+self.day+"_time_hist.jpg"
            if not os.path.exists(filename):
                t0_hours = pd.DatetimeIndex(time0).hour
                t1_hours = pd.DatetimeIndex(time1).hour
                fig, ax = plt.subplots(2,1)
                ax[0].hist(t0_hours, bins=24, color='lightblue')
                ax[0].set_title("Day "+self.day )
                ax[0].set_ylabel("Num at T0")
                ax[0].ticklabel_format(axis="y", style="sci", scilimits=(0,0))

                ax[1].hist(t1_hours, bins=24, color='lightblue')
                ax[1].set_ylabel("Num at T1")
                ax[1].set_xlabel("Hour of day")
                ax[1].ticklabel_format(axis="y", style="sci", scilimits=(0,0))

                plt.savefig(filename, dpi=300)
            #sys.exit()

    def calc_vel_day_diagnostics(self):

            #Create new variables
            #Add to dataframe
            print()
            print("Calculating velocity diagnostics for day "+self.day)
        
            self.df['distx1']=distance_lon(
                self.df['lng0'].values, self.df['lat0'].values, 
                self.df['lng1'].values, self.df['lat0'].values)
            self.df['disty1']=distance_lat(
                self.df['lng0'].values, self.df['lat0'].values, 
                self.df['lng0'].values, self.df['lat1'].values)

            #Velocities
            vel1=np.zeros(self.n)
            velx1=np.zeros(self.n)
            vely1=np.zeros(self.n)
            #print("Calculating velocities")
            for i, dt in enumerate(tqdm.tqdm(self.df['dt1'].values)):
                if dt > 0.0001:
                    vel1[i]=np.true_divide(self.df['dist1'].values[i],float(dt))
                    velx1[i]=np.true_divide(self.df['distx1'].values[i],float(dt))
                    vely1[i]=np.true_divide(self.df['disty1'].values[i],float(dt))

            self.df['vel1']=vel1
            self.df['velx1']=velx1
            self.df['vely1']=vely1

            #Angle from North (positive clock and negative counter-clock)
            self.df['angle']=[math.atan2(self.df['velx1'].values[i],self.df['vely1'].values[i]) for i in range(len(self.df)) ]
            
            #Convert to angle with north with pos values (north is zero, clockwise angle grows)
            self.df['angle']=(2*math.pi+self.df['angle'].values)%(2.0*math.pi)

            #Angle Quadrant - 0 is north, 2 east - clockwith
            dtheta=2*math.pi/8
            self.df['quad']=np.trunc(((self.df['angle'].values+dtheta/100.0)/dtheta)%8).astype(int)

            #Directions
            self.df['angle_deg']=self.df['angle']*360.0/(2.0*math.pi)
            
            #print(self.df.info())
            print(self.df.describe())
            self.df.describe().to_csv(self.local_dir+"/day_vel_stats.csv", header=True) #Don't forget to add '.csv' at the end of the path
            self.df.to_csv(self.local_dir+"/day_vel_data.csv", header=True) #Don't forget to add '.csv' at the end of the path
            self.df.hist(bins=8, align='mid')
            fig = plt.gcf()
            fig.set_size_inches(18.5, 18.5)
            plt.savefig(self.local_dir+"/day_vel_data.eps", dpi=300)
            
            #windrose
            ax = WindroseAxes.from_ax()
            ax.bar(self.df['angle_deg'].values, self.df['dist1'].values, normed=True, opening=0.8, edgecolor='white', bins=[0.0, 10.0, 20.0, 30.0, 40.0, 60.0, 100.0, 300.0, 1000.0])
            ax.set_legend(title="Distance (km)")
            #plt.show()
            plt.savefig(self.local_dir+"/day_global_windrose.eps", dpi=300)

            #Calculate distribution per grid cell in polar coordinates --> NEEDS DEBUGGING
            if False: #Very costy
                #print(self.lat_bins)
                #print(self.lon_bins)

                dyn=[[None]*(self.net.nlon+2) for _ in range(self.net.nlat+2)] #np.empty([self.nlat+1,self.nlon+1])
                self.dist_bins = [0, 15, 30, 50, 100, 300, 500, 1000, np.inf]
                self.vel_bins = [0, 1, 10, 20, 30, 50, 100, 300, 500, 1000, np.inf]
                self.angle_bins = np.arange(8) #quadrants
                print("Calculating windroses:")
            
                for j, lon in enumerate(tqdm.tqdm(self.net.lon_bins_ext)):
                    for i, lat in enumerate(self.net.lat_bins_ext):
                        #get distribution for this lat lon
                        #print(lon, lat)
                        filter1=self.df['lng0']>lon
                        filter2=self.df['lng0']<(lon + self.net.dlon)
                        filter3=self.df['lat0']>lat
                        filter4=self.df['lat0']<(lat+self.net.dlat)
                        local_df = self.df[filter1 & filter2 & filter3 & filter4] 

                        freq=np.zeros( (int(len(self.angle_bins)), 
                                        #int(len(self.dist_bins)), 
                                        int(len(self.vel_bins))), 
                                        dtype=int)

                        n=len(local_df)
                        if n>0:
                            print(local_df)
                            for l, quad in enumerate(self.angle_bins):
                                local_df_quad=local_df[local_df['quad']==quad]
                                if(len(local_df_quad)):
                                    #print(l, quad, local_df_quad)
                                    for k, vel in enumerate(self.vel_bins[:-1]):
                                        local_df_vel=local_df_quad[local_df_quad["vel1"]>=vel]
                                        local_df_vel=local_df_vel[local_df_quad["vel1"]<self.vel_bins[k+1]]
                                        freq[l,k]=len(local_df_vel)
                                        #print(k, vel, freq[l,k])
                                #freq=len(local_df_quad[local_df_dist['quad']==quad])/float(n)
                                #local_df_dist=local_df[local_df["dist1"]>=dist]
                                #    local_df_dist=local_df_dist[local_df_dist["dist1"]<self.dist_bins[k+1]]
                                #    for l, quad in enumerate(self.angle_bins):
                                #        freq=len(local_df_dist[local_df_dist['quad']==quad])/float(n)

                            dyn[i][j]=freq
                            print(i, j, lon, lat)
                            print( dyn[i][j])
                #print(dyn)
    
            