#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
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

import geopy.distance
from windrose import WindroseAxes

import tqdm 

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
            print("1) A folder name containing the dataset, with slash")
            print("2) Date of begining of analysis in format: 2018-04-01")
            print("3) The final date - if equal, analyse a single day")
            sys.exit(1)
            
        data_dir = argv[1]
        date_ini = argv[2]

        if len(sys.argv) <= 3 :    
            date_end = argv[2]
        else:
            date_end = argv[3]

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
            self.data.append(self.DayData(day_str, self.data_dir))


    #Main class for dataframe for a single day
    class DayData:
        
        #Class for data per day
        def __init__(self, day, data_dir):
            self.read_day_data(day, data_dir)
            self.set_day_domain()
            self.calc_day_diagnostics()

        def read_day_data(self, day, data_dir):
            # Input:
            # day is a date in format Y-m-d
            # datadir is the main directory that contains the data

            self.day = day

            local_dir = data_dir+"dt="+day+"/"
            self.local_dir = local_dir
            print()
            print("Loading data for day:", day, " from ", local_dir )

            self.col_labels=[ 'time0', 'lng0', 'lat0', 
                'time1', 'lng1','lat1', 
                'time2', 'lng2','lat2']

            df_local = pd.DataFrame(columns=self.col_labels) 
            
            if not os.path.exists(local_dir):
                print( " Could not reach directory, ignoring this date.")
                self.df = df_local
                return self #empty dataframe
            
            loaded = False
            #print(os.listdir(local_dir))
            for filename in os.listdir(local_dir):
                #Check if data already saved in folder nicely formated
                if filename[-4:] == ".pkl":
                    print("Found a pickle file: ", filename, ". Loading data.")
                    df_local = pd.read_pickle(local_dir+filename)
                    #print(df_local)
                    loaded = True
                    break

                #Get data and convert to df pandas
                try:
                    print(" Reading: ",filename)
                    orc_file = orc.ORCFile(local_dir+filename)
                    data = orc_file.read()
                except:
                    print(" File not in orc format: ", filename, ". Ignoring.")
                    continue

                #Organize data and same in main data_frame
                df_tmp = pd.DataFrame(data.to_pandas())
                df_tmp = self.org_day_data(df_tmp)
                df_local=pd.concat([df_local, df_tmp], ignore_index=True )

            if not loaded: #save organized data
                print(" Saving dataframe for future use as data.csv and data.pkl")
                df_local.to_csv (local_dir+"data.csv", header=True) #Don't forget to add '.csv' at the end of the path
                df_local.to_pickle (local_dir+"data.pkl") 

            self.df = df_local
            self.n = len(df_local)
            return self

        def org_day_data(self, dflocal):
            #Organize data into nice dataframe

            n=len(dflocal)
            
            labels=list(self.col_labels)
            dfout = pd.DataFrame(index=np.arange(0, n), columns=labels, dtype='float')
            
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

            return dfout
        
        def set_day_domain(self):
            
            #Domain for each day
            self.minlons=min(np.amin(self.df['lng0'].values), 
                np.amin(self.df['lng1'].values),
                np.amin(self.df['lng2'].values))
            self.maxlons=max(np.amax(self.df['lng0'].values), 
                np.amax(self.df['lng1'].values),
                np.amax(self.df['lng2'].values))
            self.minlats=min(np.amin(self.df['lat0'].values), 
                np.amin(self.df['lat1'].values),
                np.amin(self.df['lat2'].values))
            self.maxlats=max(np.amax(self.df['lat0'].values), 
                np.amax(self.df['lat1'].values),
                np.amax(self.df['lat2'].values))
            #print("Domain: ")
            #print( "  Lon:", self.minlons, self.maxlons)
            #print( "  Lat:", self.minlats, self.maxlats)

            #Bin padding (MANUAL)
            self.dlat = 0.2
            self.dlon = 0.2
            self.nlon = int((self.maxlons - (self.minlons))/self.dlon)
            self.nlat = int((self.maxlats - (self.minlats))/self.dlat)
            #print("   nLon:", self.nlon)
            #print("   nLat:", self.nlat)

            self.lon_bins = np.linspace(self.minlons, self.maxlons, self.nlon+1, endpoint=True) 
            self.lat_bins = np.linspace(self.minlats, self.maxlats, self.nlat+1, endpoint=True) 
            #print(self.lon_bins)
            self.lon_bins_c = np.linspace(self.minlons-self.dlon/2, self.maxlons+self.dlon/2, self.nlon+2, endpoint=True) 
            self.lat_bins_c = np.linspace(self.minlats-self.dlat/2, self.maxlats+self.dlat/2, self.nlat+2, endpoint=True) 
            #print(self.lon_bins_c)
            self.lon_bins_2d, self.lat_bins_2d = np.meshgrid(self.lon_bins, self.lat_bins)
            self.lon_bins_2d_c, self.lat_bins_2d_c = np.meshgrid(self.lon_bins_c, self.lat_bins_c)
        
        def calc_day_diagnostics(self):

            #Create new variables
            #Add to dataframe
            print()
            print("Calculating diagnostics for day "+self.day)
        
            #Time step
            dt = self.df['time1'].values-self.df['time0'].values
            dth = np.array([i.total_seconds()/3600.00 for i in dt]).astype(int)
            self.df['dt1']=dth

            #Distances
            self.df['dist1']=distance(
                self.df['lng0'].values, self.df['lat0'].values, 
                self.df['lng1'].values, self.df['lat1'].values)
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
            print("Calculating velocities")
            for i, dt in enumerate(tqdm.tqdm(self.df['dt1'].values)):
                if dt > 0.0001:
                    vel1[i]=np.true_divide(self.df['dist1'].values[i],float(dt))
                    velx1[i]=np.true_divide(self.df['distx1'].values[i],float(dt))
                    vely1[i]=np.true_divide(self.df['disty1'].values[i],float(dt))

                #self.df['vel1'][self.df['vel1'] == np.inf] = 0
                #self.df['vel1'] = np.nan_to_num(self.df['vel1'])
                            
                #self.df['velx1']=np.true_divide(self.df['distx1'].values,self.df['dt1'].values)
                #self.df['velx1'][self.df['velx1'] == np.inf] = 0
                #self.df['velx1'] = np.nan_to_num(self.df['vel1'])
            
                #self.df['vely1']=np.true_divide(self.df['disty1'].values,self.df['dt1'].values)
                #self.df['velx1'][self.df['velx1'] == np.inf] = 0
                #self.df['velx1'] = np.nan_to_num(self.df['velx1'])

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
            self.df.describe().to_csv(self.local_dir+"/day_stats.csv", header=True) #Don't forget to add '.csv' at the end of the path
            self.df.to_csv(self.local_dir+"/day_diagnostics.csv", header=True) #Don't forget to add '.csv' at the end of the path
            self.df.hist(bins=8, align='mid')
            fig = plt.gcf()
            fig.set_size_inches(18.5, 18.5)
            plt.savefig(self.local_dir+"/day_diagnostics.eps", dpi=300)
            
            #windorose
            ax = WindroseAxes.from_ax()
            ax.bar(self.df['angle_deg'].values, self.df['dist1'].values, normed=True, opening=0.8, edgecolor='white', bins=[0.0, 10.0, 20.0, 30.0, 40.0, 60.0, 100.0, 300.0, 1000.0])
            ax.set_legend(title="Distance (km)")
            #plt.show()
            plt.savefig(self.local_dir+"/day_move_directions_distributions.eps", dpi=300)

            #Calculate distribution per grid cell in polar coordinates
            #print(self.lat_bins)
            #print(self.lon_bins)

            dyn=[[None]*(self.nlon+2) for _ in range(self.nlat+2)] #np.empty([self.nlat+1,self.nlon+1])
            self.dist_bins = [0, 15, 30, 50, 100, 300, 500, 1000, np.inf]
            self.vel_bins = [0, 1, 10, 20, 30, 50, 100, 300, 500, 1000, np.inf]
            self.angle_bins = np.arange(8) #quadrants
            print("Calculating windroses:")
            if False:
                for j, lon in enumerate(tqdm.tqdm(self.lon_bins_c)):
                    for i, lat in enumerate(self.lat_bins_c):
                        #get distribution for this lat lon
                        #print(lon, lat)
                        filter1=self.df['lng0']>lon
                        filter2=self.df['lng0']<(lon + self.dlon)
                        filter3=self.df['lat0']>lat
                        filter4=self.df['lat0']<(lat+self.dlat)
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

    def set_global_domain(self):
        self.minlons=100000.0
        self.maxlons=-1000000.0
        self.minlats=100000.0
        self.maxlats=-1000000.0

        for d in self.data:
            #Min/Max of domain for each day
            self.minlons=min(self.minlons, d.minlons)
            self.minlats=min(self.minlats, d.minlats)
            self.maxlons=max(self.maxlons, d.maxlons)
            self.maxlats=max(self.maxlats, d.maxlats)
            
        print("Domain: ")
        print( "  Lon:", self.minlons, self.maxlons)
        print( "  Lat:", self.minlats, self.maxlats)

        #Bin padding (MANUAL)
        dlat = 0.2
        dlon = 0.2
        self.nlon = int((self.maxlons - (self.minlons))/dlon)
        self.nlat = int((self.maxlats - (self.minlats))/dlat)
        print("   nLon:", self.nlon)
        print("   nLat:", self.nlat)

        self.lon_bins = np.linspace(self.minlons, self.maxlons, self.nlon+1, endpoint=True) 
        self.lat_bins = np.linspace(self.minlats, self.maxlats, self.nlat+1, endpoint=True) 
        #print(self.lon_bins)
        self.lon_bins_c = np.linspace(self.minlons-dlon/2, self.maxlons+dlon/2, self.nlon+2, endpoint=True) 
        self.lat_bins_c = np.linspace(self.minlats-dlat/2, self.maxlats+dlat/2, self.nlat+2, endpoint=True) 
        #print(self.lon_bins_c)
        self.lon_bins_2d, self.lat_bins_2d = np.meshgrid(self.lon_bins, self.lat_bins)
        self.lon_bins_2d_c, self.lat_bins_2d_c = np.meshgrid(self.lon_bins_c, self.lat_bins_c)


    #Create map
    def map_base(self):
        
        fig, ax = plt.subplots()
        
        map = Basemap(projection='merc', resolution='l',
            llcrnrlon=self.minlons-1, llcrnrlat=self.minlats-1,
            urcrnrlon=self.maxlons+1, urcrnrlat=self.maxlats+1)
            #width=2E6, height=2E6, lat_0=lat0, lon_0=lon0,
        
        map.drawcoastlines()
        map.drawcountries()
        #map.fillcontinents(color = 'coral')
        map.drawmapboundary()
        
        map.drawstates()

        map.ax = ax
        
        # convert the bin mesh to map coordinates:
        self.x_bins, self.y_bins = map(self.lon_bins_2d, self.lat_bins_2d) # will be plotted using pcolormesh
        self.x_bins_c, self.y_bins_c = map(self.lon_bins_2d_c, self.lat_bins_2d_c) # will be plotted using pcolormesh

        self.map=map
        self.fig=fig

        return fig, map

    def map_density_data(self, lng, lat, title):

        #Map for data
        _, _ = self.map_base()

        density, _, _ = np.histogram2d(lat, lng, [self.lat_bins_c, self.lon_bins_c])
        plt.pcolormesh(self.x_bins_c, self.y_bins_c, density, 
            cmap="hot_r", norm=colors.LogNorm(), snap=True)

        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)

    
    

#General functions
#--------------------------------

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
    

def distance(lon, lat, lon1,lat1):
    return np.array([geopy.distance.vincenty([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)

def distance_lon(lon, lat, lon1,lat1):
    dist = np.array([geopy.distance.vincenty([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)
    signdistlon = np.sign(lon1-lon)
    return signdistlon*dist

def distance_lat(lon, lat, lon1,lat1):
    dist = np.array([geopy.distance.vincenty([lon[i], lat[i]], [lon1[i], lat1[i]]).km 
                for i in range(len(lon)) ]).astype(float)
    signdistlat = np.sign(lat1-lat)
    return signdistlat*dist
