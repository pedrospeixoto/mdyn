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

import geopandas as gpd
import descartes
from shapely.geometry import Point, Polygon

from windrose import WindroseAxes

import tqdm 

from mdyn_daydata import DayData
from mdyn_network import Network
from mdyn_extras import *

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
            self.data.append(DayData(day_str, self.data_dir))

    def calc_diagnostics(self):
        for day in self.data:
            day.calc_day_diagnostics()

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

    #Build city network
    def set_network_grid(self, state, n):
        self.network=Network(state, n)
        self.region_grid=np.zeros((self.nlat+1, self.nlon+1))
        for i, lat in enumerate(self.lat_bins):
            for j, lon in enumerate(self.lon_bins):
                reg=self.network.get_closest_region(lat, lon)
                self.region_grid[i,j]=self.network.regions[reg][2]

        self.map_data(self.region_grid, "Regions")
        #plt.show()

        

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

    def map_data(self, data, title):

        #Map for data
        _, _ = self.map_base()

        plt.pcolormesh(self.x_bins_c, self.y_bins_c, data)  
            #cmap="hot_r", norm=colors.LogNorm(), snap=True)

        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)
    
    def test_map_shapes(self):
        #geo_df = gpd.read_file('../Maps/SP-MUN/35MUE250GC_SIR.shp')
        geo_df = gpd.read_file('maps/UFEBRASIL.shp')
        fig,ax =plt.subplots(figsize =(10,10))
        print(geo_df.head())
        geo_df.plot(column='NM_ESTADO',  cmap='Set3', ax=ax, edgecolor='grey')
        plt.show()
    
