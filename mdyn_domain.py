#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import numpy as np

from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.colors as colors
import matplotlib.cm as cm

class Domain:
    def __init__(self):
        #Bin padding (MANUAL)
        self.dlat = 0.2
        self.dlon = 0.2

    def set_domain(self, minlon, maxlon, minlat, maxlat):
        
        self.minlons = minlon
        self.maxlons = maxlon
        self.minlats = minlat
        self.maxlats = maxlat
        #print(minlon, maxlon, minlat, maxlat)

        self.nlon = int((self.maxlons - (self.minlons))/self.dlon)
        self.nlat = int((self.maxlats - (self.minlats))/self.dlat)

        self.lon_bins = np.linspace(self.minlons, self.maxlons, self.nlon+1, endpoint=True) 
        self.lat_bins = np.linspace(self.minlats, self.maxlats, self.nlat+1, endpoint=True) 
        #print(self.lon_bins)
        self.lon_bins_c = np.linspace(self.minlons-self.dlon/2, self.maxlons+self.dlon/2, self.nlon+2, endpoint=True) 
        self.lat_bins_c = np.linspace(self.minlats-self.dlat/2, self.maxlats+self.dlat/2, self.nlat+2, endpoint=True) 
        #print(self.lon_bins_c)
        self.lon_bins_2d, self.lat_bins_2d = np.meshgrid(self.lon_bins, self.lat_bins)
        self.lon_bins_2d_c, self.lat_bins_2d_c = np.meshgrid(self.lon_bins_c, self.lat_bins_c)


    def set_global_domain(self, data):

        #data needs to have days with precalculated domains

        self.minlons=100000.0
        self.maxlons=-1000000.0
        self.minlats=100000.0
        self.maxlats=-1000000.0
        for d in data:
            #Min/Max of domain for each day
            self.minlons=min(self.minlons, d.dom.minlons)
            self.minlats=min(self.minlats, d.dom.minlats)
            self.maxlons=max(self.maxlons, d.dom.maxlons)
            self.maxlats=max(self.maxlats, d.dom.maxlats)
            
        print("Domain: ")
        print( "  Lon:", self.minlons, self.maxlons)
        print( "  Lat:", self.minlats, self.maxlats)

        self.set_domain(self.minlons, self.maxlons, self.minlats, self.maxlats)

        print("  nLon:", self.nlon)
        print("  nLat:", self.nlat)


class Map:
    def __init__(self, dom): 

        #Save a copy of the domain
        self.dom=dom       

        #Init the figure
        fig, ax = plt.subplots()
        
        #Define map projection
        map = Basemap(projection='merc', resolution='l',
            llcrnrlon=dom.minlons-1, llcrnrlat=dom.minlats-1,
            urcrnrlon=dom.maxlons+1, urcrnrlat=dom.maxlats+1)
            #width=2E6, height=2E6, lat_0=lat0, lon_0=lon0,
        
        #Config map
        map.drawcoastlines()
        map.drawcountries()
        #map.fillcontinents(color = 'coral')
        map.drawmapboundary()
        map.drawstates()

        map.ax = ax
        
        # convert the bin mesh to map coordinates:
        self.x_bins, self.y_bins = map(dom.lon_bins_2d, dom.lat_bins_2d) # will be plotted using pcolormesh
        self.x_bins_c, self.y_bins_c = map(dom.lon_bins_2d_c, dom.lat_bins_2d_c) # will be plotted using pcolormesh

        #Save fig basic config
        self.map = map
        self.fig = fig


    def map_density_data(self, lng, lat, title, dir):
        
        #Calculate 2d histogram
        density, _, _ = np.histogram2d(lat, lng, [self.dom.lat_bins_c, self.dom.lon_bins_c])

        #Create color plot
        plt.pcolormesh(self.x_bins_c, self.y_bins_c, density, cmap="hot_r", norm=colors.LogNorm(), snap=True)

        #Add stuf to plot
        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)

        #Save density plot to folder "dir"
        filename = dir+"/density_"+title+".eps"
        plt.savefig(filename, dpi=300)


    def map_data(self, data, title, dir):

        #2d color plot of data
        plt.pcolormesh(self.x_bins_c, self.y_bins_c, data)  
            #cmap="hot_r", norm=colors.LogNorm(), snap=True)

        #Config
        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)
    
        #Save density plot to folder "dir"
        filename = dir+"/map_data_"+title+".eps"
        plt.savefig(filename, dpi=300)
