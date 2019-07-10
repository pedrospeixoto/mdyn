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

        #These are the datapoints
        self.lon_bins_c = np.linspace(self.minlons, self.maxlons, self.nlon+1, endpoint=True) 
        self.lat_bins_c = np.linspace(self.minlats, self.maxlats, self.nlat+1, endpoint=True) 

        #These are bins for datagrid (larger than datagrid) - Extended grid
        self.lon_bins_ext = np.linspace(self.minlons-self.dlon/2, self.maxlons+self.dlon/2, self.nlon+2, endpoint=True) 
        self.lat_bins_ext = np.linspace(self.minlats-self.dlat/2, self.maxlats+self.dlat/2, self.nlat+2, endpoint=True) 

        #2d grids
        self.lon_bins_c_2d, self.lat_bins_c_2d = np.meshgrid(self.lon_bins_c, self.lat_bins_c)
        self.lon_bins_ext_2d, self.lat_bins_ext_2d = np.meshgrid(self.lon_bins_ext, self.lat_bins_ext)


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
        map = Basemap(projection='merc', resolution='h',
            llcrnrlon=dom.minlons-1, llcrnrlat=dom.minlats-1,
            urcrnrlon=dom.maxlons+1, urcrnrlat=dom.maxlats+1)
            #width=2E6, height=2E6, lat_0=lat0, lon_0=lon0,
        #map = Basemap(width=12000000,height=9000000,
        #    rsphere=(6378137.00,6356752.3142),\
        #    resolution='l',area_thresh=1000.,projection='lcc',\
        #    lat_1=dom.minlats-1,lat_2=dom.maxlats+1,\
        #    lat_0=0.5*(dom.minlats+dom.maxlats),lon_0=0.5*(dom.minlons+dom.maxlons))
        #map = Basemap(width=1200000,height=800000,
        #    rsphere=(6378137.00,6356752.3142),\
        #    resolution='h',area_thresh=100.,projection='lcc',\
        #    lat_1=-20.4,lat_2=-24.2,\
        #    lat_0=-22,lon_0=-48.3)

        #Config map
        map.drawcoastlines()
        map.drawcountries()
        #map.fillcontinents(color = 'coral')
        map.drawmapboundary()
        map.drawstates()

        map.ax = ax
        
        # convert the bin mesh to map coordinates:
        self.x_bins_c, self.y_bins_c = map(dom.lon_bins_c_2d, dom.lat_bins_c_2d) # will be plotted using pcolormesh
        self.x_bins_ext, self.y_bins_ext = map(dom.lon_bins_ext_2d, dom.lat_bins_ext_2d) # will be plotted using pcolormesh

        #Save fig basic config
        self.map = map
        self.fig = fig


    def map_density_data(self, lng, lat, title, dir):
        
        #Calculate 2d histogram
        density, _, _ = np.histogram2d(lat, lng, [self.dom.lat_bins_ext, self.dom.lon_bins_ext], density=False)

        #Create color plot
        plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, density, cmap="hot_r", norm=colors.LogNorm(), snap=True)

        #Add stuf to plot
        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)

        #Save density plot to folder "dir"
        filename = dir+"/density_"+title+".eps"
        plt.savefig(filename, dpi=300)


    def map_data(self, data, title, dir):
        
        #2d color plot of data
        plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, density=False)  
            #cmap="hot_r", norm=colors.LogNorm(), snap=True)

        #Config
        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)
    
        #Save density plot to folder "dir"
        filename = dir+"/map_data_"+title+".eps"
        plt.savefig(filename, dpi=300)

    def map_reg_data(self, network, title, dir):
        
        data=network.region_grid
        data=data.astype(int)

        #np.savetxt('tmp.txt', data, fmt='%3i')
        #reg = np.unique(data)
        
        #cols = colors.cnames.keys()
        cols = ['blue', 'red', 'yellow', 'orange',  'limegreen', \
            'violet', 'purple', 'brown', 'cyan', 'olive', 'coral','lightgreen' ,'grey']
        
        cmap = colors.ListedColormap(cols, N=(network.nregions+1))
        #boundaries = reg 
        #norm = colors.BoundaryNorm(boundaries, len(cols), clip=True)
        
        #2d color plot of data
        plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, cmap=cmap, snap=True) #, norm=norm)  
            #cmap="hot_r", norm=colors.LogNorm(), snap=True)

        #Config
        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)
        cbar.set_ticks(range(-1, network.nregions+1, 1))
        cbar.set_ticklabels(range(-1, network.nregions+1, 1))
    
        #Add labels 
        reg = list(network.regions_in_latlon.values())
        
        for lat, lon, i in reg:
            #print(lon, lat, i)
            xpt, ypt = self.map([lon], [lat])
            try: 
                #print(xpt[0], ypt[0], str(network.region_full.get(i)))
                self.map.plot(xpt, ypt, 'kx', markersize=1)
                plt.text(xpt[0], ypt[0], str(network.regions.get(i)),fontsize=5)
                #plt.text(xpt[0], ypt[0], str(i),fontsize=10)
            except:
                pass
            
        #Save density plot to folder "dir"
        filename = dir+"/map_data_"+title+".eps"
        plt.savefig(filename, dpi=300)