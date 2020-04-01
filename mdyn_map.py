#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import numpy as np

from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.colors as colors
import matplotlib.cm as cm

from itertools import product

import tqdm as tqdm

import sys

import mdyn_network 
from mdyn_extras import matprint
import mdyn_extras as mex 

class Map:
    def __init__(self, network): 

        #Init the figure
        fig, ax = plt.subplots( figsize=(10, 5))
        
        lat0=0.5*(network.maxlats+network.minlats)
        lon0=0.5*(network.maxlons+network.minlons)

        #Define map projection
        #map = Basemap(projection='merc', resolution='h',
        #    llcrnrlon=dom.minlons-1, llcrnrlat=dom.minlats-1,
        #    urcrnrlon=dom.maxlons+1, urcrnrlat=dom.maxlats+1)
        map = Basemap(width=1.1e6,height=7.2e5,\
            projection='gnom',lat_0=lat0,lon_0=lon0, resolution="f")
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
        map.drawcountries(color='k',linestyle='-', linewidth=0.8)
        #map.fillcontinents(color = 'coral')
        map.drawmapboundary()
        map.drawstates(color='k',linestyle='--', linewidth=0.6)
        map.drawparallels(np.arange(-50,0,1), labels=[False,True,True,False])
        map.drawmeridians(np.arange(-180,180,1), labels=[False,False,True,False])
        
        map.ax = ax
        self.dom = network
        
        # convert the bin mesh to map coordinates:
        self.x_bins_c, self.y_bins_c = map(network.lon_bins_c_2d, network.lat_bins_c_2d) # will be plotted using pcolormesh
        self.x_bins_ext, self.y_bins_ext = map(network.lon_bins_ext_2d, network.lat_bins_ext_2d) # will be plotted using pcolormesh

        #Save fig basic config
        self.map = map
        self.fig = fig


    def map_density_data(self, lng, lat, title, filename):
        
        #Calculate 2d histogram
        density, _, _ = np.histogram2d(lat, lng, [self.dom.lat_bins_ext, self.dom.lon_bins_ext], density=False)
        
        #Create color plot
        plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, density, cmap="hot_r", norm=colors.LogNorm(), snap=True)
        
        #Add stuf to plot
        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)
        
        #Save density plot to folder "dir"
        plt.savefig(filename, dpi=300)

    def map_data(self, data, title, dir):
        
        #2d color plot of data
        plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, cmap="hot_r") #, density=False)  
            #cmap="hot_r", norm=colors.LogNorm(), snap=True)

        #Config
        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)
    
        #Save density plot to folder "dir"
        #filename = dir+"/map_data_"+title.strip()+".eps"
        filename = dir+"/map_data_"+title.strip()+".jpg"
        plt.savefig(filename, dpi=300)

    def map_reg_data(self, network, title):
        
        data=network.region_grid
        data=data.astype(float)

        #matprint(data)
        data[data<0]=np.nan #network.nregions+1
        data=data+0.5
        #matprint(data)
        #np.savetxt('tmp.txt', data, fmt='%3i')
        #reg = np.unique(data)
        
        #cols = colors.cnames.keys()
        cols = ['blue', 'red', 'yellow', 'orange',  'limegreen', \
            'violet', 'purple', 'brown', 'cyan', 'olive', 'coral', 'lightgreen' ,'grey', \
               'blue', 'red', 'yellow', 'orange',  'limegreen' ]
        
        cmap = colors.ListedColormap(cols, N=(network.nregions+2))
        #boundaries = reg 
        #norm = colors.BoundaryNorm(boundaries, len(cols), clip=True)
        
        #2d color plot of data
        plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, cmap=cmap, snap=True) #, norm=norm)  
            #cmap="hot_r", norm=colors.LogNorm(), snap=True)

        #Config
        bounds = np.linspace(0, network.nregions, network.nregions+1)
        reg = list(network.regions_in_latlon.values())
        
        if len(reg)<15:
            cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=25, fraction=0.1, pad=0.01, boundaries=bounds, \
                spacing='proportional', ticks=bounds)
            cbar.set_label(title,size=12)
            cbar.set_ticks(range(network.nregions+2))
            cbar.set_ticklabels(range(network.nregions+2))
        
            #Add labels 
            
            for lat, lon, i in reg:
                #print(lon, lat, i)
                xpt, ypt = self.map([lon], [lat])
                try: 
                    #print(xpt[0], ypt[0], str(network.region_full.get(i)))
                    self.map.plot(xpt, ypt, 'kx', markersize=1)
                    plt.text(xpt[0], ypt[0], str(network.regions.get(i))+"-"+str(i),fontsize=6)
                    #plt.text(xpt[0], ypt[0], str(i),fontsize=10)
                except:
                    pass
            
        #Save density plot to folder "dir"
        plt.tight_layout()
        
        filename=title
        print(filename)
        #filename = dir+"/map_data_"+title+".eps"
        filename = filename+".jpg"
        plt.savefig(filename, dpi=300)
        

    def map_movemat_by_reg(self, mat, ireg0, reg1, network, title, filename):
        
        data=network.region_grid
        data=data.astype(float)

        move_from_r0=mat[: , ireg0]

        for ir1 in reg1:
            data[data==ir1]=move_from_r0[ir1]
    
        data[data<0]=0.0

        plt.title(title, y=1.08)

        #2d color plot of data
        cmap = "hot_r" 
        plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, cmap=cmap, norm=colors.LogNorm(), snap=True) #, norm=norm)  
            #cmap="hot_r", norm=colors.LogNorm(), snap=True)

        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=25, fraction=0.1, pad=0.01, \
            spacing='proportional')
        cbar.set_label("Probability",size=12)
                    
        #plt.tight_layout()
        plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
        
        #filename = dir+"/map_data_"+title+".eps"
        filename = filename+".jpg"
        plt.savefig(filename, dpi=300)   
        
    def map_move_by_reg(self, movevec, reg1, network, title, filename):
        print(network.regions)

        print("REG1:", reg1, len(reg1))
        print("movve:", movevec, len(movevec), len(reg1))

        data=network.region_grid
        data=data.astype(float)
        #mex.matprint(data)
        move_from_r0=movevec

        for ir1 in reg1:
            data[data==ir1]=move_from_r0[ir1]
    
        data[data<0]=0.0

        title = title.replace("_", " ")
        plt.title(title, y=1.08)

        #2d color plot of data
        cmap = "hot_r" 
        plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, cmap=cmap, norm=colors.LogNorm(), snap=True) #, norm=norm)  
            #cmap="hot_r", norm=colors.LogNorm(), snap=True)

        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=25, fraction=0.1, pad=0.01, \
            spacing='proportional')
        cbar.set_label("Probability",size=12)
                    
        #plt.tight_layout()
        plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
        
        #filename = dir+"/map_data_"+title+".eps"
        filename = filename+".jpg"
        plt.savefig(filename, dpi=300)   

    def map_lat_lon_z_data(self, lats, lons, z, title, filename):
        
        x, y = np.meshgrid(lons, lats)
        x, y = self.map(x, y)
        
        plt.title(title, y=1.08)
        print("  Plotting: ", title)
        #2d color plot of data
        cmap = "hot_r" 
        if "IsoIndex" in title:
            plt.pcolormesh(x, y, z, vmin=0.001, vmax=1.0, cmap=cmap, norm=colors.LogNorm(), snap=True) #, norm=norm)  
        else:
            plt.pcolormesh(x, y, z, cmap=cmap, norm=colors.LogNorm(), snap=True) #, norm=norm)  

        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=25, fraction=0.1, pad=0.01, \
            spacing='proportional')
        cbar.set_label("Probability",size=12)
        
        #plt.tight_layout()
        plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
        
        #filename = dir+"/map_data_"+title+".eps"
        filename = filename+".jpg"
        plt.savefig(filename, dpi=300)   
        