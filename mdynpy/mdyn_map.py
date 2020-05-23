#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import numpy as np

import matplotlib as mpl
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.colors as colors
import matplotlib.cm as cm
from matplotlib.ticker import MaxNLocator
from mpl_toolkits.axes_grid1 import make_axes_locatable

import networkx as nx

from itertools import product

import tqdm as tqdm

import sys

import mdynpy.mdyn_network 
from mdynpy.mdyn_extras import matprint
import mdynpy.mdyn_extras as mex 

class Map:
    def __init__(self, network, zoom=[False, 0, 0, 0, 0, False]): 

        #Init the figure
        linewidth=0.8
        
        if zoom[0]:
            factor = 1.0e5
            lat0=0.5*(zoom[2]+zoom[1])
            lon0=0.5*(zoom[4]+zoom[3])
            width = abs(zoom[4]-zoom[3])*factor
            height = abs(zoom[2]-zoom[1])*factor
            #print(lat0, lon0, height, width , zoom[2]-zoom[1] ,  zoom[4]-zoom[3])
        else:
        
            factor = 1.0e5
            lat0=0.5*(network.maxlats+network.minlats)
            lon0=0.5*(network.maxlons+network.minlons)
            width = abs(network.maxlons-network.minlons)*factor
            height = abs(network.maxlats-network.minlats)*factor
            #print(lat0, lon0, height, width,  network.maxlats-network.minlats, network.maxlons-network.minlons)
    
        #width=1.1e6
        #height=7.2e5
        #in meters
        # 1 deg aprox 110km, so 10e5
        
        fwidth = width/factor
        fheight = height/factor
        if fwidth > 25: #Brasil plot - very large!
            #print(fwidth, fheight)
            fwidth = fwidth/5
            fheight = fheight/5
            width = width*1.1
            height = height*1.1
            #print(fwidth, fheight)
        if fwidth > 15: #Bug region plot, but not all brasil
            #print(fwidth, fheight)
            fwidth = fwidth/3
            fheight = fheight/3
            width = width*1.05
            height = height*1.05
            #print(fwidth, fheight)
        if fwidth < 5: #small region plot, zoom
            #print(fwidth, fheight)
            fwidth = fwidth*4
            fheight = fheight*4
            width = width*1.05
            height = height*1.05
            #print(fwidth, fheight)
        fig, ax = plt.subplots( figsize=(fwidth, fheight))

        #Define map projection
        #map = Basemap(projection='merc', resolution='h',
        #    llcrnrlon=dom.minlons-1, llcrnrlat=dom.minlats-1,
        #    urcrnrlon=dom.maxlons+1, urcrnrlat=dom.maxlats+1)
        map = Basemap(width=width,height=height,\
            projection='gnom',lat_0=lat0,lon_0=lon0, resolution="h")
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

        #map.drawcoastlines(color='k',linestyle='-', linewidth=0.2)
        map.drawcountries(color='k',linestyle='-', linewidth=linewidth)
        #map.fillcontinents(lake_color='white',zorder=1)
        #map.drawcoastlines(zorder=1,color='white',linewidth=0)
        #map.fillcontinents(color = 'coral')
        #map.drawmapboundary(fill_color='aqua')
        #map.fillcontinents(lake_color='aqua')
        map.drawmapboundary()
        #map.drawstates(color='k',linestyle='--', linewidth=0.2)
        if width/factor > 10: 
            map.drawparallels(np.arange(-50,10,2), labels=[True,False,False,False])
            map.drawmeridians(np.arange(-180,180,2), labels=[False,False,True,False])
        else:
            map.drawparallels(np.arange(-50,10,1), labels=[True,False,False,False])
            map.drawmeridians(np.arange(-180,180,1), labels=[False,False,True,False])
        map.ax = ax
        self.dom = network
        
        #Plot domain  map
        #print(network.domain_geometry)
        poly = network.domain_geometry
        if poly.geom_type == 'MultiPolygon':
            # do multipolygon things.
            for poly_local in list(poly):
                x, y = poly_local.exterior.coords.xy
                x, y = map(x, y)
                map.plot(x, y, marker=None, color='k',linestyle='-', linewidth=linewidth)
        elif poly.geom_type == 'Polygon':
        # do polygon things.          
            x, y = poly.exterior.coords.xy
            x, y = map(x, y)
            map.plot(x, y, marker=None, color='k',linestyle='-', linewidth=linewidth)
        
        if zoom[0]:
            subdomain_geometry = network.df_subdomains.geometry
            for poly in subdomain_geometry:
                if poly.geom_type == 'MultiPolygon':
                    # do multipolygon things.
                    for poly_local in list(poly):
                        x, y = poly_local.exterior.coords.xy
                        x, y = map(x, y)
                        map.plot(x, y, marker=None, color = '0.35',linestyle=':', linewidth=linewidth/2)
                elif poly.geom_type == 'Polygon':
                # do polygon things.          
                    x, y = poly.exterior.coords.xy
                    x, y = map(x, y)
                    map.plot(x, y, marker=None, color = '0.35',linestyle=':', linewidth=linewidth/2)

        # convert the bin mesh to map coordinates:
        self.x_bins_c, self.y_bins_c = map(network.lon_bins_c_2d, network.lat_bins_c_2d) # will be plotted using pcolormesh
        self.x_bins_ext, self.y_bins_ext = map(network.lon_bins_ext_2d, network.lat_bins_ext_2d) # will be plotted using pcolormesh

        #Save fig basic config
        self.map = map
        self.fig = fig
        
        self.dpi = 200
        self.max_dpi = 300


    def map_density_data(self, lng, lat, title, filename):
        
        #Calculate 2d histogram
        density, _, _ = np.histogram2d(lat, lng, [self.dom.lat_bins_ext, self.dom.lon_bins_ext], density=False)
        
        #Create color plot
        plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, density, cmap="hot_r", norm=colors.LogNorm(), snap=True)
        
        #Add stuf to plot
        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)
        
        #Save density plot to folder "dir"
        plt.savefig(filename, dpi=200)

    def map_data(self, data, title, dir):
        
        #2d color plot of data
        plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, cmap="hot_r") #, density=False)  
            #cmap="hot_r", norm=colors.LogNorm(), snap=True)

        #Config
        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=20, fraction=0.1, pad=0.01)
        cbar.set_label(title,size=12)
    
        #Save density plot to folder "dir"
        #filename = dir+"/map_data_"+title.strip()+".eps"
        filename = dir+"/map_data_"+title.strip()+".png"

        dpi = min(self.max_dpi, int(max(self.dpi, self.dpi*np.max(data.shape)/1000)))
        print("Using ", dpi, " dpi")
        plt.savefig(filename, dpi=dpi)

    def map_reg_data(self, network, title, filename=None):
        
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
        
        if len(reg)<15 and len(reg)>1:
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
        
        if filename == None:
            filename = title
            #filename = dir+"/map_data_"+title+".eps"
            filename = filename+".jpg"

        print("File: ", filename)
        dpi = min(self.max_dpi, int(max(self.dpi, self.dpi*np.max(data.shape)/1000)))
        print("Using ", dpi, " dpi")
        plt.savefig(filename, dpi=dpi, transparent=False)
        

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
        #print(network.regions)

        #print("REG1:", reg1, len(reg1))
        #print("movve:", movevec, len(movevec), len(reg1))
        
        data=network.region_grid
        data=data.astype(float)
        #mex.matprint(data)
        move_from_r0=movevec

        for ir1 in reg1:
            data[data==ir1]=move_from_r0[ir1]
    
        data[data<0]=np.nan

        title = title.replace("_", " ")
        filename = filename.replace("\n", "")
        plt.title(title, y=1.08, size="10")

        #2d color plot of data
        cmap = "hot_r" 

        if "Diag" in title:
            plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, vmin=0.1, vmax=1.0, cmap=cmap, snap=True) #, norm=norm)  
            label = "Probability"
        elif "index" in title:
            plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, vmin=0, vmax=1, cmap=cmap , snap=True) #, norm=norm)  
            label = "Risk index"
        elif "time" in title:
            plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, vmin=1.0, vmax=60.0, cmap="hot", snap=True) #, norm=norm)  
            label = "Days to reach risk limit"
        elif "Model" in title:
            plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, vmin=0.01, cmap=cmap, norm=colors.LogNorm(), snap=True) #, norm=norm)  
            label = "Number of People"
        elif "Simul" in title:
            plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, vmin=1, vmax=100., cmap=cmap, norm=colors.LogNorm(), snap=True) #, norm=norm)  
            label = "Number of People"
        else:
            plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, vmin=0.000001, vmax=1.0, cmap=cmap, norm=colors.LogNorm(), snap=True) #, norm=norm)  
            label = "Probability"
            #cmap="hot_r", norm=colors.LogNorm(), snap=True)

        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=25, fraction=0.1, pad=0.01, \
            spacing='proportional')
        #cbar.set_label(label,size=12)
        if "index" in title:
            cbar.set_ticks([0, 0.5, 1.0])
            cbar.ax.set_xticklabels([ 'Low', 'Medium', 'High']) 

        
        #plt.tight_layout()
        plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
        
        #filename = dir+"/map_data_"+title+".eps"
        #filename = filename+".jpg"
        plt.savefig(filename, dpi=150)   
        

    def map_reg_var(self, regvec, regs, network, title, filename):
        #print(network.regions)

        #print("REG1:", reg1, len(reg1))
        #print("movve:", movevec, len(movevec), len(reg1))
        print("  Plotting: ", filename)
        data=network.region_grid
        data=data.astype(float)
        #mex.matprint(data)

        #print(regvec)
        for ir1 in regs:
            try:
                data[data==ir1]=regvec[ir1]
            except:
                data[data==ir1]=np.nan

        data[data<0]=np.nan

        title = title.replace("_", " ")
        filename = filename.replace("\n", "")
        plt.title(title, y=1.08, size="10")

        #2d color plot of data
        cmap = "hot_r" 

        if "Index" in title:
            plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, vmin=0.0, vmax=1.0, cmap=cmap , snap=True) #, norm=norm)  
            label = ""
        elif "SEIR" in title:
            plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data,  vmin=0.01, cmap=cmap, norm=colors.LogNorm(), snap=True)
            label=""
        else:
            plt.pcolormesh(self.x_bins_ext, self.y_bins_ext, data, vmin=1.0, cmap=cmap, snap=True) #, norm=norm)  
            label = ""
        
        cbar = plt.colorbar(orientation='horizontal', shrink=0.5, aspect=25, fraction=0.1, pad=0.01, \
            spacing='proportional')
        #cbar.set_label(label,size=12)
        if "index" in title:
            cbar.set_ticks([0, 0.5, 1.0])
            cbar.ax.set_xticklabels([ 'Low', 'Medium', 'High']) 


        #plt.tight_layout()
        plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
        
        #filename = dir+"/map_data_"+title+".eps"
        if filename[-4:] != ".jpg":
            filename = filename+".jpg"
        plt.savefig(filename, dpi=300)   

    def map_lat_lon_z_data(self, lats, lons, z, title, filename):
        
        x, y = np.meshgrid(lons, lats)
        x, y = self.map(x, y)
        
        plt.title(title, y=1.08)
        print("  Plotting: ", filename)
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
        
    def map_network(self, mat, reg0, title, filename):

        network = self.dom
        plt.title(title, y=1.08)
        print("  Plotting: ", filename)

        #print(network.regions_in_latlon)
        n=len(network.regions_in_latlon)
        
        pos={}
        # define position in basemap
        for i, reg in enumerate(network.regions_in_latlon.values()):
            x, y = self.map(reg[1], reg[0])
            #print(i, reg[0], reg[1], x, y)
            pos[i]=[x,y]
        
        #Only graph inner nodes
        mattmp=mat[:n, :n]
        np.fill_diagonal(mattmp, 0)

        #Graph
        G = nx.from_numpy_matrix(mattmp, create_using=nx.DiGraph)

        #Nodes (Katz)
        N = len(G)
        print("Network len:", N)
        node_sizes = [0.5 for i in range(N)]

        #Edges
        M = G.number_of_edges()
        print("Network edges:", M)

        edges, weights = zip(*nx.get_edge_attributes(G,'weight').items())
        weights = np.array(weights)  

        np.set_printoptions(threshold=sys.maxsize)
        
        #Set categories
        limits = np.percentile(weights, [10, 70, 80, 90, 95, 99])
        weights = np.digitize(weights, limits, right=True)/len(limits)
        
        #weights=np.where(weights>10, 1, 0)
        maxw = max(weights) #
        #print(weights, maxw)
        edge_colors = weights #[2+M*(i+2)/maxw for i in weights] #100*weights #range(2, M + 2)
        edge_widths = 0.1+0.9*weights
        edge_alphas = 0.3+weights*0.5
    
        nodes = nx.draw_networkx_nodes(G, pos, ax=self.map.ax, node_size=node_sizes, node_color='black', with_labels=True)
        edges = nx.draw_networkx_edges(G, pos, ax=self.map.ax, node_size=1.0, arrowstyle='->',
                                    arrowsize=5, edgelist=edges, edge_color=edge_colors,
                                    edge_cmap=plt.cm.hot_r, width=edge_widths,
                                    connectionstyle='arc3, rad=0.1')
        
        # set alpha value for each edge
        for i in range(M):
            edges[i].set_alpha(edge_alphas[i])

        pc = mpl.collections.PatchCollection(edges, cmap=plt.cm.hot_r)
        pc.set_array(edge_colors)
        plt.colorbar(pc)

        #ax = plt.gca()
        #ax.set_axis_off()
        plt.tight_layout() 
        plt.savefig(filename, dpi=300)   

    def map_network_data(self, data, mat, reg0, title, filename):
        
        network = self.dom
        
        plt.title(title, y=1.08)
        print("  Plotting: ", filename)

        #print(network.regions_in_latlon)
        n=len(network.regions_in_latlon)
        
        pos={}
        # define position in basemap
        for i, reg in enumerate(network.regions_in_latlon.values()):
            x, y = self.map(reg[1], reg[0])
            #print(i, reg[0], reg[1], x, y)
            pos[i]=[x,y]
        
        #Only graph inner nodes
        mattmp=mat[:n, :n]
        np.fill_diagonal(mattmp, 0)

        #Graph
        G = nx.from_numpy_matrix(mattmp, create_using=nx.DiGraph)
        N = len(G)
        print("Network len:", N)
        M = G.number_of_edges()
        print("Network edges:", M)

        #Filer low flux edges:       
        remove = [edge for edge, w in nx.get_edge_attributes(G,'weight').items() if w <= 4] 
        #keep = [edge for edge, w in nx.get_edge_attributes(G,'weight').items() if w > 2] 
        G.remove_edges_from(remove)
        
        N = len(G)
        print("Filtred Network len:", N)
        M = G.number_of_edges()
        print("Filtred Network edges:", M)

        np.set_printoptions(threshold=sys.maxsize)
        #Nodes (Katz)
        
        
        #print(data)
        #delta=max(data)-min(data)
        #Set isolation limits
        #limits = np.array([min(data)+0.05,min(data)+delta/3, min(data)+2*delta/3, max(data)-0.05 ])
        #print("Isolation limits: ", limits)
        #nodelimits = np.array([0.2, 0.3, 0.4, 0.5, 0.6 ])
        #print(" Enforced limits: ", nodelimits)
        #dataw = np.digitize(data, nodelimits, right=True)/len(nodelimits)
        #dataw[np.isnan(data)]=np.nan
        node_colors = data
        #print(node_colors)
        node_sizes = [25 for i in range(N)]

        edges, weights = zip(*nx.get_edge_attributes(G,'weight').items())
        weights = np.array(weights)  
        
        #Set categories
        #limits = np.percentile(weights, [5, 80, 85, 90, 95, 99])
        #print("Flux limits: ", limits)
        #edlimits = np.array([1, 10, 20, 40, 80, 160, 320, 640])
        #print("  Enforced limits: ", edlimits)
        #weights = np.digitize(weights, edlimits, right=True)/len(edlimits)
        weights = np.log2(weights)
        #weights=np.where(weights>10, 1, 0)
        maxw = max(weights) #
        #print(weights, maxw)
        edge_colors = weights #[2+M*(i+2)/maxw for i in weights] #100*weights #range(2, M + 2)
        edge_widths = 0.1+0.9*(weights/maxw)
        edge_alphas = 0.3+(weights/maxw)*0.5
    
        nodes = nx.draw_networkx_nodes(G, pos, ax=self.map.ax, node_size=node_sizes, 
            node_color=node_colors, with_labels=False, linewidths= 0.3, cmap=plt.cm.winter,
            vmin=0.3, vmax=0.7)
        edges = nx.draw_networkx_edges(G, pos, ax=self.map.ax, node_size=1.0, arrowstyle='->',
                                    arrowsize=5, edgelist=edges, edge_color=edge_colors,
                                    edge_cmap=plt.cm.hot_r, width=edge_widths,
                                    edge_vmin=0 , edge_max=14,
                                    connectionstyle='arc3, rad=0.1')
        
        # set alpha value for each edge
        for i in range(M):
            edges[i].set_alpha(edge_alphas[i])

        ax = plt.gca()
        divider = make_axes_locatable(ax)
        cax = divider.append_axes("right", size="3%", pad=0.05)
        #cax = divider.new_vertical(size="5%", pad=0.5, pack_start=True)
        
        #pc = mpl.collections.PatchCollection(edges, cmap=plt.cm.hot_r, match_original=True)
        #pc.set_array(edge_colors)
        
        sm = plt.cm.ScalarMappable(cmap=plt.cm.hot_r, norm=plt.Normalize(vmin=0, vmax=14))
        sm.set_array(edge_colors)
        #cbar = plt.colorbar(sm)
        cbared = plt.colorbar(sm, cax=cax, label='Mobility (log2 number of trips / day)')        
        #cbared.set_ticks([0, 1])
        #cbared.set_ticks(np.array(range(len(edlimits)))/len(edlimits))
        #cbared.ax.set_yticklabels([ 'Low', 'High']) 
        #cbared.ax.set_yticklabels(edlimits) 

        nodes.set_array(node_colors)
        cax = divider.append_axes("bottom", size="5%", pad=0.05)
        cbarnodes = plt.colorbar(nodes, orientation="horizontal", cax=cax, label="Isolation Index")
         #label='Isolation index (normalized between min-max)'
        #cbarnodes.set_ticks([0, 0.5, 1.0])
        #cbarnodes.set_ticks(np.array(range(len(nodelimits)))/len(edlimits))
        #cbarnodes.ax.set_xticklabels([ 'Min State Isolation', 'Isolation Index', 'Max State Isolation']) 
        #cbarnodes.ax.set_xticklabels(nodelimits) 

        plt.tight_layout() 
        plt.savefig(filename, dpi=300)   

        
