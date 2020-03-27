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

import matplotlib.pyplot as plt


import geopy.distance
import geopandas as gpd

from shapely.geometry import Point, Polygon


import tqdm 

from mdyn_daydata import DayData
from mdyn_map import Map
from mdyn_extras import distance, distance_lat, distance_lon, daterange, matprint, round_down, round_up


class Network:

    #cities = {}

    def __init__(self, 
        domain = None, 
        domain_gran = None,
        domain_shape = None, 
        subdomains = None,
        subdomains_gran = None, 
        subdomains_shape=None,
        latlon_gran = 0.01,
        load = False
        ):
        print("")
        print("Mobile Dynamics Network Construction")
        print("-----------------------------------")

        self.domain = domain
        self.domain_gran= domain_gran
        self.domain_shape = domain_shape
        self.subdomains = subdomains
        self.subdomains_gran = subdomains_gran
        self.subdomains_shape = subdomains_shape
        self.latlon_gran = latlon_gran
        self.load = load

        #Load main domain
        self.df_domain = self.load_domain() 

        #Load subdomains
        self.df_subdomains = self.load_subdomains()

        #Create in/out region lists
        self.create_regions()

        #Build grid network
        self.build_grid_network()


    def load_domain(self):

        #Get map shapes files
        self.domain_shape_file = self.domain_shape+".shp"
        self.domain_shape_file_mdyn = self.domain_shape+"mdyn.shp"

        #Load if exists and load flag set
        if os.path.exists(self.domain_shape_file_mdyn) and self.load:

            df = gpd.read_file(self.domain_shape_file_mdyn)
            print("  Domain shape file loaded.")

        else: #Build map structure with neighbours
            df = gpd.read_file(self.domain_shape_file)
            print("  Creating basic domain shape file...", end = '')
            
            df["NEIGHBORS"] = None  # add NEIGHBORS column
            for index, reg in df.iterrows():   
                # get 'not disjoint' countries
                df_local = df[~df.geometry.disjoint(reg.geometry)]
                neighbors = df_local[self.domain_gran].tolist()
                
                # remove own name from the list
                neighbors = [ name for name in neighbors if reg[self.domain_gran] != name ]
                
                # add names of neighbors as NEIGHBORS value
                df.at[index, "NEIGHBORS"] = ",".join(neighbors)
                
            #Save modified shape file for future use
            print("Done. Saving domain shape structure for future use")
            df.to_file(self.domain_shape_file_mdyn)   

        return df

    def load_subdomains(self):
        
        #Get map subdomains shapes files
        self.subdomains_shape_file = self.subdomains_shape+".shp"
        self.subdomains_shape_file_mdyn = self.subdomains_shape+"mdyn.shp"

        #Get map subdomains shapes
        if os.path.exists(self.subdomains_shape_file_mdyn) and self.load:
            df = gpd.read_file(self.subdomains_shape_file_mdyn)
            print("  Subdomains shape file loaded.")

        else: #Build map subdomains structure 
            df = gpd.read_file(self.subdomains_shape_file)
            print("  Creating basic subdomains shape file...", end = '')
            
            df["lonc"] = None #long centroid
            df["latc"] = None #lat centroid
            for index, mun in df.iterrows():   
                df.at[index, "lonc"] = mun.geometry.centroid.x
                df.at[index, "latc"] = mun.geometry.centroid.y

            df["latc"]=df["latc"].astype(float)
            df["lonc"]=df["lonc"].astype(float)

            #Save modified shape file for future use
            print("Done. Saving subdomains shape structure for future use")
            df.to_file(self.subdomains_shape_file_mdyn)

        #print(df)
        #print(df.head)

        #Indexes columns
        idx = np.arange(len(df))
        df["idx"]=idx.astype(int)    

        #print(df)
        #print(self.regions_in, self.regions_in_latlon)
        return df

    def create_regions(self):

        #inner regions (subdomains)
        #--------------------------

        self.regions_in = self.df_subdomains[self.subdomains_gran].to_dict()
        filt = [self.subdomains_gran, "latc", "lonc", "idx"]
        self.regions_in_latlon = self.df_subdomains.filter(filt).set_index(self.subdomains_gran).T.to_dict('list') 
        self.nreg_in = len(self.regions_in)

        #Outer regions (domain)
        #-------------------------
        
        df_domain_local = self.df_domain[self.df_domain[self.domain_gran] == self.domain]
        self.domain_geometry=df_domain_local.geometry.values[0]
        self.domain_neib = df_domain_local.NEIGHBORS.values[0]
        self.domain_neib = self.domain_neib.split(',') 
        self.df_domain_nb=self.df_domain[self.df_domain[self.domain_gran].isin(self.domain_neib)]
        domain_limit_coords=list(self.domain_geometry.envelope.exterior.coords)


        # Regular grid structure
        x = []
        y = []
        for xy in domain_limit_coords:
            x.append(xy[0])
            y.append(xy[1])
        self.minlons = round_down(min(x), 1)- 0.1
        self.maxlons = round_up(max(x), 1)  + 0.1
        self.minlats = round_down(min(y), 1)- 0.1
        self.maxlats = round_up(max(y), 1)  + 0.1
        print("  Domain Box: ")
        print( "   Lon:", self.minlons, self.maxlons)
        print( "   Lat:", self.minlats, self.maxlats)

        #Latlon spacing
        self.dlon = self.latlon_gran
        self.dlat = self.latlon_gran
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

        #Region grid is composed of data points, cell centres
        self.region_grid = np.zeros((self.nlat+1, self.nlon+1)).astype(int)

        print("   nLon:", self.nlon)
        print("   nLat:", self.nlat)

        #Regions out are out of main domain
        regions_out = self.df_domain_nb[self.domain_gran].to_dict() 
        reg_out = list(regions_out.values())
        self.nreg_out = len(reg_out)

        #Align indexing of out regions with in regions
        self.regions_out = {self.nreg_in+key:reg for key, reg in enumerate(reg_out)}
        #print(self.regions_out)

        #Join in and out regions
        self.regions = {**self.regions_in, **self.regions_out}
        self.nregions = len(self.regions)
        
        #Add extra region for all the rest (ocean or other places)
        #self.regions[str(self.nregions)] = 'NAN'
        #self.nregions = self.nregions + 1

        print("  Defined the following regions for the network:")
        if self.nregions > 25:
            print("  First 10 regions: ", dict(list(self.regions.items())[0:10]))
            print("  Last 10 regions:", dict(list(self.regions.items())[self.nregions-10:self.nregions]))
        else:
            print(self.regions)
        

    #Build regions grid
    def build_grid_network(self):
        
        print("Building grid network...")
        
        self.gridname = 'maps/regions_'+self.domain+"_"+\
            self.subdomains+\
            "_lats"+str(self.minlats)+"_"+str(self.maxlats)+\
            "_lons"+str(self.minlons)+"_"+str(self.maxlons)+\
            "_dlat"+str(self.dlat)+"_dlon"+str(self.dlon)


        #check if network pre built
        if os.path.exists(self.gridname+".npy") and self.load:
            self.region_grid=np.load(self.gridname+".npy")
            print("Regions loaded from file "+self.gridname)
        else:
            #Grid is based on cell centers
            for i, lat in enumerate(tqdm.tqdm(self.lat_bins_c)):
                for j, lon in enumerate(self.lon_bins_c):
                    #print(lat, lon)
                    reg0 = self.get_closest_region(lat, lon) #This is the center of cell
                    if reg0 != -1:
                        self.region_grid[i,j]=reg0
                    else:
                        #Check if part of the cell is in a region, otherwise it is ocean
                        reg1 = self.get_closest_region(lat-self.dlat, lon-self.dlon)
                        if reg1 != -1:
                            ireg=reg1
                        else:
                            reg2 = self.get_closest_region(lat+self.dlat, lon-self.dlon)
                            if reg2 != -1:
                                ireg=reg2
                            else:
                                reg3 = self.get_closest_region(lat-self.dlat, lon+self.dlon)
                                if reg3 != -1:
                                    ireg=reg3
                                else:
                                    reg4 = self.get_closest_region(lat+self.dlat, lon+self.dlon)
                                    if reg4 != -1:
                                        ireg=reg4
                                    else:
                                        ireg = -1
                        
                        self.region_grid[i,j] = ireg

            #Save this region grid for furute use, as it takes time to build
            np.savetxt(self.gridname+".csv", self.region_grid)
            np.save(self.gridname, self.region_grid)
            print("Regions saved in file "+self.gridname)

            #Map the regions
            map = Map(self)
            map.map_reg_data(self, self.gridname )
            
    def get_closest_region(self, lat, lon):
                
        p = Point(lon, lat)
        inmaindomain = p.within(self.domain_geometry)
        #print(p, inmaindomain)

        ireg=-1
        if inmaindomain:
            
            lat_tmp=self.df_subdomains["latc"].to_numpy()
            lon_tmp=self.df_subdomains["lonc"].to_numpy()
            
            latv = np.full(len(lat_tmp), lat)
            lonv = np.full(len(lon_tmp), lon)

            dist_tmp = distance(lonv, latv, lon_tmp, lat_tmp)

            k = 10 # k nearest neighbours
            inearreg = np.argpartition(dist_tmp, k)
            
            #Use geometry to check if point in subregion
            for ireg in inearreg:
                subreg_geometry = self.df_subdomains.loc[ireg, "geometry"]
                in_region=p.within(subreg_geometry)
                if in_region:
                    return ireg
        
        else:
            #check if in neighbour states
            for index, nb in self.df_domain_nb.iterrows(): 
                nb_name=nb[self.domain_gran]
                innbreg=p.within(nb.geometry)
                if innbreg:
                    ireg = self.regions_out.get(nb_name, self.nregions)
        
        return ireg

    def add_reg_to_df(self, dom, data):
        
        for day in data:
            #Add column with region tag
            #for each event
            for i in range(2):
                s=str(i)
                lon=day.df['lng'+s].values
                lat=day.df['lat'+s].values
                lonnan=np.isnan(lon)
                latnan=np.isnan(lat)
                nan = lonnan*latnan
                ilon=((lon[~nan]-dom.minlons)/dom.dlon).astype(int)
                ilat=((lat[~nan]-dom.minlats)/dom.dlat).astype(int)
                #print(lon[~nan], lat[~nan], ilon, ilat)
                reg = np.zeros(day.n).astype(int)
                reg[nan] = -1
                reg[~nan]=self.region_grid[ilat, ilon]
                day.df['reg'+s]=reg

            #Add column with moved or not
            reg0 = day.df['reg0']
            reg1 = day.df['reg1']
            mov = reg0 != reg1
            day.df['mov_reg']=mov

            #day.df.to_csv("tmp.csv")

    def add_reg_to_daydf(self, daydata):
        
        #Add column with region tag
        #for each event
        for i in range(2):
            s=str(i)
            lon=daydata.df['lng'+s].values
            lat=daydata.df['lat'+s].values
            lonnan=np.isnan(lon)
            latnan=np.isnan(lat)
            nan = lonnan*latnan
            ilon=((lon[~nan]-self.minlons)/self.dlon).astype(int)
            ilat=((lat[~nan]-self.minlats)/self.dlat).astype(int)
            #print(lon[~nan], lat[~nan], ilon, ilat)
            reg = np.zeros(daydata.n).astype(int)
            reg[nan] = -1
            reg[~nan]=self.region_grid[ilat, ilon]
            daydata.df['reg'+s]=reg

        #Add column with moved or not
        reg0 = daydata.df['reg0']
        reg1 = daydata.df['reg1']
        mov = reg0 != reg1
        daydata.df['mov_reg']=mov

        #day.df.to_csv("tmp.csv")

    def month_users(self):

        #manual
        self.monthly_users = {
                "2018-04":  6325668,
                "2018-05":  6515725,
                "2018-06":	6228902,
                "2018-07":	6000567,
                "2018-08":	6188322,
                "2018-09":	6167131,
                "2018-10":	6180900,
                "2018-11": 	7108021,
                "2018-12":	7634732,
                "2019-01":	6852594,
                "2019-02":	7169397,
                "2019-03":	7247796,
            }

    def calc_transition_matrix(self, day):
        
        print()
        print("Generating transition matrices...", end="")

        df=day.df
        month=day.month
        
        #df.to_csv("tmp.csv", header=True)
        table = df.pivot_table(values='dt1', index=['reg1'],\
                     columns=['reg0'], aggfunc=np.count_nonzero, fill_value=0, dropna=False)
        
        
        #Remove other states
        neib_states = range(self.nreg_in, self.nregions, 1)
        #print(neib_states)
        try:
            table = table.drop(columns=neib_states)
            table = table.drop(neib_states, axis=0)
        except:
            pass

        #remove the problematic -1 regions
        #Columns
        try:
            table = table.drop(columns=[-1])
        except:
            pass
        #Rows
        try:
            table = table.drop([-1], axis=0)
        except:
            pass

        #The resulting table has the people that moved, we now need to include people that did not move
        if self.nregions<20:
            print("")
            print("Transition Matrix (number of people moving to/from regions)")
            print(table)
            print()

        #self.month_users()

        #total_users = self.monthly_users.get(month, 0)
        #moving_users = day.n
        #steady_users = total_users - moving_users
        #print("")
        #print("Month, Total Users, Day moving users, Day steady users" )
        #print(month, total_users, moving_users, steady_users)
        #steady_users_per_reg = steady_users * self.regions_pop_freq
        #print("Users per region:", steady_users_per_reg)

        #Columns are regions at time 0
        #Rows are regions at time 1
        mat = table.as_matrix(columns=None)
        
        #np.fill_diagonal(mat, mat.diagonal() + steady_users_per_reg)
        #print(mat)
        
        if self.nregions<20:
            print("Transition matrix including steady users")
            matprint(mat)
            print()
        #Normalize
        mat_normed = mat / mat.sum(axis=0)
        if self.nregions<20:
            print("Normalized transition matrix (transition probability)")
            matprint(mat_normed)
        
        if self.nregions > 20:
            print("..done")

        return mat_normed