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
from mdyn_extras import distance, distance_lat, distance_lon, daterange, matprint


class Network:

    cities = {}

    def __init__(self, main_state, all_mun=True):
        print("Creating/Loading network structure")
        self.all_mun = all_mun
        self.main_state = main_state
        self.load_state_data(main_state) 

    def load_state_data(self, main_state):

        #Main_State = string with Brazilian state abbrev

        #All municipalities
        if main_state=="SP" and self.all_mun == True:
            self.SP_all_mun()

        # large cities only
        if main_state=="SP" and self.all_mun == False: 
            self.SP_large_cities()

        self.nreg_in = len(self.regions_in)

        df = self.state_structure()

        #Save polygons in dataframes
        self.main_state_poly=df[df.name == main_state].geometry.values[0]
        self.main_state_neighb = df[df.name == main_state].NEIGHBORS.values[0]
        self.main_state_neighb = self.main_state_neighb.split(',') 
        self.nb_states_df=df[df['name'].isin(self.main_state_neighb)]
        
        #Regions out are out of main state
        self.regions_out = self.nb_states_df['name'].to_dict() 
        states = list(self.regions_out.values())
        self.nreg_out = len(states)
        self.nbstates = {states[key]:self.nreg_in+key for key in range(self.nreg_out) }
        self.regions_out = {self.nreg_in+key:states[key] for key in range(self.nreg_out) }
        
        #Join in and out regions
        self.regions = {**self.regions_in, **self.regions_out}
        self.nregions = len(self.regions)

        #Add extra region for all the rest (ocean or other places)
        #self.regions[str(self.nregions)] = 'NAN'
        #self.nregions = self.nregions + 1

        print("Defined the following regions for the network:")
        print(self.regions)
        
    def SP_all_mun(self):
        
        #Get map shapes
        if os.path.exists('maps/sp_municipios/35MUE250GC_SIR_mdyn.shp'):
            df = gpd.read_file('maps/sp_municipios/35MUE250GC_SIR_mdyn.shp')
            print("  Modified shape file loaded.")

        else: #Build map structure with neighbours
            df = gpd.read_file('maps/sp_municipios/35MUE250GC_SIR.shp')
            print("  Loaded basic shape file for municipalities...", end = '')

            df["lonc"] = None #long centroid
            df["latc"] = None #lat centroid
            for index, mun in df.iterrows():   
                df.at[index, "lonc"] = mun.geometry.centroid.x
                df.at[index, "latc"] = mun.geometry.centroid.y

            df["latc"]=df["latc"].astype(float)
            df["lonc"]=df["lonc"].astype(float)

            #Save modified shape file for future use
            print("Done. Saving modified municipality shape structure for future use")
            df.to_file('maps/sp_municipios/35MUE250GC_SIR_mdyn.shp')
        #print(df)

        #Indexes columns
        idx = np.arange(len(df))
        df["idx"]=idx.astype(int)    

        self.regions_in = df["NM_MUNICIP"].to_dict()
        
        self.regions_in_latlon = df.filter(["NM_MUNICIP", "latc", "lonc", "idx"]).set_index('NM_MUNICIP').T.to_dict('list') 
        
        self.df_mun = df


    def SP_large_cities(self):
        #https://pt.wikipedia.org/wiki/Demografia_de_S%C3%A3o_Paulo
                
        self.regions_in_latlon = { #Lat, lon, index of region
            "São Paulo":[-23.32, -46.38, 0],  #Guarulhos, SBC, SAndre, Osasco, Maua, diadema
            "Campinas": [-22.54, -47.03, 1],  
            "Jundiaí":[-23.11, -46.53, 2],  
            "São José dos Campos":[-23.10, -45.53, 3],  
            "Santos":[-23.95, -46.33, 4],
            "Sorocaba":[-23.30, -47.27, 5],  
            "Piracicaba":[-22.43, -47.38, 6],
            "Ribeirão Preto":[-21.10, -47.48, 7],  
            "Franca":[-20.54, -47.40, 8],
            "S. J. do Rio Preto":[-20.49, -49.22, 9]
        }

        self.regions_in = { #index of regions in main state
            0:"GrandeSP",
            1:"Campinas",
            2:"Jundiai",
            3:"SJC",
            4:"Santos",
            5:"Sorocaba",  
            6:"Pira",
            7:"RP",  
            8:"Franca",
            9:"SJRP"                
        }
        
        self.regions_pop = np.array([ #index of regions in main state
            21571281,3224443,804936,2528345,1848654,2120095,1481652,1702479, 657753,456245
        ])

        self.regions_pop_freq = self.regions_pop/np.sum(self.regions_pop)
        #print(self.regions_pop_freq)

    def state_structure(self):

        self.state_dict={
            "RONDÔNIA":"RO",
            "ACRE":"AC",
            "AMAZONAS":"AM",
            "RORAIMA":"RR",
            "PARÁ":"PA",
            "AMAPÁ":"AP",
            "TOCANTINS":"TO",
            "MARANHÃO":"MA",
            "PIAUÍ":"PI",
            "CEARÁ":"CE",
            "RIO GRANDE DO NORTE":"RN",
            "PARAÍBA":"PB",
            "PERNAMBUCO":"PE",
            "ALAGOAS":"AL",
            "SERGIPE":"SE",
            "BAHIA":"BA",
            "MINAS GERAIS":"MG",
            "ESPIRITO SANTO":"ES",
            "RIO DE JANEIRO":"RJ",
            "SÃO PAULO":"SP",
            "PARANÁ":"PR",
            "SANTA CATARINA":"SC",
            "RIO GRANDE DO SUL":"RS",
            "MATO GROSSO DO SUL":"MS",
            "MATO GROSSO":"MT",
            "GOIÁS":"GO",
            "DISTRITO FEDERAL":"DF"
        }

        #Get map shapes
        if os.path.exists('maps/UFEBRASIL/UFEBRASIL_mdyn.shp'):
            df = gpd.read_file('maps/UFEBRASIL/UFEBRASIL_mdyn.shp')
            print("  Modified shape file loaded.")

        else: #Build map structure with neighbours
            df = gpd.read_file('maps/UFEBRASIL/UFEBRASIL.shp')
            print("  Loaded basic shape file - creating neighbour structure...", end = '')

            df["name"] = None #add abreviated name column
            for index, state in df.iterrows():   
                df.at[index, "name"] = self.state_dict.get(state.NM_ESTADO, state.NM_ESTADO)

            df["NEIGHBORS"] = None  # add NEIGHBORS column
            for index, state in df.iterrows():   
                # get 'not disjoint' countries
                neighbors = df[~df.geometry.disjoint(state.geometry)].name.tolist()
                
                # remove own name from the list
                neighbors = [ name for name in neighbors if state['name'] != name ]
                
                # add names of neighbors as NEIGHBORS value
                df.at[index, "NEIGHBORS"] = ",".join(neighbors)
            
            #Save modified shape file for future use
            print("Done. Saving modified shape structure for future use")
            df.to_file('maps/UFEBRASIL/UFEBRASIL_mdyn.shp')

        return df

    def get_closest_region(self, lat, lon):
        dist=1000000.0
        
        p = Point(lon, lat)
        inmainstate = p.within(self.main_state_poly)
        
        ireg=-1
        if inmainstate:
            if not self.all_mun:
                #loop over dictionary - slow
                for reg in self.regions_in_latlon:
                    lat_tmp = self.regions_in_latlon[reg][0]
                    lon_tmp = self.regions_in_latlon[reg][1]
                    dist_tmp = distance([lon], [lat], [lon_tmp], [lat_tmp])
                    if dist_tmp < dist: 
                        dist=dist_tmp
                        ireg=self.regions_in_latlon[reg][2]

            else: #loop over dataframe arrays and geometry
                
                lat_tmp=self.df_mun["latc"].to_numpy()
                lon_tmp=self.df_mun["lonc"].to_numpy()
                
                latv = np.full(len(lat_tmp), lat)
                lonv = np.full(len(lon_tmp), lon)

                dist_tmp = distance(lonv, latv, lon_tmp, lat_tmp)

                k = 10 # k nearest neighbours
                inearreg = np.argpartition(dist_tmp, k)
                
                #Use geometry to check if point in municipality
                for ireg in inearreg:
                    mun_geometry = self.df_mun.loc[ireg, "geometry"]
                    in_region=p.within(mun_geometry)
                    if in_region:
                        return ireg
            
        else:
            #check if in neighbour states
            for index, state in self.nb_states_df.iterrows(): 
                st_name=state['name']
                innbstate=p.within(state.geometry)
                if innbstate:
                    ireg = self.nbstates.get(st_name, self.nregions)
        
        return ireg

    #Build regions grid
    def network_grid(self, dom):
        
        #Region grid is composed of data points, cell centres
        self.region_grid=np.zeros((dom.nlat+1, dom.nlon+1)).astype(int)

        print("Building grid network...")
        
        self.gridname = 'maps/regions_'+self.main_state+"_lats"+str(dom.minlats)+\
            "_"+str(dom.maxlats)+"_lons"+str(dom.minlons)+"_"+str(dom.maxlons)+\
            "_dlat"+str(dom.dlat)+"_dlon"+str(dom.dlon)

        #check if network pre built
        if os.path.exists(self.gridname+".npy"):
            self.region_grid=np.load(self.gridname+".npy")
            print("Regions loaded from file "+self.gridname)
            #print(self.region_grid.shape)
        else:
            #Grid is based on cell centers
            for i, lat in enumerate(tqdm.tqdm(dom.lat_bins_c)):
                for j, lon in enumerate(dom.lon_bins_c):
                    #print(lat, lon)
                    reg0 = self.get_closest_region(lat, lon) #This is the center of cell
                    if reg0 != -1:
                        self.region_grid[i,j]=reg0
                    else:
                        #Check if part of the cell is in a region, otherwise it is ocean
                        reg1 = self.get_closest_region(lat-dom.dlat, lon-dom.dlon)
                        if reg1 != -1:
                            ireg=reg1
                        else:
                            reg2 = self.get_closest_region(lat+dom.dlat, lon-dom.dlon)
                            if reg2 != -1:
                                ireg=reg2
                            else:
                                reg3 = self.get_closest_region(lat-dom.dlat, lon+dom.dlon)
                                if reg3 != -1:
                                    ireg=reg3
                                else:
                                    reg4 = self.get_closest_region(lat+dom.dlat, lon+dom.dlon)
                                    if reg4 != -1:
                                        ireg=reg4
                                    else:
                                        ireg = -1
                        
                        self.region_grid[i,j] = ireg

            #Save this region grid for furute use, as it takes time to build
            np.savetxt(self.gridname+".csv", self.region_grid)
            np.save(self.gridname, self.region_grid)
            print("Regions saved in file "+self.gridname)


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

    def add_reg_to_daydf(self, dom, daydata):
        
        #Add column with region tag
        #for each event
        for i in range(2):
            s=str(i)
            lon=daydata.df['lng'+s].values
            lat=daydata.df['lat'+s].values
            lonnan=np.isnan(lon)
            latnan=np.isnan(lat)
            nan = lonnan*latnan
            ilon=((lon[~nan]-dom.minlons)/dom.dlon).astype(int)
            ilat=((lat[~nan]-dom.minlats)/dom.dlat).astype(int)
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
        print("")
        print("Transition Matrix (number of people moving to/from regions)")
        print(table)

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
        print()
        print("Transition matrix including steady users")
        matprint(mat)
        print()
        #Normalize
        mat_normed = mat / mat.sum(axis=0)
        print("Normalized transition matrix (transition probability)")
        matprint(mat_normed)
        
        return mat_normed