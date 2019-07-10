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


import geopy.distance
import geopandas as gpd

from shapely.geometry import Point, Polygon


import tqdm 

from mdyn_daydata import DayData
from mdyn_extras import distance, distance_lat, distance_lon, daterange


class Network:

    cities = {}

    def __init__(self, main_state):
        print("Creating/Loading network structure")
        self.main_state=main_state
        self.load_state_data(main_state)

    def load_state_data(self, main_state):

        #State = string with Brazilian state abbrev
        # n = number of cities to include
        if main_state=="SP": 
        #Source https://g1.globo.com/sp/sao-paulo/noticia/2018/08/29/cidade-de-sao-paulo-tem-122-milhoes-de- -e-e-a-mais-populosa-do-pais.ghtml
            self.city_pop={
                "São Paulo":12176866,  
                "Guarulhos":1365899,  
                "Campinas": 1194094,  
                "São Bernardo do Campo": 833240,
                "Santo André": 716109,
                "São José dos Campos":713943,
                "Osasco":696850,  
                "Ribeirão Preto":694.534,
                "Sorocaba":671186,  
                "Mauá":468.168,  
                "São José do Rio Preto":456.245,
                "Mogi das Cruzes":440.769,  
                "Diadema":420.934,  
                "Jundiaí":414.810,
                "Piracicaba":400.949
            }
            
            self.city_latlon={
                "São Paulo":[-23.32, -46.38],  
                "Guarulhos":[-23.27, -46.32],  
                "Campinas": [-22.54, -47.03],  
                "São Bernardo do Campo": [-23.41, -46.33],  
                "Santo André": [-23.39, -46.32],  
                "São José dos Campos":[-23.10, -45.53],  
                "Osasco":[-23.31, -46.47],  
                "Ribeirão Preto":[-21.10, -47.48],  
                "Sorocaba":[-23.30, -47.27],  
                "Mauá":[-23.40, -46.27],  
                "São José do Rio Preto":[-20.49, -49.22],  
                "Mogi das Cruzes":[-23.31, -46.11],  
                "Diadema":[-23.41, -46.37],  
                "Jundiaí":[-23.11, -46.53],  
                "Piracicaba":[-22.43, -47.38]
            }
            
            self.regions_in_latlon = { #Lat, lon, index of region
                "Grande São Paulo":[-23.32, -46.38, 0],  #Guarulhos, SBC, SAndre, Osasco, Maua, diadema
                "Campinas": [-22.54, -47.03, 1],  
                "São José dos Campos":[-23.10, -45.53, 2],  
                "Ribeirão Preto":[-21.10, -47.48, 3],  
                "Sorocaba":[-23.30, -47.27, 4],  
                "São José do Rio Preto":[-20.49, -49.22, 5],  
                "Piracicaba":[-22.43, -47.38, 6]
            }

            self.regions_in = { #index of regions in main state
                0:"GrandeSP",
                1:"Campinas",
                2:"SJC",  
                3:"RP",  
                4:"Sorocaba",  
                5:"SJRP",  
                6:"Pira"
            }
            
            self.nreg_in = len(self.regions_in)

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
            if os.path.exists('maps/UFEBRASIL_mdyn.shp'):
                df = gpd.read_file('maps/UFEBRASIL_mdyn.shp')
                print("  Modified shape file loaded.")

            else: #Build map structure with neighbours
                df = gpd.read_file('maps/UFEBRASIL.shp')
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
                df.to_file('maps/UFEBRASIL_mdyn.shp')
            
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
            

    def get_closest_region(self, lat, lon):
        dist=1000000.0
        
        p = Point(lon, lat)
        inmainstate = p.within(self.main_state_poly)
        ireg=-1
        if inmainstate:
            for reg in self.regions_in_latlon:
                lat_tmp = self.regions_in_latlon[reg][0]
                lon_tmp = self.regions_in_latlon[reg][1]
                dist_tmp = distance([lon], [lat], [lon_tmp], [lat_tmp])
                if dist_tmp < dist: 
                    dist=dist_tmp
                    ireg=self.regions_in_latlon[reg][2]
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
        
        gridname = 'maps/regions_'+self.main_state+"_lats"+str(dom.minlats)+\
            "_"+str(dom.maxlats)+"_lons"+str(dom.minlons)+"_"+str(dom.maxlons)+\
            "_dlat"+str(dom.dlat)+"_dlon"+str(dom.dlon)

        #check if network pre built
        if os.path.exists(gridname+".npy"):
            self.region_grid=np.load(gridname+".npy")
            print("Regions loaded from file "+gridname)
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

                #Save this region grid for turute use, as it takes time to build
            np.save(gridname, self.region_grid)
            print("Regions saved in file "+gridname)


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

    def calc_transition_matrix(self, df):
        #print(df)
        #df.to_csv("tmp.csv", header=True)
        table = df.pivot_table(values='dt1', index=['reg1'],\
                     columns=['reg0'], aggfunc=np.count_nonzero, fill_value=0, dropna=False)
        
        #remove the problematic -1 regions
        table = table.div(table.sum(axis=0), axis=1)
        try:
            table = table.drop(columns=[-1])
        except:
            pass
        try:
            table = table.drop([-1], axis=0)
        except:
            pass

        
        print(table)
        #We might regions without data
        #print(table)
        #print(table.columns)
        #reglist0=table.columns.to_list()
        #print(reglist0)
        #n=len(table)
        #print(n)
        #missing = [i for i in range(-1, self.nregions, 1) if i not in reglist0] 
        #Add missing columns
        #print(missing)
        #for i in missing:
        #    table.insert(i, str(i), np.zeros(n).astype(int), True) 

        #print(table)
        mat = table.as_matrix(columns=None)
        #print(mat)
        #print(mat.shape)
        
        return mat