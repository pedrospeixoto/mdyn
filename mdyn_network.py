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
            
            self.regions = { #Lat, lon, indice da regiao
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
                    neighbors = [ name for name in neighbors if state.name != name ]
                    # add names of neighbors as NEIGHBORS value
                    df.at[index, "NEIGHBORS"] = ",".join(neighbors)
                
                #Save modified shape file for future use
                print("Done. Saving modified shape structure for future use")
                df.to_file('maps/UFEBRASIL_mdyn.shp')

            self.main_state_poly=df[df.name == main_state].geometry.values[0]
            self.main_state_neighb = df[df.name == main_state].NEIGHBORS.values[0]
            self.main_state_neighb = self.main_state_neighb.split(',') 
            self.nb_states=df[df['name'].isin(self.main_state_neighb)]

            self.regions_out = self.nb_states['name'].to_dict() 
 

    def get_closest_region(self, lat, lon):
        dist=1000000.0
        p=Point(lon, lat)
        inmainstate=p.within(self.main_state_poly)
        ireg=np.nan
        if inmainstate:
            for reg in self.regions:
                lat_tmp=self.regions[reg][0]
                lon_tmp=self.regions[reg][1]
                dist_tmp=distance([lon], [lat], [lon_tmp], [lat_tmp])
                if dist_tmp < dist: 
                    dist=dist_tmp
                    ireg=self.regions[reg][2]
        else:
            #check if in neighbour states
            for index, state in self.nb_states.iterrows(): 
                innbstate=p.within(state.geometry)
                if innbstate:
                    ireg=(-1)*index

        return ireg

    #Build city network
    def network_grid(self, dom):
        
        self.region_grid=np.zeros((dom.nlat+1, dom.nlon+1))    

        print("Building grid network...")
        for i, lat in enumerate(tqdm.tqdm(dom.lat_bins)):
            for j, lon in enumerate(dom.lon_bins):
                self.region_grid[i,j]=self.get_closest_region(lat, lon)
                
        