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
from windrose import WindroseAxes
from shapely.geometry import Point, Polygon

import tqdm 

from mdyn_daydata import DayData
from mdyn_extras import *

class Network():
    cities = {}
    def __init__(self, state):
        self.load_state_data(state)



    def load_state_data(self, state):

        #State = string with Brazilian state abbrev
        # n = number of cities to include
        if state=="SP": 
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

            #Get state shape polygon
            self.state_shape(state)

    def get_closest_region(self, lat, lon):
        dist=1000000.0
        regname=""
        for reg in self.regions:
            lat_tmp=self.regions[reg][0]
            lon_tmp=self.regions[reg][1]
            dist_tmp=distance([lon], [lat], [lon_tmp], [lat_tmp])
            if dist_tmp < dist : 
                regname=reg
                dist=dist_tmp
        
        return regname

    def state_shape(self, state):
        #geo_df = gpd.read_file('../Maps/SP-MUN/35MUE250GC_SIR.shp')
        geo_df = gpd.read_file('maps/UFEBRASIL.shp')
        #fig,ax =plt.subplots(figsize =(10,10))
        #print(geo_df.head())
        if state == 'SP':
            self.state_poly=geo_df[geo_df.NM_ESTADO == 'SÃO PAULO'].geometry.values[0]
        else:
            self.state_poly=geo_df[geo_df.NM_ESTADO == state].geometry.values[0]
        
        #self.centroid=self.state_poly.centroid
        #print(self.centroid.within(self.state_poly))
        #print(p1.within(poly))
        #geo_df.plot(column='NM_ESTADO',  cmap='Set3', ax=ax, edgecolor='grey')
        #plt.show()
    
    #Build city network
    def network_grid(self, lat_bins, lon_bins):
        nlat=len(lat_bins)
        nlon=len(lon_bins)
        self.region_grid=np.zeros((nlat+1, nlon+1))       
        
        for i, lat in enumerate(lat_bins):
            for j, lon in enumerate(lon_bins):
                p=Point(lon, lat)
                instate=p.within(self.state_poly)
                if instate:
                    reg=self.get_closest_region(lat, lon)
                    self.region_grid[i,j]=self.regions[reg][2]
                else:
                    self.region_grid[i,j]=np.nan
    
        