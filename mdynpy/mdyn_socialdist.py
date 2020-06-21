#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
import numpy as np
import pandas as pd
import math
import geopandas as gpd

from shapely.geometry import Point, Polygon

import time
from datetime import datetime
from datetime import date
from datetime import timedelta

import geopy.distance
import difflib 
import tqdm as tqdm

import mdynpy.mdyn_extras as mex

#Garbage collection
import gc

class socialdist:
    #social distancing class
     def __init__(self, filename, network):
        print("Fetching social distancing data...")
        last_date = filename[-14:-4]
        df = pd.read_csv(filename)
        #print(df)
        
        #Check states names
        df['state_abrv'] = df['state_name'].map(mex.state_name2abrv)

        #filter state
        #if network.domain_abrv != "BRA":
        #Try to filter state, if possible
        dftmp = df[df['state_abrv']==network.domain_abrv]
        
        if not dftmp.empty:
            #failed to filter state, keep full dataframe
            df = dftmp
            
        #Check city names
        fix_municip_name = {
            "BOCAIUVA": "BOCAIÚVA",
            "AMPARO DA SERRA" : 'AMPARO DO SERRA',
            "SANTANA DO LIVRAMENTO" : "SANT'ANA DO LIVRAMENTO",
            "FLORÍNIA" : "FLORÍNEA",
            "ITAÓCA" : 'ITAOCA',
            "SÃO TOMÉ DAS LETRAS" : 'SÃO THOMÉ DAS LETRAS',
            "SÃO JOÃO DEL-REI" : 'SÃO JOÃO DEL REI',
            "GUATAMBU" : "GUATAMBÚ",
            "SÃO CRISTOVÃO DO SUL" : 'SÃO CRISTÓVÃO DO SUL',
            "BIRITIBA-MIRIM" : 'BIRITIBA MIRIM',
            "ÁGUAS CLARAS" : 'ÁGUA CLARA',
            "BRAZLÂNDIA" : 'BRASILÂNDIA',
            "MUQUÉM DE SÃO FRANCISCO" : 'MUQUÉM DO SÃO FRANCISCO',
            "LUÍS ALVES" : 'LUIZ ALVES',
            "PINGO-D'ÁGUA" : "PINGO D'ÁGUA",
            "RESTINGA SECA" : 'RESTINGA SÊCA',
            "LAURO MULLER" : 'LAURO MÜLLER',
            "ITAETÊ" : 'ITAETÉ',
            "SAMAMBAIA" : "SAMBAÍBA",
            "ITAPOÃ" : "ITAPOÁ",
            "IUIÚ": "IUIU",
            "POMPEIA" : 'POMPÉIA',
            "DIAMANTINA‎" : 'DIAMANTINA',
            "BOCAIUVA DO SUL" : 'BOCAIÚVA DO SUL',
            "ERERÉ":  'ERERÊ',
            "ALVORADA DO GURGUEIA" : 'ALVORADA DO GURGUÉIA',
            "SÃO JOÃO DO CARU" : 'SÃO JOÃO DO CARÚ',
            "SÃO MIGUEL DO PASSA-QUATRO" : 'SÃO MIGUEL DO PASSA QUATRO',
            "REDENÇÃO DO GURGUEIA" : 'REDENÇÃO DO GURGUÉIA',
            "MATUREIA" :  'MATURÉIA',
            "LINDOIA" : 'LINDÓIA',
            "TAUBATÉ‎" : "TAUBATÉ",
            "MAJOR IZIDORO" : 'MAJOR ISIDORO',
            "GALILEIA" : 'GALILÉIA',
            "LAGEDO DO TABOCAL" : 'LAJEDO DO TABOCAL',
            "OLHO-D'ÁGUA DO BORGES" : "OLHO D'ÁGUA DO BORGES",
            "RUBINEIA" : 'RUBINÉIA',
            "ITAPECURU-MIRIM" : 'ITAPECURU MIRIM',
            "BORACEIA" : 'BORACÉIA',
            "SANTA LUZIA DO ITANHI" : 'SANTA LUZIA DO ITANHY',
            "LUIZ ANTÔNIO" : 'LUÍS ANTÔNIO',
            "PASSA-E-FICA" :  'PASSA E FICA',
            "CONCEIÇÃO DO LAGO AÇU" :  'CONCEIÇÃO DO LAGO-AÇU',
            "ATÍLIO VIVÁCQUA" : 'ATÍLIO VIVACQUA',
            "GRÃO-PARÁ" : 'GRÃO PARÁ',
            "PATOS" : 'PATOS',
            "COLÔNIA DO GURGUEIA" : 'COLÔNIA DO GURGUÉIA',
            "SANTO ANTÔNIO DE LEVERGER" : 'SANTO ANTÔNIO DO LEVERGER',
            "GRACCHO CARDOSO" : 'GRACHO CARDOSO',
            "TAIUVA" : 'TAIÚVA',
            "IPUIUNA" : 'IPUIÚNA',
            "ELDORADO DOS CARAJÁS" : 'ELDORADO DO CARAJÁS',
            "MUNHOZ DE MELLO" : 'MUNHOZ DE MELO',
            "COITÉ DO NOIA" : 'COITÉ DO NÓIA',
            "SÃO GONÇALO DO GURGUEIA" : 'SÃO GONÇALO DO GURGUÉIA',
            "SÃO BENTO DO TRAIRI" : 'SÃO BENTO DO TRAIRÍ',
            "SÃO CAETANO" : 'SÃO CAITANO'
        }

        df['city_name']=df['city_name'].str.upper()
        df['city_name'] = df['city_name'].replace(fix_municip_name)

        df=df.rename(columns={"isolated": "iso", "dt": "day", "city_name":"reg_name"})
        df['reg_state'] = df.apply (lambda row: row.reg_name + "_"+row.state_abrv, axis=1)

        #filter probelmatic cities
        ndates=len(df['day'].unique())
        cities = df['reg_name'].unique()
        df_cities=df['reg_name'].value_counts().to_frame().reset_index().rename(columns={'index':'name', 'reg_name':'count'})
        #print(df_cities)

        print("Filtering buggy cities from isolation index...")
        for city in cities:
            ncity = df_cities.loc[df_cities['name'] == city, 'count'].values[0] 
            if ncity < ndates-40 :
                #print("Removing city:", city, ncity)
                df = df[df['reg_name'] != city]      
        print("...done.")
        
        self.df = df            

