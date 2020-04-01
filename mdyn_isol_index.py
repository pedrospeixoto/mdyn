import sys
import os


import statistics
import numpy as np
import pandas as pd
import math

from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.colors as colors
import matplotlib.cm as cm

import tqdm as tqdm


from mdyn_map import Map
import mdyn_extras as mex

def isol_index(network, ipar):
    
    data_dir = ipar.data_dir 

    #Load data
    base_name = network.domain+"_"+network.subdomains
    df = mex.read_orc2df(data_dir, base_name, True)
    
    df = org_data(df, network, data_dir)

    table = df.pivot_table(values='total_users', index=['lat'],\
                     columns=['lon'], aggfunc=np.average, fill_value=0, dropna=False)

    print(table)
    #table.plot(figsize=(16, 9), title='Population');
    #plt.show()

    #print(table.loc['-19.93', ])
    map = Map(network)                
    map.map_lat_lon_z_data(df['lat'].values, df['lon'].values, df['IsoIndex'].values)


def org_data(dflocal, network, dir):
        #Organize data into nice dataframe

        n=len(dflocal)
        
        pklfile = dir+network.domain+"_"+network.subdomains+"_data_proc.pkl"
        
        if not os.path.exists(pklfile):
            #Convert lat lon from dict to numpy array
            loc = dflocal.location
            lat = np.array([list(iloc.values())[0] for iloc in loc])
            lon = np.array([list(iloc.values())[1] for iloc in loc])
            dflocal['lat']=lat
            dflocal['lon']=lon
            
            #add regions
            lonnan=np.isnan(lon)
            latnan=np.isnan(lat)
            nan = lonnan*latnan
            ilon=((lon[~nan]- network.minlons)/network.dlon).astype(int)
            ilat=((lat[~nan]- network.minlats)/network.dlat).astype(int)
            
            reg = np.zeros(n).astype(int)
            reg[nan] = -1
            reg[~nan]=network.region_grid[ilat, ilon]
            dflocal['reg']=reg

            #Calc isolation index
            dflocal['IsoIndex'] = dflocal['fixed_users']/dflocal['total_users']


            filename = dir+network.domain+"_"+network.subdomains+"_data_proc.pkl"
            print("Saving pre-processed data-file for future use:", filename)
            dflocal.to_pickle (filename) 

        return dflocal