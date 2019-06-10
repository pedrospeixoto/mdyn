#! /usr/bin/env python3
import sys
import os
import numpy as np
import warnings
import time

import matplotlib.pyplot as plt

from mobile_dynamics import MobileDynamics

warnings.filterwarnings("ignore")


#Initialize mobile data and load data to dataframe 
mdyn=MobileDynamics(sys.argv)


#Analyse data
for i in range(3):
    s=str(i)
    mdyn.map_density_data(mdyn.data[0].df['lng'+s].values, mdyn.data[0].df['lat'+s].values, 
        mdyn.date_ini+" event "+str(i))
    #plt.show()
    #time.sleep(5)
