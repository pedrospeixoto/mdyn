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

import tqdm as tqdm

import mdyn_extras as mex

#Garbage collection
import gc

#Input parameters - dir name
#-----------------------------
filename = sys.argv[1]

#Load data
base_name = "data"

df = pd.read_csv(filename)
states=df['state_name'].unique()

print(df)
print(states)

for state in states:
    df_tmp=df[df['state_name']==state]
    #df_tmp=df_tmp.drop(['state_name'], axis=1)
    filename="dump/IIS/"+state.upper()+"_Municipios_2020-02-01_2020-04-20_iso_index.csv"
    df_tmp.to_csv(filename)
    