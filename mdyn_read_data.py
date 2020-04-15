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
data_dir = sys.argv[1]

#Load data
base_name = "data"
#df = mex.read_pq2df_filter(data_dir, base_name, "01-03-2020", True)

df = mex.read_pq2df(data_dir, base_name, True)

