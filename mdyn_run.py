#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import time
import sys
import os
import warnings
warnings.filterwarnings("ignore")

import numpy as np
import statistics

from mdynpy.mdyn_network import Network
from mdynpy.mdyn_main import MobileDynamics
from mdynpy.mdyn_map import Map
import mdynpy.mdyn_extras as mex
import mdynpy.mdyn_move_mats as mmat 
import mdynpy.mdyn_isol_index as isol
import mdynpy.mdyn_fixed_users as fx
import mdynpy.mdyn_seir as seir



#Input parameters
#-----------------------------
ipar, run_opt = mex.get_input(sys.argv)


ipar.load_network = True



#Initialize network
#-----------------------------
time_start = time.time()
network = Network(ipar)
time_end = time.time()
print("Execution time "+str(time_end-time_start)+" seconds")
        
#Initialize Data
#-----------------------------
if run_opt < 30:
    time_start = time.time()
    mdyn = MobileDynamics(ipar) 
    time_end = time.time()
    print("Execution time "+str(time_end-time_start)+" seconds")

#Build model = generate movement model
if run_opt == 0:
    time_start = time.time()
    mdyn.build_model(network)
    time_end = time.time()
    print("Execution time "+str(time_end-time_start)+" seconds")


if run_opt == 1:
    time_start = time.time()
    mmat.analyse_move_mats(mdyn, network, ipar)
    time_end = time.time()
    print("Execution time "+str(time_end-time_start)+" seconds")

if run_opt == 2:
    time_start = time.time()
    mmat.analyse_move_mats_dow(mdyn, network, ipar)
    time_end = time.time()
    print("Execution time "+str(time_end-time_start)+" seconds")

if run_opt == 20:
    time_start = time.time()
    mmat.simulate_move_mats(mdyn, network, ipar)
    time_end = time.time()
    print("Execution time "+str(time_end-time_start)+" seconds")

if run_opt == 21:
    time_start = time.time()
    mmat.simulate_model(mdyn, network, ipar)
    time_end = time.time()
    print("Execution time "+str(time_end-time_start)+" seconds")

if run_opt == 22:
    time_start = time.time()
    seir.simulate_seir_model(mdyn, network, ipar)
    time_end = time.time()
    print("Execution time "+str(time_end-time_start)+" seconds")

if run_opt == 30:
    time_start = time.time()
    isol.isol_index(network, ipar)
    time_end = time.time()
    print("Execution time "+str(time_end-time_start)+" seconds")

if run_opt == 31:
    time_start = time.time()
    fx.fixed_users_by_date(network, ipar)
    time_end = time.time()
    print("Execution time "+str(time_end-time_start)+" seconds")
    
