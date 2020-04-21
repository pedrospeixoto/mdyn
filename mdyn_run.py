#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
import warnings
warnings.filterwarnings("ignore")

import numpy as np
import statistics

from pysrc.mdyn_network import Network
from pysrc.mdyn_main import MobileDynamics
from pysrc.mdyn_map import Map
import pysrc.mdyn_extras as mex
import pysrc.mdyn_move_mats as mmat 
import pysrc.mdyn_isol_index as isol
import pysrc.mdyn_fixed_users as fx
import pysrc.mdyn_seir as seir

#Input parameters
#-----------------------------
ipar, run_opt = mex.get_input(sys.argv)


ipar.load_network = True

#Initialize network
#-----------------------------
network = Network(ipar)
        
#Initialize Data
#-----------------------------
if run_opt < 30:
    mdyn = MobileDynamics(ipar) 


#Build model = generate movement model
if run_opt == 0:
    mdyn.build_model(network)

if run_opt == 1:
    mmat.analyse_move_mats(mdyn, network, ipar)

if run_opt == 2:
    mmat.analyse_move_mats_dow(mdyn, network, ipar)

if run_opt == 20:
    mmat.simulate_move_mats(mdyn, network, ipar)

if run_opt == 21:
    mmat.simulate_model(mdyn, network, ipar)

if run_opt == 22:
    seir.simulate_seir_model(mdyn, network, ipar)

if run_opt == 30:
    isol.isol_index(network, ipar)

if run_opt == 31:
    fx.fixed_users_by_date(network, ipar)
    
