#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
import warnings
warnings.filterwarnings("ignore")

import numpy as np
import statistics

from mdyn_network import Network
from mdyn_main import MobileDynamics
from mdyn_map import Map
import mdyn_extras as mex
import mdyn_move_mats as mmat 
import mdyn_isol_index as isol
import mdyn_fixed_users as fx

import pandas as pd

#Input parameters
#-----------------------------
ipar, run_opt = mex.get_input(sys.argv)

ipar.load_network = True

#Get parameters for simulation


#Build network for all states of Brazil:
file_states = "maps/ufebrasil_input_info.csv"
df = pd.read_csv(file_states)


for index, row in df.iterrows():
    #Domain
    ipar.domain = row['domain']
    ipar.domain_abrv = row['abrv']

    #Subdomain definitions
    ipar.subdomains_shape = "maps/ac_municipios/12MUE250GC_SIR" #"maps/"+row['shape']
    
    print()
    print("---------------------------")
    print(ipar.domain, ipar.subdomains, ipar.subdomains_shape)
    print("---------------------------")
    print()



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

    if run_opt == 30:
        isol.isol_index(network, ipar)

    if run_opt == 31:
        fx.fixed_users_by_date(network, ipar)
        
