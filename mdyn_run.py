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

#Input parameters
#-----------------------------
ipar, run_opt = mex.get_input(sys.argv)


ipar.load_network = True

#Initialize network
#-----------------------------
network = Network(
            domain = ipar.domain, 
            domain_gran = ipar.domain_gran,
            domain_shape = ipar.domain_shape, 
            domain_pop = ipar.domain_pop,
            domain_pop_labels = ipar.domain_pop_labels,
            subdomains = ipar.subdomains, 
            subdomains_gran = ipar.subdomains_gran,
            subdomains_shape = ipar.subdomains_shape,
            subdomains_pop = ipar.subdomains_pop,
            subdomains_pop_labels = ipar.subdomains_pop_labels,
            latlon_gran = ipar.latlon_gran,
            load = ipar.load_network
            )

#Initialize Data
#-----------------------------
if run_opt < 30:
    mdyn = MobileDynamics(
        data_dir = ipar.data_dir,
        date_ini = ipar.date_ini,
        date_end = ipar.date_end,
        dump_dir = ipar.dump_dir,
        load = ipar.load_data
        )


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

