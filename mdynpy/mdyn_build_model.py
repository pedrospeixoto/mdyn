#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
import warnings
warnings.filterwarnings("ignore")

from mdyn_network import Network
from mdyn_main import MobileDynamics
import mdyn_extras as mex

#Get parameters
if len(sys.argv) < 2:
    print("Please provide a parameter file for mdyn as input parameter")
    sys.exit(1)

#Get parameters for simulation
ipar = mex.getVarFromFile(sys.argv[1])

if len(sys.argv) < 3:
    print("Please provide as second parameter the option to run:")
    print(" 0) Build model")
    print(" 1) Analyse movement")
    sys.exit(1)

run_opt = sys.argv[2]
if run_opt == 0:
    build = True
else:
    build = False

#Initialize network
#-----------------------------

network = Network(
            domain = ipar.domain, 
            domain_gran = ipar.domain_gran,
            domain_shape = ipar.domain_shape, 
            subdomains = ipar.subdomains, 
            subdomains_gran = ipar.subdomains_gran,
            subdomains_shape = ipar.subdomains_shape,
            latlon_gran = ipar.latlon_gran,
            load = ipar.load_domain
            )

#Read data and add to network
#-----------------------------

mdyn = MobileDynamics(
    data_dir = ipar.data_dir,
    date_ini = ipar.date_ini,
    date_end = ipar.date_end,
    load = ipar.load_data
    )


#Build model
if build:
    mdyn.build_model(network)


