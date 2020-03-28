#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
import warnings
warnings.filterwarnings("ignore")

from mdyn_network import Network
from mdyn_main import MobileDynamics

#Initialize network
#-----------------------------

#Main domain definition
domain = "RIO DE JANEIRO" #SP or RJ
domain_gran = "NM_ESTADO"
domain_shape = "maps/UFEBRASIL/UFEBRASIL"

#Subdomain definitions
subdomains = "RJ-Microregioes"
subdomains_gran = "NM_MICRO"
subdomains_shape = "maps/rj_microrregioes/33MIE250GC_SIR"

#Network granularity
latlon_gran = 0.01 #granularity of lat long spacing

#Network pre-computation flag
load_domain = True #Load precomp structures

network = Network(
            domain = domain, 
            domain_gran = domain_gran,
            domain_shape = domain_shape, 
            subdomains = subdomains, 
            subdomains_gran = subdomains_gran,
            subdomains_shape = subdomains_shape,
            latlon_gran = latlon_gran,
            load = load_domain
            )

#Read data and add to network
#-----------------------------
#Initialize mobile data and load data to dataframe 
data_dir = "data/visit_journey_rio_2019/"
date_ini = "2019-04-01"
date_end = "2020-02-29"
load_data = True 

mdyn = MobileDynamics(
    data_dir = data_dir,
    date_ini = date_ini,
    date_end = date_end,
    load = load_data
    )

#Build model
mdyn.build_model(network)


