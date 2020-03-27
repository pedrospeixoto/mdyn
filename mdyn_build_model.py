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
domain = "SÃO PAULO" #SP or RJ
domain_gran = "NM_ESTADO"
domain_shape = "maps/UFEBRASIL/UFEBRASIL"

#Subdomain definitions
subdomains = "SP-Municipios"
subdomains_gran = "NM_MUNICIP"
subdomains_shape = "maps/sp_municipios/35MUE250GC_SIR"

#Network granularity
latlon_gran = 0.5 #granularity of lat long spacing

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
data_dir = "data/visit_journey_sao_paulo_2019/"
date_ini = "2019-12-01"
date_end = "2019-12-01"
load_data = True 

mdyn = MobileDynamics(
    data_dir = data_dir,
    date_ini = date_ini,
    date_end = date_end,
    load = load_data
    )

#Build model
mdyn.build_model(network)


