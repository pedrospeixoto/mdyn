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


domain = "SÃO PAULO" #SP or RJ
domain_gran = "NM_ESTADO"
domain_shape = "maps/UFEBRASIL/UFEBRASIL"

subdomains = "SP-Municipios"
subdomains_gran = "NM_MUNICIP"
subdomains_shape = "maps/sp_municipios/35MUE250GC_SIR"

latlon_gran = 0.02 #granularity of lat long spacing

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

sys.exit(1)

#Initialize mobile data and load data to dataframe 
mdyn=MobileDynamics(sys.argv)



mdyn.build_model(state=state, granularity=granularity, precompdomain = precompdomain)

