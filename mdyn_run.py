#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys

import os
import warnings
warnings.filterwarnings("ignore")

from mdyn_network import Network
from mdyn_main import MobileDynamics
from mdyn_map import Map
import mdyn_extras as mex

#Input parameters
#-----------------------------
ipar, run_opt = mex.get_input(sys.argv)

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

#Initialize Data
#-----------------------------
mdyn = MobileDynamics(
    data_dir = ipar.data_dir,
    date_ini = ipar.date_ini,
    date_end = ipar.date_end,
    load = ipar.load_data
    )


#Build model
if run_opt == 0:
    mdyn.build_model(network)

mdyn.collect_move_mat(network.domain, network.subdomains)

#Loop work with transitions matrices
for i, day in enumerate(mdyn.days_all):
    print(i, day)
    
    mat = mdyn.movemats_norm[i]
    reg0 = mdyn.movemats_reg0[i]
    reg1 = mdyn.movemats_reg1[i]
    
    title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+day.strftime("%Y-%m-%d")
    for j in reg0:
        title = title_base+"_origin_"+mdyn.movemats_reg_names[i][j]
        print("Creating plot for ", title)
        filename = mdyn.dump_dir+title
        map=Map(network)
        map.map_move_by_reg(mat, j, reg1, network, title, filename)
    #mex.matprint(mdyn.movemats_norm[i])




