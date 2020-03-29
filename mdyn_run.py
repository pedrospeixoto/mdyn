#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
import warnings
warnings.filterwarnings("ignore")

import numpy as np

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

movemat_avg = np.zeros(mdyn.movemats[0].shape)

#Loop work with transitions matrices and average then
for i, day in enumerate(mdyn.days_all):
    print("Load: ", i, day)
    
    mat = mdyn.movemats[i]
    movemat_avg = movemat_avg + mat
    #mex.matprint(mdyn.movemats_norm[i])

#Get primary and secondary sources
movemat_avg_diag = np.diag(movemat_avg)
movemat_avg_diag = movemat_avg_diag[0:network.nreg_in]
#print(movemat_avg_diag)
movemat_avg = movemat_avg / movemat_avg.sum(axis=0)
prim_source = np.argmax(movemat_avg_diag)
#print(prim_source)
num_source = 12
#print(movemat_avg[0:network.nreg_in, prim_source], np.sum(movemat_avg[:, prim_source]))
sources = np.argpartition(movemat_avg[0:network.nreg_in, prim_source], -num_source)[-num_source:]
sources2 = np.argpartition(movemat_avg_diag[0:network.nreg_in], -num_source)[-num_source:]
sources = np.unique(np.concatenate((sources, sources2), axis=0) )

title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end
print(network.regions)
#Plot main sources
for j in sources:
    title = title_base+"_origin_"+network.regions[j]
    print("Creating plot for ", title)
    move_vec = movemat_avg[:, j]
    sumv = np.sum(move_vec)
    if abs(sumv-1.0)>0.001:
        print("Warning: Probability with sum not 1.", sumv)
        
    map=Map(network)
    map.map_move_by_reg(move_vec, network.regions, network, title, mdyn.dump_dir+title)



