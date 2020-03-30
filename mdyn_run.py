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


#Build model = generated movement model
if run_opt == 0:
    mdyn.build_model(network)

#Analyse movement matrices
mdyn.collect_move_mat(network.domain, network.subdomains)

#Period mean move mat
movemat_avg = np.zeros(mdyn.movemats[0].shape)

#Loop work with transitions matrices and average then
for i, day in enumerate(mdyn.days_all):
    print("Load: ", i, day)
    
    title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+day.strftime("%Y-%m-%d")
    mex.plot_matrix(mdyn.movemats_norm[i], title_base+"\nDay_Prob_Move", mdyn.dump_dir)
    movemat_avg = movemat_avg + mdyn.movemats[i]
    #mex.matprint(mdyn.movemats_norm[i])

#Get primary and secondary sources
movemat_avg_diag = np.diag(movemat_avg)
movemat_avg_diag = movemat_avg_diag[0:network.nreg_in]
#print(movemat_avg_diag)
movemat_avg = movemat_avg / movemat_avg.sum(axis=0)

title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end
mex.plot_matrix(movemat_avg, title_base+"\nMean_Prob", mdyn.dump_dir)

movemat_std = np.std(mdyn.movemats_norm, axis=0)
mex.plot_matrix(movemat_std, title_base+"\nStd_Dev_of_Prob", mdyn.dump_dir)

#Sources to plot
num_source = ipar.num_source #min(network.nregions-2, 20)
#Primary source 
prim_source = np.argmax(movemat_avg_diag)
#First hits by primary source
sources = np.argpartition(movemat_avg[0:network.nreg_in, prim_source], -num_source)[-num_source:]
#Large centers (secondary sources)
sources2 = np.argpartition(movemat_avg_diag[0:network.nreg_in], -num_source)[-num_source:]
#Join all main sources
sources = np.unique(np.concatenate((sources, sources2), axis=0) )


print("Regions:", network.regions)
print("Main regions:", sources)
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



