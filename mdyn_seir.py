
#  SEIR model
#-----------------

import sys
import os

import numpy as np
import statistics
from scipy import stats
import pandas as pd

from datetime import datetime
from datetime import date
from datetime import timedelta

from mdyn_map import Map
import mdyn_extras as mex



def simulate_seir_model(mdyn, network, ipar):

    #Analyse movement matrices
    mdyn.collect_move_mat(network)

    #Calculate forecasted movemats as averages of previous data
    movemat_avg, movemat_std, movemat_avg_diag = calc_move_mat_avg_dow(mdyn, network, ipar)

    #Initial condition
    Iini = np.zeros([network.nregions])
    for key in ipar.data_ini_by_reg:
        Iini[key] = ipar.data_ini_by_reg[key]
    I = np.copy(Iini)
    
    Eini = np.zeros([network.nregions])
    E = np.zeros([network.nregions])
    E=I+1

    Sini = np.zeros([network.nregions])
    S = np.zeros([network.nregions])
    S=E/2.0

    Rini = np.zeros([network.nregions])
    R = np.zeros([network.nregions])
    R=I+10

    day_state = [S, E, I, R] 
    labels = ["S", "E", "I", "R"]

    ntime = mdyn.days+ipar.num_simul_days
    data_evol = [np.zeros((network.nregions, ntime))]*4

    #Add network
    title_base = "Model_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end
    #Add parameters
    title_base = title_base + "\n_r"+str(ipar.infec_rate).replace(".","") \
        +"_s"+str(ipar.spread_rate).replace(".","") 
        #+"_s"+str(int(ipar.spread_rate))
    
    #simulate scenario
    drange = mex.daterange(mdyn.date_ini_obj, mdyn.date_end_obj+timedelta(days=ipar.num_simul_days))
    for i, day in enumerate(drange):
        print()
        print("Iterating day: ", day)
        indx = '{:02d}'.format(i)
        title = title_base+"_day_"+day.strftime("%Y-%m-%d")
        basename = mdyn.dump_dir+title_base.replace("\n", "")+"_day_"+indx
        map_seir_state(day_state, network, title, basename, labels)

        #Save day state
        for k, state in enumerate(day_state):
            data_evol[k][:,i] = state

        if day in mdyn.days_all: 
            mat = mdyn.movemats_adj_norm[i]
        else:
            #Use matrix with dow average
            dow = day.weekday()
            mat = movemat_avg[dow]
            
        day_state = model_seir(day_state, mat, ipar, network)
        

    filename = mdyn.dump_dir+title_base+"data_evol.csv"
    filename = filename.replace("\n", "")
    np.savetxt(filename, data_evol, delimiter=",")
    filename = mdyn.dump_dir+title_base+"data_evol.npy"
    filename = filename.replace("\n", "")
    np.save(filename, data_evol)

    risk_time = mex.risk_time(data_evol[2], ipar.risk_lim)
    risk_time = risk_time.astype(float)
    ones = np.ones(len(risk_time))
    risk_index = ones-(risk_time/np.max(risk_time))
    risk_index[risk_time<0]=np.nan
    risk_time[risk_time<0]=np.nan
    risk_time[risk_time<1]=1.0    
    
    filename = mdyn.dump_dir+title_base+"_risk_index.npy"
    filename = filename.replace("\n", "")
    np.save(filename, risk_index)
    filename = mdyn.dump_dir+title_base+"_risk_index.csv"
    filename = filename.replace("\n", "")
    np.savetxt(filename, risk_index, delimiter=",")

    risk_ind_fmt = {"Region": list(network.regions.values()) , "Index": risk_index, "Time": risk_time}
    df_risk_ind = pd.DataFrame(risk_ind_fmt)
    df_risk_ind = df_risk_ind.sort_values(["Index"], ascending = (False))
    print(df_risk_ind)

    filename = mdyn.dump_dir+title_base+"_risk_index_time_list.csv"
    filename = filename.replace("\n", "")
    df_risk_ind.to_csv (filename, index = False, header=True)

    title = title_base+"_risk_time_with_lim_"+str(ipar.risk_lim)
    filename = mdyn.dump_dir+title_base+"_risk_lim_"+str(ipar.risk_lim)+".jpg"
    print(" Plotting risk time ", filename)
    map=Map(network)
    map.map_move_by_reg(risk_time, network.regions, network, title, filename)

    title = title_base+"_risk_index"
    filename = mdyn.dump_dir+title_base+"_risk_index.jpg"
    print(" Plotting risk index ", filename)
    map=Map(network)
    map.map_move_by_reg(risk_index, network.regions, network, title, filename)


def model_seir(day_state, mat, ipar, network):

    S = day_state[0]
    E = day_state[1]
    I = day_state[2]
    R = day_state[3]
    N = network.reg_pop

    beta = 1.0
    rho = 1.0
    gamma = 1.0
    nu = 1.0
    s = ipar.spread_rate

    FS = -beta*S*(rho*E+I)/N
    FE =  beta*S*(rho*E+I)/N - gamma*E
    FI =  gamma*E - nu*I
    FR = nu*I  

    #Outgoing infected
    M = np.copy(mat)
    zero = np.zeros([M.shape[0]])
    np.fill_diagonal(M, zero) #Zero diagonal of move matriz
    Iout = np.matmul(M, I) #AI

    #Incoming infected    
    Iin = M.sum(axis=0)*I 

    #Newly infected
    Snew = S  + FS 
    Enew = E  + FE 
    Inew = I  + FI + ipar.spread_rate*(Iout-Iin)
    Rnew = R  + FR 

    S = Snew.clip(min=0) #Make positive
    E = Enew.clip(min=0) #Make positive
    I = Inew.clip(min=0) #Make positive
    R = Rnew.clip(min=0) #Make positive

    #Check non source infected people
    tmp_state = np.copy(I)
    tmp_state[np.argmax(I)] = 0.0
    print("Non source: avg,max,min :",np.average(tmp_state), np.max(tmp_state), np.min(tmp_state))

    day_state = [S, E, I, R]

    return day_state


def calc_move_mat_avg_dow(mdyn, network, ipar):
    #Period mean move mat per dow
    movemat_avg_adj = [np.zeros(mdyn.movemats[0].shape)]*7
    movemat_avg = [np.zeros(mdyn.movemats[0].shape)]*7
    
    print("Warning: Will adjust raw data matrices for region populations")
    mdyn.movemats_adj = [np.zeros(mdyn.movemats[0].shape)]*len(mdyn.days_all)
    mdyn.movemats_adj_norm = [np.zeros(mdyn.movemats[0].shape)]*len(mdyn.days_all)

    #Loop work with transitions matrices and average them by day of the week
    for i, day in enumerate(mdyn.days_all):
        print()
        print("Calculating on: ", i, day)
        dow = day.weekday()
        
        diag_orig = np.diag(mdyn.movemats[i])
        print(" Original Diagonal(avg, max, min)       :", \
            np.average(np.diag(mdyn.movemats[i])), np.max(np.diag(mdyn.movemats[i])), np.min(np.diag(mdyn.movemats[i])))
        move_orig = mdyn.movemats[i].sum(axis=0) - diag_orig
        print(" Original Moving  (avg, max, min):", \
            np.average(move_orig), np.max(move_orig), np.min(move_orig))    

        #Only movement is trustworthy in the data, so use it and adjust diagonal
        #Adjust diagonal according to population size
        #Normalize matrix to population
        if np.sum(network.reg_pop) > 0.0:
            diag_adj = network.reg_pop - move_orig
            mat_adj =  np.copy(mdyn.movemats[i])
            np.fill_diagonal(mat_adj, diag_adj, wrap=False)
            mdyn.movemats_adj[i] = mat_adj
            mdyn.movemats_adj_norm[i] = mat_adj/ mat_adj.sum(axis=0)
        else:
            diag_adj = np.diag(mdyn.movemats[i])
            mdyn.movemats_adj[i] = mdyn.movemats[i] 
            mdyn.movemats_adj_norm[i] = mdyn.movemats_norm[i]
        
        title_base = network.domain+" "+network.subdomains+" "+day.strftime("%Y-%m-%d")+" "+mex.weekdays[dow]
                
        movemat_avg_adj[dow] = movemat_avg_adj[dow] + mdyn.movemats_adj[i]
        movemat_avg[dow] = movemat_avg[dow] + mdyn.movemats[i]

        #Plot matrix diagonal
        diag_norm = np.diag(mdyn.movemats_norm[i])

        filename =  mdyn.dump_dir+title_base.replace('\n','').replace(' ','_')+"_day_prob_diag.jpg"
        if not os.path.exists(filename):
            print("  Plotting :", filename)
            map=Map(network)
            map.map_move_by_reg(diag_norm, network.regions, network, title_base+"\nDiagonal Prob Move", filename)

        num_source = ipar.num_source
        
        sources = np.argpartition(diag_orig, -num_source)[-num_source:]
        
        print("Main regions:", sources)
        #Plot daily main sources
        for j in sources:
            title = title_base+"\nOrigin "+str(network.regions[j])
            filename =  mdyn.dump_dir+title.replace('\n','').replace(' ','_')+"_day_prob.jpg"
            if not os.path.exists(filename):
                print("Creating plot for ", filename)
                move_vec = mdyn.movemats_norm[i][:, j]
                    
                map=Map(network)
                map.map_move_by_reg(move_vec, network.regions, network, title, filename)

        #Plot daily main sources - adjusted matrices
        for j in sources:
            title = title_base+"\nOrigin "+str(network.regions[j])
            filename =  mdyn.dump_dir+title.replace('\n','').replace(' ','_')+"_adj_day_prob.jpg"
            if not os.path.exists(filename):
                print("Creating plot for ", filename)
                move_vec = mdyn.movemats_adj_norm[i][:, j]
                    
                map=Map(network)
                map.map_move_by_reg(move_vec, network.regions, network, title, filename)
            

        #mex.matprint(mdyn.movemats_norm[i])

    #Save 1st diagonal for future use, just as a refernce to get main sources
    movemat_avg_diag = np.diag(movemat_avg_adj[0])
    movemat_avg_diag = movemat_avg_diag[0:network.nreg_in]

    #Calculate average matrices
    #print(movemat_avg_diag)
    for i in range(7):
        #normalize move mat
        if np.sum(np.sum(movemat_avg_adj[i])) > 0 :
            movemat_avg_adj[i] = movemat_avg_adj[i] / movemat_avg_adj[i].sum(axis=0)
            movemat_avg[i] = movemat_avg[i] / movemat_avg[i].sum(axis=0)

        #Plot avg matrices
        title_base = network.domain+" "+network.subdomains+" "+mdyn.date_ini+" "+mdyn.date_end+" "+mex.weekdays[i]
        filename =  mdyn.dump_dir+title_base.replace('\n','').replace(' ','_')+"avg_prob_move.jpg"
        if np.sum(np.sum((movemat_avg_adj[i]))) > 0 and network.nregions < 20:
            if not os.path.exists(filename):
                mex.plot_matrix(movemat_avg_adj[i], title_base+"\nMean Move Prob", filename)

        #Std dev and its plot
        movemat_std = np.std(mdyn.movemats_norm, axis=0)
        filename =  mdyn.dump_dir+title_base.replace('\n','').replace(' ','_')+"_std_prob_move.jpg"
        if not os.path.exists(filename) and network.nregions < 20:
            try:
                mex.plot_matrix(movemat_std, title_base+"\nStd_Dev_of_Prob", filename)
            except:
                pass
        
        #Plot daily main sources - non adjusted matrices
        for j in sources:
            title = title_base+"\nOrigin "+str(network.regions[j])
            filename =  mdyn.dump_dir+title.replace('\n','').replace(' ','_')+"_dow_avg_prob.jpg"
            if not os.path.exists(filename):
                print("Creating plot for ", filename)
                move_vec = movemat_avg[i][:, j]
                    
                map=Map(network)
                map.map_move_by_reg(move_vec, network.regions, network, title, filename)

    return movemat_avg_adj,  movemat_std, movemat_avg_diag


def map_seir_state(day_state, network, title, basename, labels):

    for state, lab in zip(day_state, labels):
        filename = basename+"_SEIR-"+lab+".jpg"
        if not os.path.exists(filename):
            #print("Creating plot for", filename)
            map=Map(network)
            map.map_reg_var(state, network.regions, network, title+" SEIR: "+lab, filename)
    