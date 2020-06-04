
import sys
import os

import numpy as np
import statistics
from scipy import stats
import pandas as pd

from datetime import datetime
from datetime import date
from datetime import timedelta

from mdynpy.mdyn_map import Map
import mdynpy.mdyn_extras as mex
import mdynpy.mdyn_socialdist as sd

def map_move_mats(mdyn, network, ipar):
    print()
    print("Mapping move mats:")
    #Get movement matrices
    mdyn.collect_move_mat(network)

    #Get isolation indeces
    iso = sd.socialdist(ipar.isoind_file, network) 

    #print(iso.df.columns)
    #print("Regions:", network.regions)
    
    #Loop for each day
    for i, day in enumerate(mdyn.days_all):
        
        print("Calculating on: ", i, day.strftime("%Y-%m-%d"))
        #print(iso.df['day'].unique(), day.strftime("%Y-%m-%d"))
        
        #filter day, state, regions
        df_iso = iso.df[iso.df['day']==day.strftime("%Y-%m-%d")]
        
        if network.domain_abrv == "BRA":
            #BRA uses geocodes, so get city names
            regions = network.regions_in_names
        else:
            #Use actual city names
            regions = network.regions
            #Filter state
            df_iso = df_iso[df_iso['reg_name'].isin(regions.values())]

        mat = mdyn.movemats[i]
        reg_iso = np.zeros([network.nreg_in])
        for reg in range(network.nreg_in):
            region = regions.get(reg)
            region = str(region)   
            if region in list(df_iso['reg_name'].values): 
                isotmp = df_iso.loc[df_iso['reg_name'] == region, 'iso'].values[0]
            else:
                isotmp = np.nan
            reg_iso[reg] = isotmp
            
        #Do map
        dow=mex.weekdays[day.weekday()]
        if ipar.zoom[0]:
            title = network.domain+" "+network.subdomains+" Network Zoom "+ipar.zoom[6]+" "
            filename = mdyn.dump_dir+title.replace(" ", "_")+"_"+str(i).zfill(3)+".jpg"
        else:
            title = network.domain+" "+network.subdomains+" Network "
            filename = mdyn.dump_dir+title.replace(" ", "_")+str(i).zfill(3)+".jpg"

        title = title + day.strftime("%Y-%m-%d")+" "+dow
        
        if network.domain_abrv=="BRA":

            zooms=[
                [False, -15, -34, -60, -40, False, "BRA"  ],
                [True, 0, -15, -50,-34, False, "Nordeste"  ],
                [True, -15, -34, -60,-40, False, "Sul-Sudeste"  ],
                [True, -10, -25, -60,-40, False, "Centro-Sudeste"  ],
                [True, -22.8, -24.2, -47.8,-45.6, True, "RMSP"  ],
                [True, -19.8, -25.5, -52.25,-43, True, "SP"  ]
            ]

            for zoom in zooms:
                map=Map(network, zoom)
                map.map_network_data(reg_iso, mat, regions, title, filename)
        
                map=Map(network, zoom)
                map.map_network_flux(mat, regions, title, filename.replace("Network", "Network_Flux"))

                map=Map(network, zoom)
                map.map_data_on_network(reg_iso, mat, regions, title, filename=filename.replace("Network", "Network_Iso"))
        else:
            map=Map(network, ipar.zoom)
            map.map_network_data(reg_iso, mat, regions, title, filename)
        
            map=Map(network, ipar.zoom)
            filename=filename.replace("Network", "Network_Flux")
            map.map_network_flux(mat, regions, title, filename)

            map=Map(network, ipar.zoom)
            filename=filename.replace("Network_Flux", "Network_Iso")
            map.map_data_on_network(reg_iso, mat, regions, title, filename)

def analyse_move_mats(mdyn, network, ipar):
    print()
    print("Analyse move mats:")
    #Analyse movement matrices
    mdyn.collect_move_mat(network)

    #Calculate matrix statistics
    movemat_avg, movemat_std, movemat_avg_diag = calc_move_mat_avg(mdyn, network, ipar)

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

    title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end

    print("Regions:", network.regions)
    print("Main regions:", sources)
    #Plot main sources
    for j in sources:
        title = title_base+"_origin_"+str(network.regions[j])
        print("Creating plot for ", title)
        move_vec = movemat_avg[:, j]
        sumv = np.sum(move_vec)
        if abs(sumv-1.0)>0.001:
            print("Warning: Probability with sum not 1.", sumv)
            
        map=Map(network)
        map.map_move_by_reg(move_vec, network.regions, network, title, mdyn.dump_dir+title+".jpg")

def analyse_move_mats_dow(mdyn, network, ipar):
    print()
    print("Analyse move mats by day of the week:")
    #Analyse movement matrices
    mdyn.collect_move_mat(network)

    #Calculate matrix statistics
    movemat_avg, movemat_std, movemat_avg_diag = calc_move_mat_avg_dow(mdyn, network, ipar)

    #Sources to plot
    num_source = ipar.num_source #min(network.nregions-2, 20)
    #Primary source 
    prim_source = np.argmax(movemat_avg_diag)
    #First hits by primary source
    sources = np.argpartition(movemat_avg[0][0:network.nreg_in, prim_source], -num_source)[-num_source:]
    #Large centers (secondary sources)
    sources2 = np.argpartition(movemat_avg_diag[0:network.nreg_in], -num_source)[-num_source:]
    #Join all main sources
    sources = np.unique(np.concatenate((sources, sources2), axis=0) )

    title_base = network.domain+" "+network.subdomains+" "+mdyn.date_ini+" "+mdyn.date_end

    print("Regions:", network.regions)
    print("Main regions:", sources)
    #Plot main sources
    for i in range(7):
        for j in sources:
            title = title_base+" "+mex.weekdays[i]+"\n Origin "+str(network.regions[j])
            filename =  mdyn.dump_dir+title.replace('\n','').replace(' ','_')+".jpg"
            
            print("Creating plot for ", filename)
            move_vec = movemat_avg[i][:, j]
            sumv = np.sum(move_vec)
            if abs(sumv-1.0)>0.001:
                print("Warning: Probability with sum not 1.", sumv)
            map=Map(network)
            map.map_move_by_reg(move_vec, network.regions, network, title, filename)
        

def simulate_move_mats(mdyn, network, ipar):

    #Analyse movement matrices
    mdyn.collect_move_mat(network)

    movemat_avg, movemat_std, movemat_avg_diag = calc_move_mat_avg(mdyn, network, ipar)
    #Sources to plot 
    #num_simul_days = ipar.num_simul_days #min(network.nregions-2, 20)

    #Initial condition
    data_ini_regv = np.zeros([network.nregions])
    try:
        data_ini_regv[268] = 100.0
    except:
        data_ini_regv[10] = 100.0

    day_state = data_ini_regv

    title_base = "Simul_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end
    #simulate scenario
    if ipar.simul_type == "avg":
        drange = mex.daterange(mdyn.date_ini_obj, mdyn.date_end_obj+timedelta(days=40))
    else:
        drange = mdyn.days_all

    for i, day in enumerate(drange):
    #for j in num_simul_days:
        indx = '{:02d}'.format(i)
        title = title_base+"_day_"+indx #+day.strftime("%Y-%m-%d")
        print("Creating plot for ", title)
            
        map=Map(network)
        map.map_move_by_reg(day_state, network.regions, network, title, mdyn.dump_dir+title)

        if ipar.simul_type == "avg":
            mat = movemat_avg
        else:
            mat = mdyn.movemats_norm[i]

        day_state=np.matmul(mat, day_state)

        sumv = np.sum(day_state)
        if abs(sumv-np.sum(data_ini_regv))>0.001:
            print("Warning: Mass not being conserved! ", sumv)
    


def calc_move_mat_avg(mdyn, network, ipar):
    #Period mean move mat
    movemat_avg = np.zeros(mdyn.movemats[0].shape)

    #Loop work with transitions matrices and average then
    for i, day in enumerate(mdyn.days_all):
        print("Calculating on: ", i, day)
        
        title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+day.strftime("%Y-%m-%d")
        filename=mdyn.dump_dir+title_base+"_day_prob_move.jpg"
        if not os.path.exists(filename):
            print("  Plotting :", filename)
            mex.plot_matrix(mdyn.movemats_norm[i], title_base+"\nDay_Prob_Move", filename)
        movemat_avg = movemat_avg + mdyn.movemats[i]

        #Plot matrix diagonal
        diag = np.diag(mdyn.movemats_norm[i])
        title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+day.strftime("%Y-%m-%d")
        filename=mdyn.dump_dir+title_base+"_diagonal_prob.jpg"
        if not os.path.exists(filename):
            print("  Plotting :", filename)
            map=Map(network)
            map.map_move_by_reg(diag, network.regions, network, title_base+"\nDiagonal Prob_Move", filename)

        #mex.matprint(mdyn.movemats_norm[i])

    #Get primary and secondary sources
    movemat_avg_diag = np.diag(movemat_avg)
    movemat_avg_diag = movemat_avg_diag[0:network.nreg_in]
    #print(movemat_avg_diag)
    movemat_avg = movemat_avg / movemat_avg.sum(axis=0)

    title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end
    filename = mdyn.dump_dir+title_base+"_avg_prob_move.jpg"
    
    if not os.path.exists(filename):
        mex.plot_matrix(movemat_avg, title_base+"\nMean_Prob", filename)

    filename = mdyn.dump_dir+title_base+"_std_prob_move.jpg"
    movemat_std = np.std(mdyn.movemats_norm, axis=0)
    if not os.path.exists(filename):
        try:
            mex.plot_matrix(movemat_std, title_base+"\nStd_Dev_of_Prob", filename)
        except:
            pass

    return movemat_avg,  movemat_std, movemat_avg_diag


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
        if np.sum(network.reg_pop) > 0.0:
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

def simulate_model(mdyn, network, ipar):

    #Analyse movement matrices
    mdyn.collect_move_mat(network)

    movemat_avg, movemat_std, movemat_avg_diag = calc_move_mat_avg_dow(mdyn, network, ipar)
    #Sources to plot 
    #num_simul_days = ipar.num_simul_days #min(network.nregions-2, 20)
    
    #Initial condition
    data_ini_regv = np.zeros([network.nregions])
    for key in ipar.data_ini_by_reg:
        data_ini_regv[key] = ipar.data_ini_by_reg[key]

    day_state = data_ini_regv
    ntime = mdyn.days+ipar.num_simul_days
    data_evol = np.zeros((network.nregions, ntime))
    title_base = "Model_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end
    title_base = title_base + "\n_r"+str(ipar.infec_rate).replace(".","") \
        +"_s"+str(ipar.spread_rate).replace(".","") 
        #+"_s"+str(int(ipar.spread_rate))
    #simulate scenario
    drange = mex.daterange(mdyn.date_ini_obj, mdyn.date_end_obj+timedelta(days=ipar.num_simul_days))
    
    for i, day in enumerate(drange):
    #for j in num_simul_days:
        indx = '{:02d}'.format(i)
        title = title_base+"_day_"+day.strftime("%Y-%m-%d")
        filename = mdyn.dump_dir+title_base+"_day_"+indx+".jpg"
        if not os.path.exists(filename):
            print("Creating plot  ", filename)
            print()    
            map=Map(network)
            map.map_move_by_reg(day_state, network.regions, network, title, filename)

        #Save day state
        data_evol[:,i] = day_state

        if day in mdyn.days_all: 
            mat = mdyn.movemats_adj_norm[i]
        else:
            #Use matrix with dow average
            dow = day.weekday()
            mat = movemat_avg[dow]
            
        day_state = model(day_state, mat, ipar, network)

        sumv = np.sum(day_state)
        maxv = np.max(day_state)
        minv = np.min(day_state)
        print(day, "Num infected, avg, max, min:", sumv, np.average(day_state), maxv, minv)
        
        if maxv > np.sum(network.reg_pop):
            print( "Too many people infected, reached the limit of the model")
            break

    filename = mdyn.dump_dir+title_base+"data_evol.csv"
    filename = filename.replace("\n", "")
    np.savetxt(filename, data_evol, delimiter=",")
    filename = mdyn.dump_dir+title_base+"data_evol.npy"
    filename = filename.replace("\n", "")
    np.save(filename, data_evol)

    risk_time = mex.risk_time(data_evol, ipar.risk_lim)
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




def model(day_state, mat, ipar, network):

    if ipar.model == 0: #simple model
        day_state=(1.0+ipar.infec_rate)*day_state + np.matmul(mat, day_state)

    elif ipar.model == 1: #Infected model

        pop_inf = np.divide(network.reg_pop - day_state, network.reg_pop) #* np.heaviside( np.divide(network.reg_pop - day_state, network.reg_pop), 0) #(N-I)/N
        print("(N-I)/N :     avg, max, min :", np.average(pop_inf), np.max(pop_inf), np.min(pop_inf))

        local_inf = day_state + ipar.infec_rate * np.multiply(day_state, pop_inf) #I+rI(N-I)/N 
        print("I+rI(N-I)/N : avg, max, min :", np.average(local_inf), np.max(local_inf), np.min(local_inf))

        out_inf = np.divide(np.matmul(mat, day_state), network.reg_pop) #AI/N
        print("AI/N :        avg, max, min :", np.average(out_inf), np.max(out_inf), np.min(out_inf))

        in_inf = np.divide(np.matmul(mat.transpose(), day_state), network.reg_pop) #AtI/N
        print("AtI/N :       avg, max, min :", np.average(in_inf), np.max(in_inf), np.min(in_inf))

        day_state= local_inf +out_inf - in_inf
        print("I+rI(N-I)/N + AI/N-AtI/N : avg, max, min :",np.average(day_state), np.max(day_state), np.min(day_state))

    elif ipar.model == 2: #2 parameter model

        #Local infections
        pop_inf = np.divide(network.reg_pop - day_state, network.reg_pop)  #(N-I)/N
        pop_inf = pop_inf.clip(min=0) #Make positive, just in case
        #print("(N-I)/N :     avg, max, min :", np.average(pop_inf), np.max(pop_inf), np.min(pop_inf)) 
        local_inf = day_state + ipar.infec_rate * day_state*pop_inf #I+rI(N-I)/N 
        #print("I+rI(N-I)/N : avg, max, min :", np.average(local_inf), np.max(local_inf), np.min(local_inf))

        #Outgoing infected
        move_mat = np.copy(mat)
        zero = np.zeros([mat.shape[0]])
        np.fill_diagonal(move_mat, zero) #Zero diagonal of move matriz
        out_inf = np.matmul(move_mat, day_state) #AI
        #print("AI/N :        avg, max, min :", np.average(out_inf), np.max(out_inf), np.min(out_inf))

        #Incoming infected
        #in_inf = np.divide(np.matmul(mat.transpose(), day_state), network.reg_pop) #AtI/N
        #in_inf = np.matmul(move_mat.transpose(), day_state) #AtI - wrong!
        in_inf = move_mat.sum(axis=0)*day_state #
        #print("AtI/N :       avg, max, min :", np.average(in_inf), np.max(in_inf), np.min(in_inf))

        #Newly infected
        day_state = local_inf + ipar.spread_rate*(out_inf -  in_inf)
        day_state = day_state.clip(min=0) #Make positive

        #Check non source infected people
        #print("I+rI(N-I)/N + s(AI/N-AtI/N): avg,max,min :",np.average(day_state), np.max(day_state), np.min(day_state))
        tmp_state = np.copy(day_state)
        tmp_state[np.argmax(tmp_state)] = 0.0
        print("Non source: avg,max,min :",np.average(tmp_state), np.max(tmp_state), np.min(tmp_state))

    return day_state

