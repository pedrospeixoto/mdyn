
import sys
import os

import numpy as np
import statistics

from datetime import datetime
from datetime import date
from datetime import timedelta

from mdyn_map import Map
import mdyn_extras as mex

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
        title = title_base+"_origin_"+network.regions[j]
        print("Creating plot for ", title)
        move_vec = movemat_avg[:, j]
        sumv = np.sum(move_vec)
        if abs(sumv-1.0)>0.001:
            print("Warning: Probability with sum not 1.", sumv)
            
        map=Map(network)
        map.map_move_by_reg(move_vec, network.regions, network, title, mdyn.dump_dir+title)

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

    title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end

    print("Regions:", network.regions)
    print("Main regions:", sources)
    #Plot main sources
    for i in range(7):
        for j in sources:
            title = title_base+"_origin_"+network.regions[j]+"_"+mex.weekdays[i]
            print("Creating plot for ", title)
            move_vec = movemat_avg[i][:, j]
            sumv = np.sum(move_vec)
            if abs(sumv-1.0)>0.001:
                print("Warning: Probability with sum not 1.", sumv)
                
            map=Map(network)
            map.map_move_by_reg(move_vec, network.regions, network, title, mdyn.dump_dir+title)
        

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
        filename=mdyn.dump_dir+title_base+"_day_prob_move"
        if not os.path.exists(filename+".jpg"):
            print("  Plotting :", filename)
            mex.plot_matrix(mdyn.movemats_norm[i], title_base+"\nDay_Prob_Move", filename)
        movemat_avg = movemat_avg + mdyn.movemats[i]

        #Plot matrix diagonal
        diag = np.diag(mdyn.movemats_norm[i])
        title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+day.strftime("%Y-%m-%d")
        filename=mdyn.dump_dir+title_base+"_diagonal_prob"
        if not os.path.exists(filename+".jpg"):
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
    filename = mdyn.dump_dir+title_base+"_avg_prob_move"
    
    if not os.path.exists(filename+".jpg"):
        mex.plot_matrix(movemat_avg, title_base+"\nMean_Prob", filename)

    filename = mdyn.dump_dir+title_base+"_std_prob_move"
    movemat_std = np.std(mdyn.movemats_norm, axis=0)
    if not os.path.exists(filename+".jpg"):
        try:
            mex.plot_matrix(movemat_std, title_base+"\nStd_Dev_of_Prob", filename)
        except:
            pass

    return movemat_avg,  movemat_std, movemat_avg_diag


def calc_move_mat_avg_dow(mdyn, network, ipar):
    #Period mean move mat per dow
    movemat_avg = [np.zeros(mdyn.movemats[0].shape)]*7

    #Loop work with transitions matrices and average then by day of the week
    for i, day in enumerate(mdyn.days_all):
        print("Calculating on: ", i, day)
        dow = day.weekday()
        title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+day.strftime("%Y-%m-%d")
        filename=mdyn.dump_dir+title_base+"_day_prob_move"
        if not os.path.exists(filename+".jpg"):
            print("  Plotting :", filename)
            mex.plot_matrix(mdyn.movemats_norm[i], title_base+"\nDay_Prob_Move", filename)
        movemat_avg[dow] = movemat_avg[dow] + mdyn.movemats[i]

        #Plot matrix diagonal
        diag = np.diag(mdyn.movemats_norm[i])
        title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+day.strftime("%Y-%m-%d")
        filename=mdyn.dump_dir+title_base+"_diagonal_prob"
        if not os.path.exists(filename+".jpg"):
            print("  Plotting :", filename)
            map=Map(network)
            map.map_move_by_reg(diag, network.regions, network, title_base+"\nDiagonal Prob_Move", filename)

        #mex.matprint(mdyn.movemats_norm[i])

    #Get primary and secondary sources
    movemat_avg_diag = np.diag(movemat_avg[0])
    movemat_avg_diag = movemat_avg_diag[0:network.nreg_in]

    #print(movemat_avg_diag)
    for i in range(7):
        movemat_avg[i] = movemat_avg[i] / movemat_avg[i].sum(axis=0)

        title_base = "move_mat_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end+"_dow"+str(i)
        filename = mdyn.dump_dir+title_base+"_avg_prob_move_dow"+str(i)
        
        if np.sum(np.sum((movemat_avg[i]))) > 0:
            if not os.path.exists(filename+".jpg"):
                mex.plot_matrix(movemat_avg[i], title_base+"\nMean_Prob", filename)

        filename = mdyn.dump_dir+title_base+"_std_prob_move"
        movemat_std = np.std(mdyn.movemats_norm, axis=0)
        if not os.path.exists(filename+".jpg"):
            try:
                mex.plot_matrix(movemat_std, title_base+"\nStd_Dev_of_Prob", filename)
            except:
                pass

    return movemat_avg,  movemat_std, movemat_avg_diag


"""     def simulate_daily(self, mode):
    
        #initial condition
        
        x = np.zeros([self.network.nreg_in])
        x[3]=1

        fig, ax = plt.subplots(figsize=(12,6))
        labels = list(self.network.regions.values())
        
        ax.plot( x, 'o', label='IC')

        #ax.set_ylim(0,450)
        ax.set_ylabel('Individuals')
        ax.set_title('Zombie Test')
        ax.set_xticks(np.arange(len(x)))
        ax.set_xticklabels(labels)
        ax.set_yscale('log')
        
        
        for i, day in enumerate(self.data):
            #print(day.tmat, day.tmat.shape, x.shape)
            x=np.matmul(day.tmat, x)
            print(x.sum())
            ax.plot( x, 'x', label=day.day)
            ax.legend()
            filename = self.dump_dir+"daily_simulation"+day.day+".jpg"
            plt.savefig(filename, dpi=300)
            del ax.lines[1]   
            
    def simulate_weekly(self, mode, dayoftheweek):

        #initial condition
        
        x = np.zeros([self.network.nreg_in])
        tmatweek = np.identity(self.network.nreg_in)
        tmat_list = [] #List of transiction matrices
        day_list = [] #List of dates used
        for i, day in enumerate(self.data):
            if day.day_weekday == dayoftheweek: #Monday=0
                day_list.append(day.day)
                tmat_list.append(tmatweek)
                print("Transition matrix for Monday to Monday", day.day)
                print(tmatweek)
                tmatweek = np.identity(self.network.nreg_in)

            #print(day.tmat, day.tmat.shape, x.shape)
            tmatweek=np.matmul(day.tmat, tmatweek)

        #Calculate the average matrix
        #basicmat=np.zeros([self.network.nregions,self.network.nregions])
        #for mat in tmat_list:
        #    basicmat = basicmat + mat
        if len(tmat_list) > 0:
            basicmat=sum(tmat_list)/len(tmat_list)
        else:
            basicmat=tmat_list
        print(basicmat)

        #       
        #Simulate 
        x[3]=1

        fig, ax = plt.subplots(figsize=(12,6))
        labels = list(self.network.regions.values())
        
        ax.plot( x, 'o', label='IC')

        #ax.set_ylim(0,450)
        ax.set_ylabel('Individuals')
        ax.set_title('Zombie Test')
        ax.set_xticks(np.arange(len(x)))
        ax.set_xticklabels(labels)
        ax.set_yscale('log')
        for i in range(20):
            #print(day.tmat, day.tmat.shape, x.shape)
            x=np.matmul(basicmat, x)
            print(x.sum())
            ax.plot( x, 'x', label=str(i))
            ax.legend()
            filename = self.dump_dir+"weekly_simulation"+str(i)+".jpg"
            plt.savefig(filename, dpi=300)
            del ax.lines[1]   
        
        
 """
        
