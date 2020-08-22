
import sys
import os

import numpy as np
import statistics
from scipy import stats
import pandas as pd

from datetime import datetime
from datetime import date
from datetime import timedelta

import matplotlib as mpl
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.colors as colors
import matplotlib.cm as cm
from matplotlib.ticker import MaxNLocator
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib.ticker as mtick
from adjustText import adjust_text
from matplotlib import patches

import seaborn as sns


from mdynpy.mdyn_map import Map
import mdynpy.mdyn_extras as mex
import mdynpy.mdyn_socialdist as sd


def statistics_move_mats(mdyn, network, ipar):
    print()
    print("Statistics of move mats:")
    #Get movement matrices
    mdyn.collect_move_mat(network)
    
    if network.domain_abrv == "BRA":
        #BRA uses geocodes, so get city names
        regions = network.regions_in_names
    else:
        #Use actual city names
        regions = network.regions
        #Filter state

    print("Regions:", regions)
    
    key = ipar.reference
    refname = regions.get(key)
    refind = key
    print()
    print("Reference:", refname, refind)
    keys = ipar.neighb
    nneib_pop = ipar.nneighb

    pop=np.array(network.reg_pop)
    pop_ref = pop[refind]
    ipop = list(pop.argsort()[-nneib_pop:][::-1])
    if refind in ipop:
        ipop.remove(refind)
    
    for k in keys:
        if k in ipop:
            ipop.remove(k)

    keys = keys + ipop
    if refind in keys:
        keys.remove(refind)
    neibind = keys
    nneib = len(keys)
    neib = [None]*nneib

    print("Neighbours")
    for i, key in enumerate(keys):
        neib[i]=regions.get(key)

    print(neib, neibind)

    title = network.domain+" "+network.subdomains+" Mobility "+refname+" "
    filename = mdyn.dump_dir+title.replace(" ", "_")
    #neib_title = network.domain+" "+network.subdomains+" Mains Fluxes "+refname+" "
    #neib_file = mdyn.dump_dir+neib_title.replace(" ", "_")

    ndays = len(mdyn.days_all)
    evolneib_in = np.zeros((nneib,ndays))
    evolneib_out = np.zeros((nneib,ndays))
    evoldiag = np.zeros(ndays)
    evol_inall = np.zeros(ndays)
    evol_outall = np.zeros(ndays)

    days = []
    #Loop for each day
    for i, day in enumerate(mdyn.days_all):
        day_str = day.strftime("%Y-%m-%d")
        #print("Calculating on: ", i, day_str)
        days.append(day_str)              
        mat = mdyn.movemats[i]
        evoldiag[i] = mat[refind,refind]
        evol_inall[i] = sum(mat[refind,:])-evoldiag[i]
        evol_outall[i] = sum(mat[: , refind])-evoldiag[i]

        for j, ind in enumerate(neibind):
            evolneib_in[j, i] = mat[refind, ind]
            evolneib_out[j, i] = mat[ind, refind]  
    
    # Draw Plot
    fig = plt.figure(figsize=(20,10), dpi= 300)
    mycolors = [
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        ]      

    ref_ndays = 7

    relative = False
    lab = "INTERNAS A "+refname
    #plt.plot(mdyn.days_all, evoldiag, color=mycolors[0], linewidth=3, label=lab)
    dates_ma = mdyn.days_all[6:]
    evol_ma = mex.moving_average(evoldiag)
    evol_ref = np.average(evoldiag[0:ref_ndays])
    #evol_ref = evol_ma[0]
    if relative:
        evol_ma = 100*(evol_ma - evol_ref)/evol_ref
        plt.plot(dates_ma, evol_ma, color=mycolors[0], linewidth=4, label=lab)
        #plt.text(dates_ma[-1]+timedelta(days=3), evol_ma[-1], lab, fontsize=14, color=mycolors[0])
    
    data = {"Data": dates_ma, lab: evol_ma }
    dataraw = {"Data": mdyn.days_all, lab: evoldiag }
    dataraw_in = {"Data": dates_ma}
    dataraw_out = {"Data": dates_ma}

    lab = "SAINDO DE "+refname
    #plt.plot(mdyn.days_all, evoldiag, color=mycolors[0], linewidth=3, label=lab)
    evol_ma = mex.moving_average(evol_outall)
    dataraw_out[refname]=evol_ma

    evol_ref = np.average(evol_outall[0:ref_ndays])
    #evol_ref = evol_ma[0]
    if relative:
        evol_ma = 100*(evol_ma - evol_ref)/evol_ref
        #evol_ma = 100*(evol_ma - evol_ma[0])/evol_ma[0]
        plt.plot(dates_ma, evol_ma, color=mycolors[1], linewidth=4, label=lab)
        #plt.text(dates_ma[-1]+timedelta(days=3), evol_ma[-1]+1, lab, fontsize=14, color=mycolors[1])
    
    data[lab]=evol_ma
    dataraw[lab]=evol_outall
    
    lab = "ENTRANDO EM "+refname
    #plt.plot(mdyn.days_all, evoldiag, color=mycolors[0], linewidth=3, label=lab)
    evol_ma = mex.moving_average(evol_inall)
    dataraw_in[refname]=evol_ma

    evol_ref = np.average(evol_inall[0:ref_ndays])
    #evol_ref = evol_ma[0]
    if relative:
        evol_ma = 100*(evol_ma - evol_ref)/evol_ref
        #evol_ma = 100*(evol_ma - evol_ma[0])/evol_ma[0]
        plt.plot(dates_ma, evol_ma, color=mycolors[2], linewidth=4, label=lab)
        #plt.text(dates_ma[-1]+timedelta(days=3), evol_ma[-1]-1, lab, fontsize=14, color=mycolors[2])
    data[lab]=evol_ma
    dataraw[lab]=evol_inall
    
    texts = []
    k = 0
    for ki, nb in enumerate(neib):
        lab_out = refname+"->"+nb
        lab_in = refname+"<-"+nb
        evol = evolneib_in[ki, :]

        if np.average(evol) < 5:
            print("Warning: City with small number of trips, removing ", nb)
            continue

        k = k +1
        evol_ma = mex.moving_average(evol)
        dataraw_in[nb] = evol_ma

        evol_ref = np.average(evol[0:ref_ndays])
        #evol_ref = evol_ma[0]
        if relative:
            evol_ma = 100*(evol_ma - evol_ref)/evol_ref
        #evol_ma = 100*(evol_ma - evol_ma[0])/evol_ma[0]
        plt.plot(dates_ma, evol_ma, color=mycolors[3+k], linewidth=1, label=lab_in, linestyle="--")
        texts.append(plt.text(dates_ma[-1]+timedelta(days=7), 
            evol_ma[-1], nb, fontsize=10, color=mycolors[3+k], alpha=0.5))

        data["de "+nb] = evol_ma
        dataraw["de "+nb] = evol
        
        evol = evolneib_out[ki, :]
        evol_ma = mex.moving_average(evol)
        dataraw_out[nb] = evol_ma

        evol_ref = np.average(evol[0:ref_ndays])
        #evol_ref = evol_ma[0]
        if relative:
            evol_ma = 100*(evol_ma - evol_ref)/evol_ref
        #evol_ma = 100*(evol_ma - evol_ma[0])/evol_ma[0]
        plt.plot(dates_ma, evol_ma, color=mycolors[3+k], linewidth=1, label=lab_out, linestyle="-.")
        #plt.text(dates_ma[-1]+timedelta(days=1), evol_ma[-1], lab_out, fontsize=14, color=mycolors[3+k])

        data["para "+nb] = evol_ma
        dataraw["para "+nb] = evol
        

    plt.legend(loc='best', fontsize=12, ncol=2)

    #fancy stuff
    ax = plt.gca()

    ax.yaxis.get_offset_text().set_fontsize(14)
    ax.set_ylabel('viagens - média móvel semanal', fontsize=14)
    if relative:
        ax.yaxis.set_major_formatter(mtick.PercentFormatter())
    #plt.ticklabel_format(axis="y", style="plain", scilimits=(0,0))
    #plt.yscale('log')
    plt.yticks(fontsize=14, alpha=.7)

    plt.xlim(dates_ma[0], dates_ma[-1]+timedelta(days=10))
    xtick_location = mdyn.days_all[::7]
    xtick_labels = days[::7]
    xtick_location.append(xtick_location[-1]+timedelta(days=7))
    xtick_labels.append("")
    plt.xticks(ticks=xtick_location, labels=xtick_labels, ha="right", rotation=45, fontsize=15) #, alpha=.7)
    
    plt.title(refname, fontsize=16)

    plt.grid(axis='both', alpha=.3)
    plt.gca().spines["top"].set_alpha(0.0)    
    plt.gca().spines["bottom"].set_alpha(0.3)
    plt.gca().spines["right"].set_alpha(0.0)    
    plt.gca().spines["left"].set_alpha(0.3)   

    if relative:
        ref_txt = "Referência (média)\n"+days[0]+" a "+days[ref_ndays-1]
        #plt.gcf().text(0.90, 0.05, ref_txt, fontsize=14, 
        plt.gcf().text(0.1, 0.05, ref_txt, fontsize=14, 
            bbox=dict(facecolor='gray', alpha=0.3), horizontalalignment='center', 
            verticalalignment='center', transform=ax.transAxes)

    textstr = "Fonte:\n IME-USP/InLoco"
    plt.gcf().text(0.98, -0.1, textstr, fontsize=12, horizontalalignment='center', 
        verticalalignment='center', transform=ax.transAxes)

    plt.tight_layout() 

    adjust_text(texts, autoalign="y", only_move={'points': 'y',
        'text':'y', 'objects':'y'})
    
    #Save density plot to folder "dump"
    plt.savefig(filename+"evol.jpg", dpi=400)
    #plt.savefig(filename+"evol.tiff", dpi=200)
    plt.close()
    
    #-----------save dataframes -----------------------#

    df_raw = pd.DataFrame(dataraw)
    #df.to_csv(filename+"raw.csv", sep=";")
    df_raw.to_excel(filename+"raw.xls")

    df_perc = pd.DataFrame(data)
    #df.to_csv(filename+"percent.csv", sep=";")
    df_perc.to_excel(filename+"percent.xls")

    #-----------stack plot -----------------------#

    fig, ax = plt.subplots(2, 1, figsize=(20,10), dpi= 300)

    # Data in
    df_tmp = pd.DataFrame(dataraw_in)
    df_tmp['OUTROS'] = df_tmp.drop(['Data', refname], axis=1).sum(axis=1)
    df_tmp = df_tmp.drop([refname], axis=1)
    df_tmp = df_tmp.iloc[:, ::-1]
    ax[0] = df_tmp.plot.area(x='Data', ax=ax[0], alpha=0.5, legend=True)
    ax[0].set_ylabel('VIAGENS PARA '+refname, fontsize=14)
    handles, labels = ax[0].get_legend_handles_labels()
    ax[0].legend(handles[::-1], labels[::-1], bbox_to_anchor=(1.00, 1.00))

    df_tmp = pd.DataFrame(dataraw_out)
    df_tmp['OUTROS'] = df_tmp.drop(['Data', refname], axis=1).sum(axis=1)
    df_tmp = df_tmp.drop([refname], axis=1)
    df_tmp = df_tmp.iloc[:, ::-1]
    ax[1] = df_tmp.plot.area(x='Data', ax=ax[1], alpha=0.5, legend=False)
    ax[1].set_ylabel('VIAGENS DE '+refname, fontsize=14)
    #handles, labels = ax[0].get_legend_handles_labels()
    #ax[1].legend(handles[::-1], labels[::-1], bbox_to_anchor=(1.02, 1.00))

    for i in range(2):
        ax[i].yaxis.get_offset_text().set_fontsize(14)
        ax[i].set_xlabel(' ')
        ax[i].tick_params(axis='both', which='major', labelsize=14)
        ax[i].tick_params(axis='both', which='minor', labelsize=10)
        #ax[0].yaxis.set_major_formatter(mtick.PercentFormatter())
        ax[i].yaxis.get_major_formatter().set_powerlimits((0, 1))
        #plt.ticklabel_format(axis="y", style="sci", scilimits=(0,0))
        #ax[i].yticks(fontsize=14, alpha=.7)
        ax[i].spines["top"].set_alpha(0.0)    
        ax[i].spines["bottom"].set_alpha(0.3)
        ax[i].spines["right"].set_alpha(0.0)    
        ax[i].spines["left"].set_alpha(0.3)   

    textstr = "Fonte:\n IME-USP/InLoco"
    plt.gcf().text(1.10, -0.1, textstr, fontsize=12, horizontalalignment='center', 
        verticalalignment='center', transform=ax[1].transAxes)

    plt.tight_layout() 
    plt.savefig(filename+"stack_evol.jpg", dpi=400)
    
    return


def time_evolution_states_move_mats(mdyn, network, ipar):
    print()
    print("Time evolution of state move mats:")
    #Get movement matrices
    mdyn.collect_move_mat(network)
    
    if network.domain_abrv == "BRA":
        #BRA uses geocodes, so get city names
        regions = network.regions_in_names
    else:
        #Use actual city names
        regions = network.regions
        #Filter state

    print("Regions:", regions)
    
    key = ipar.reference
    refname = regions.get(key)
    refind = key
    print()
    print("Reference:", refname, refind)
    keys = ipar.neighb
    nneib_pop = ipar.nneighb

    pop=np.array(network.reg_pop)
    pop_ref = pop[refind]
    ipop = list(pop.argsort()[-nneib_pop:][::-1])
    if refind in ipop:
        ipop.remove(refind)
    
    for k in keys:
        if k in ipop:
            ipop.remove(k)

    keys = keys + ipop
    if refind in keys:
        keys.remove(refind)
    neibind = keys
    nneib = len(keys)
    neib = [None]*nneib

    print("Neighbours")
    for i, key in enumerate(keys):
        neib[i]=regions.get(key)

    print(neib, neibind)

    title = network.domain+" "+network.subdomains+" Mobility "+refname+" "
    filename = mdyn.dump_dir+title.replace(" ", "_")
    #neib_title = network.domain+" "+network.subdomains+" Mains Fluxes "+refname+" "
    #neib_file = mdyn.dump_dir+neib_title.replace(" ", "_")

    ndays = len(mdyn.days_all)
    evolneib_in = np.zeros((nneib,ndays))
    evolneib_out = np.zeros((nneib,ndays))
    evoldiag = np.zeros(ndays)
    evol_inall = np.zeros(ndays)
    evol_outall = np.zeros(ndays)

    days = []
    #Loop for each day
    for i, day in enumerate(mdyn.days_all):
        day_str = day.strftime("%Y-%m-%d")
        #print("Calculating on: ", i, day_str)
        days.append(day_str)              
        mat = mdyn.movemats[i]
        evoldiag[i] = mat[refind,refind]
        evol_inall[i] = sum(mat[refind,:])-evoldiag[i]
        evol_outall[i] = sum(mat[: , refind])-evoldiag[i]

        for j, ind in enumerate(neibind):
            evolneib_in[j, i] = mat[refind, ind]
            evolneib_out[j, i] = mat[ind, refind]  
    
    # Draw Plot
    fig = plt.figure(figsize=(20,10), dpi= 300)
    mycolors = [
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        ]      

    ref_ndays = 7

    relative = False
    lab = "INTERNAS A "+refname
    #plt.plot(mdyn.days_all, evoldiag, color=mycolors[0], linewidth=3, label=lab)
    dates_ma = mdyn.days_all[6:]
    evol_ma = mex.moving_average(evoldiag)
    evol_ref = np.average(evoldiag[0:ref_ndays])
    #evol_ref = evol_ma[0]
    if relative:
        evol_ma = 100*(evol_ma - evol_ref)/evol_ref
        plt.plot(dates_ma, evol_ma, color=mycolors[0], linewidth=4, label=lab)
        #plt.text(dates_ma[-1]+timedelta(days=3), evol_ma[-1], lab, fontsize=14, color=mycolors[0])
    
    data = {"Data": dates_ma, lab: evol_ma }
    dataraw = {"Data": mdyn.days_all, lab: evoldiag }
    dataraw_in = {"Data": dates_ma}
    dataraw_out = {"Data": dates_ma}

    lab = "SAINDO DE "+refname
    #plt.plot(mdyn.days_all, evoldiag, color=mycolors[0], linewidth=3, label=lab)
    evol_ma = mex.moving_average(evol_outall)
    dataraw_out[refname]=evol_ma

    evol_ref = np.average(evol_outall[0:ref_ndays])
    #evol_ref = evol_ma[0]
    if relative:
        evol_ma = 100*(evol_ma - evol_ref)/evol_ref
        #evol_ma = 100*(evol_ma - evol_ma[0])/evol_ma[0]
        plt.plot(dates_ma, evol_ma, color=mycolors[1], linewidth=4, label=lab)
        #plt.text(dates_ma[-1]+timedelta(days=3), evol_ma[-1]+1, lab, fontsize=14, color=mycolors[1])
    
    data[lab]=evol_ma
    dataraw[lab]=evol_outall
    
    lab = "ENTRANDO EM "+refname
    #plt.plot(mdyn.days_all, evoldiag, color=mycolors[0], linewidth=3, label=lab)
    evol_ma = mex.moving_average(evol_inall)
    dataraw_in[refname]=evol_ma

    evol_ref = np.average(evol_inall[0:ref_ndays])
    #evol_ref = evol_ma[0]
    if relative:
        evol_ma = 100*(evol_ma - evol_ref)/evol_ref
        #evol_ma = 100*(evol_ma - evol_ma[0])/evol_ma[0]
        plt.plot(dates_ma, evol_ma, color=mycolors[2], linewidth=4, label=lab)
        #plt.text(dates_ma[-1]+timedelta(days=3), evol_ma[-1]-1, lab, fontsize=14, color=mycolors[2])
    data[lab]=evol_ma
    dataraw[lab]=evol_inall
    
    texts = []
    k = 0
    for ki, nb in enumerate(neib):
        lab_out = refname+"->"+nb
        lab_in = refname+"<-"+nb
        evol = evolneib_in[ki, :]

        if np.average(evol) < 100:
            print("Warning: City with small number of trips, removing ", nb)
            continue

        k = k +1
        evol_ma = mex.moving_average(evol)
        dataraw_in[nb] = evol_ma

        evol_ref = np.average(evol[0:ref_ndays])
        #evol_ref = evol_ma[0]
        if relative:
            evol_ma = 100*(evol_ma - evol_ref)/evol_ref
        #evol_ma = 100*(evol_ma - evol_ma[0])/evol_ma[0]
        plt.plot(dates_ma, evol_ma, color=mycolors[3+k], linewidth=1, label=lab_in, linestyle="--")
        texts.append(plt.text(dates_ma[-1]+timedelta(days=7), 
            evol_ma[-1], nb, fontsize=10, color=mycolors[3+k], alpha=0.5))

        data["de "+nb] = evol_ma
        dataraw["de "+nb] = evol
        
        evol = evolneib_out[ki, :]
        evol_ma = mex.moving_average(evol)
        dataraw_out[nb] = evol_ma

        evol_ref = np.average(evol[0:ref_ndays])
        #evol_ref = evol_ma[0]
        if relative:
            evol_ma = 100*(evol_ma - evol_ref)/evol_ref
        #evol_ma = 100*(evol_ma - evol_ma[0])/evol_ma[0]
        plt.plot(dates_ma, evol_ma, color=mycolors[3+k], linewidth=1, label=lab_out, linestyle="-.")
        #plt.text(dates_ma[-1]+timedelta(days=1), evol_ma[-1], lab_out, fontsize=14, color=mycolors[3+k])

        data["para "+nb] = evol_ma
        dataraw["para "+nb] = evol
        

    plt.legend(loc='best', fontsize=12, ncol=2)

    #fancy stuff
    ax = plt.gca()

    ax.yaxis.get_offset_text().set_fontsize(14)
    ax.set_ylabel('viagens - média móvel semanal', fontsize=14)
    if relative:
        ax.yaxis.set_major_formatter(mtick.PercentFormatter())
    #plt.ticklabel_format(axis="y", style="plain", scilimits=(0,0))
    #plt.yscale('log')
    plt.yticks(fontsize=14, alpha=.7)

    plt.xlim(dates_ma[0], dates_ma[-1]+timedelta(days=10))
    xtick_location = mdyn.days_all[::7]
    xtick_labels = days[::7]
    xtick_location.append(xtick_location[-1]+timedelta(days=7))
    xtick_labels.append("")
    plt.xticks(ticks=xtick_location, labels=xtick_labels, ha="right", rotation=45, fontsize=15) #, alpha=.7)
    
    plt.title(refname, fontsize=16)

    plt.grid(axis='both', alpha=.3)
    plt.gca().spines["top"].set_alpha(0.0)    
    plt.gca().spines["bottom"].set_alpha(0.3)
    plt.gca().spines["right"].set_alpha(0.0)    
    plt.gca().spines["left"].set_alpha(0.3)   

    if relative:
        ref_txt = "Referência (média)\n"+days[0]+" a "+days[ref_ndays-1]
        #plt.gcf().text(0.90, 0.05, ref_txt, fontsize=14, 
        plt.gcf().text(0.1, 0.05, ref_txt, fontsize=14, 
            bbox=dict(facecolor='gray', alpha=0.3), horizontalalignment='center', 
            verticalalignment='center', transform=ax.transAxes)

    textstr = "Fonte:\n IME-USP/InLoco"
    plt.gcf().text(0.98, -0.1, textstr, fontsize=12, horizontalalignment='center', 
        verticalalignment='center', transform=ax.transAxes)

    plt.tight_layout() 

    adjust_text(texts, autoalign="y", only_move={'points': 'y',
        'text':'y', 'objects':'y'})
    
    #Save density plot to folder "dump"
    plt.savefig(filename+"evol.jpg", dpi=400)
    #plt.savefig(filename+"evol.tiff", dpi=200)
    plt.close()
    
    #-----------save dataframes -----------------------#

    df_raw = pd.DataFrame(dataraw)
    #df.to_csv(filename+"raw.csv", sep=";")
    df_raw.to_excel(filename+"raw.xls")

    df_perc = pd.DataFrame(data)
    #df.to_csv(filename+"percent.csv", sep=";")
    df_perc.to_excel(filename+"percent.xls")

    #-----------stack plot -----------------------#

    fig, ax = plt.subplots(2, 1, figsize=(20,10), dpi= 300)

    # Data in
    df_tmp = pd.DataFrame(dataraw_in)
    df_tmp['OUTROS'] = df_tmp.drop(['Data', refname], axis=1).sum(axis=1)
    df_tmp = df_tmp.drop([refname], axis=1)
    df_tmp = df_tmp.iloc[:, ::-1]
    ax[0] = df_tmp.plot.area(x='Data', ax=ax[0], alpha=0.5, legend=True)
    ax[0].set_ylabel('VIAGENS PARA '+refname, fontsize=14)
    handles, labels = ax[0].get_legend_handles_labels()
    ax[0].legend(handles[::-1], labels[::-1], bbox_to_anchor=(1.00, 1.00))

    df_tmp = pd.DataFrame(dataraw_out)
    df_tmp['OUTROS'] = df_tmp.drop(['Data', refname], axis=1).sum(axis=1)
    df_tmp = df_tmp.drop([refname], axis=1)
    df_tmp = df_tmp.iloc[:, ::-1]
    ax[1] = df_tmp.plot.area(x='Data', ax=ax[1], alpha=0.5, legend=False)
    ax[1].set_ylabel('VIAGENS DE '+refname, fontsize=14)
    #handles, labels = ax[0].get_legend_handles_labels()
    #ax[1].legend(handles[::-1], labels[::-1], bbox_to_anchor=(1.02, 1.00))

    for i in range(2):
        ax[i].yaxis.get_offset_text().set_fontsize(14)
        ax[i].set_xlabel(' ')
        ax[i].tick_params(axis='both', which='major', labelsize=14)
        ax[i].tick_params(axis='both', which='minor', labelsize=10)
        #ax[0].yaxis.set_major_formatter(mtick.PercentFormatter())
        ax[i].yaxis.get_major_formatter().set_powerlimits((0, 1))
        #plt.ticklabel_format(axis="y", style="sci", scilimits=(0,0))
        #ax[i].yticks(fontsize=14, alpha=.7)
        ax[i].spines["top"].set_alpha(0.0)    
        ax[i].spines["bottom"].set_alpha(0.3)
        ax[i].spines["right"].set_alpha(0.0)    
        ax[i].spines["left"].set_alpha(0.3)   

    textstr = "Fonte:\n IME-USP/InLoco"
    plt.gcf().text(1.10, -0.1, textstr, fontsize=12, horizontalalignment='center', 
        verticalalignment='center', transform=ax[1].transAxes)

    plt.tight_layout() 
    plt.savefig(filename+"stack_evol.jpg", dpi=400)
    
    return

def map_move_mats(mdyn, network, ipar):
    print()
    print("Mapping move mats:")
    #Get movement matrices
    mdyn.collect_move_mat(network)

    #Get isolation indeces
    iso = sd.socialdist(ipar.isoind_file, network) 

    #print(iso.df.columns)
    #print("Regions:", network.regions)
    mat_shape = mdyn.movemats[0].shape
    mat_sum = np.zeros(mat_shape)
    isol_sum = np.zeros([network.nreg_in])

    plot = False
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
        mat_sum = mat_sum + mat

        #filter if set up
        if ipar.filter[0]:
            if ipar.filter[1] == "pop":
                npop = ipar.filter[2]
                pop=np.array(network.reg_pop)
                ipop=list(pop.argsort()[-npop:][::-1])
                print(ipop)
                filter_list=ipop
            if ipar.filter[1] == "list":
                filter_list = ipar.filter_list
        else:
            filter_list=[]

        reg_iso = np.zeros([network.nreg_in])
        for reg in range(network.nreg_in):
            region = regions.get(reg)
            region = str(region)   
            if region in list(df_iso['reg_name'].values): 
                isotmp = df_iso.loc[df_iso['reg_name'] == region, 'iso'].values[0]
            else:
                isotmp = np.nan
            reg_iso[reg] = isotmp
        
        isol_sum = isol_sum + reg_iso

        #Do map
        dow=mex.weekdays[day.weekday()]
        
        if isinstance(ipar.zoom[0], bool): #in this case we have a single zoom
            zooms = [ipar.zoom]
        else: #list of zooms
            zooms = ipar.zoom

        for zoom in zooms:
            if zoom[0]:
                title = network.domain+" "+network.subdomains+" Network Zoom "+zoom[6]+" "
                filename = mdyn.dump_dir+title.replace(" ", "_") #+"_"+str(i+67).zfill(3)+".jpg"
            else:
                title = network.domain+" "+network.subdomains+" Network "
                filename = mdyn.dump_dir+title.replace(" ", "_") #+str(i+67).zfill(3)+".jpg"

            title = title + day.strftime("%Y-%m-%d")+" "+dow
            filename = filename + day.strftime("%Y-%m-%d")+".jpg"

            if plot:
                if "RM" in title:
                    map=Map(network, zoom)
                    map.map_network_data(reg_iso, mat, regions, title, filename)
            
                map=Map(network, zoom)
                map.map_network_flux(mat, regions, title, filename.replace("Network", "Network_Flux"), edge_filter=filter_list)

            #map=Map(network, zoom)
            #map.map_data_on_network(reg_iso, mat, regions, title, filename.replace("Network", "Network_Iso"))

        print("done date.")
        print()

    #save matrix sum
    title_mat = network.domain+" "+network.subdomains+" Network Move Mats "+ mdyn.date_ini+"_"+mdyn.date_end
    filename = mdyn.dump_dir+title_mat.replace(" ", "_") + ".csv"
    print("Saving mat sum as:", filename)
    np.savetxt( filename, mat_sum)

    #save average isolation index
    isol_avg = isol_sum / len(mdyn.days_all)
    title_iso = network.domain+" "+network.subdomains+" Iso Index Average "+ mdyn.date_ini+"_"+mdyn.date_end
    filename = mdyn.dump_dir+title_iso.replace(" ", "_")+ ".csv"
    print("Saving iso index average as:", filename)
    np.savetxt( filename, isol_avg)

    map=Map(network, zoom)
    filename = mdyn.dump_dir+title_mat.replace(" ", "_")+ "IsoFlux.jpg"
    map.map_network_data(isol_avg, mat_sum, regions, title_mat, filename)

    map=Map(network, zoom)
    filename = mdyn.dump_dir+title_mat.replace(" ", "_")+ "Flux.jpg"
    map.map_network_flux(mat_sum, regions, title_mat, filename.replace("Network", "Network_Flux"), edge_filter=filter_list)

    return
        
def centrality_move_mats(mdyn, network, ipar):
    print()
    print("Mapping centrality on move mats:")
    #Get movement matrices
    mdyn.collect_move_mat(network)

    #Get extra data
    df = pd.read_csv("input/arrival_time.csv") 
    print(df)

    #filter if set up
    if ipar.filter[0]:
        if ipar.filter[1] == "pop":
            npop = ipar.filter[2]
            pop=np.array(network.reg_pop)
            ipop=list(pop.argsort()[-npop:][::-1])
            print(ipop)
            filter_list=ipop
        if ipar.filter[1] == "list":
            filter_list = ipar.filter_list
    else:
        filter_list=[]
        
    #print(iso.df.columns)
    #print("Regions:", network.regions)
    
    #Loop for each day
    for i, day in enumerate(mdyn.days_all):
        
        print("Calculating on: ", i, day.strftime("%Y-%m-%d"))
        #print(iso.df['day'].unique(), day.strftime("%Y-%m-%d"))
        
        #filter day, state, regions
        #df_iso = iso.df[iso.df['day']==day.strftime("%Y-%m-%d")]
        
        if network.domain_abrv == "BRA":
            #BRA uses geocodes, so get city names
            regions = network.regions_in_names
        else:
            #Use actual city names
            regions = network.regions
            #Filter state
            #df_iso = df_iso[df_iso['reg_name'].isin(regions.values())]

        mat = mdyn.movemats[i]
        reg_df = np.zeros([network.nreg_in])
        reg_list = df['Município'].values
        
        reg_list = [w.upper() for w in reg_list]
        
        for reg in range(network.nreg_in):
            region = regions.get(reg)
            region = str(region)           
            if region in reg_list: 
                
                valtmp = df.loc[df['Município'].str.upper()  == region, 'arrival10week'].values[0]
            else:
                valtmp = np.nan
            reg_df[reg] = valtmp
            
        #Do map
        dow=mex.weekdays[day.weekday()]
        
        if isinstance(ipar.zoom[0], bool): #in this case we have a single zoom
            zooms = [ipar.zoom]
        else: #list of zooms
            zooms = ipar.zoom

        for zoom in zooms:
            if zoom[0]:
                title = network.domain+" "+network.subdomains+" Network Zoom "+zoom[6]+" "
                filename = mdyn.dump_dir+title.replace(" ", "_") #+"_"+str(i+67).zfill(3)+".jpg"
            else:
                title = network.domain+" "+network.subdomains+" Network "
                filename = mdyn.dump_dir+title.replace(" ", "_") #+str(i+67).zfill(3)+".jpg"

            title = title + day.strftime("%Y-%m-%d")+" "+dow
            filename = filename + day.strftime("%Y-%m-%d")+".jpg"
    
            #map=Map(network, zoom)
            #map.map_network_data(reg_df, mat, regions, title, filename.replace("Network", "Network_Arrival_Time"), node_list=filter_list )
            

            map=Map(network, zoom)
            map.map_network_centrality(mat, regions, title, filename.replace("Network", "Network_Centrality"), edge_filter=filter_list )

def centrality_move_mats_avg(mdyn, network, ipar):
    print()
    print("Mapping centrality on move mats average:")
    #Get movement matrices
    mdyn.collect_move_mat(network)

    #Get extra data
    #df = pd.read_csv("input/arrival_time.csv") 
    #print(df)

    #filter if set up
    if ipar.filter[0]:
        if ipar.filter[1] == "pop":
            npop = ipar.filter[2]
            pop=np.array(network.reg_pop)
            ipop=list(pop.argsort()[-npop:][::-1])
            print(ipop)
            filter_list=ipop
        if ipar.filter[1] == "list":
            filter_list = ipar.filter_list
    else:
        filter_list=[]
        
    #print(iso.df.columns)
    #print("Regions:", network.regions)
    
    #Loop for each day

    #for i, day in enumerate(mdyn.days_all):
    days = mdyn.days_all[0].strftime("%Y-%m-%d")+"_"+mdyn.days_all[-1].strftime("%Y-%m-%d")

    print("Calculating for period: ", days)
        #print(iso.df['day'].unique(), day.strftime("%Y-%m-%d"))
        
        #filter day, state, regions
        #df_iso = iso.df[iso.df['day']==day.strftime("%Y-%m-%d")]
        
    if network.domain_abrv == "BRA":
        #BRA uses geocodes, so get city names
        regions = network.regions_in_names
    else:
        #Use actual city names
        regions = network.regions
        #Filter state
        #df_iso = df_iso[df_iso['reg_name'].isin(regions.values())]


    mat = mdyn.movemats[0]
    mat = mat*0.0
    for mattmp in mdyn.movemats:
        mat = mat + mattmp

        
    if isinstance(ipar.zoom[0], bool): #in this case we have a single zoom
        zooms = [ipar.zoom]
    else: #list of zooms
        zooms = ipar.zoom

    for zoom in zooms:
        if zoom[0]:
            title = network.domain+" "+network.subdomains+" Network Zoom "+zoom[6]+" "
            filename = mdyn.dump_dir+title.replace(" ", "_") #+"_"+str(i+67).zfill(3)+".jpg"
        else:
            title = network.domain+" "+network.subdomains+" Network "
            filename = mdyn.dump_dir+title.replace(" ", "_") #+str(i+67).zfill(3)+".jpg"

        title = title + days
        filename = filename + days+".jpg"

        map=Map(network, zoom)
        map.map_network_centrality(mat, regions, title, filename.replace("Network", "Network_Centrality"), edge_filter=filter_list )



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
    if len(mdyn.days_all) <7:
        print("Warning:  not enough days to calculate average by day of the week")
        print("      some zero matrices may be used.")

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
        if ipar.daily_plots and not os.path.exists(filename):
            print("  Plotting :", filename)
            map=Map(network)
            map.map_move_by_reg(diag_norm, network.regions, network, title_base+"\nDiagonal Prob Move", filename)

        num_source = ipar.num_source
        
        sources = np.argpartition(diag_orig, -num_source)[-num_source:]
        
        print("Main regions:", sources)
        #Plot daily main sources
        if ipar.daily_plots:
            for j in sources:
                title = title_base+"\nOrigin "+str(network.regions[j])
                filename =  mdyn.dump_dir+title.replace('\n','').replace(' ','_')+"_day_prob.jpg"
                if not os.path.exists(filename):
                    print("Creating plot for ", filename)
                    move_vec = mdyn.movemats_norm[i][:, j]
                        
                    map=Map(network)
                    map.map_move_by_reg(move_vec, network.regions, network, title, filename)

        #Plot daily main sources - adjusted matrices
        if ipar.daily_plots and np.sum(network.reg_pop) > 0.0:
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
        if ipar.daily_plots:
            for j in sources:
                title = title_base+"\nOrigin "+str(network.regions[j])
                filename =  mdyn.dump_dir+title.replace('\n','').replace(' ','_')+"_dow_avg_prob.jpg"
                if not os.path.exists(filename):
                    print("Creating plot for ", filename)
                    move_vec = movemat_avg[i][:, j]
                        
                    map=Map(network)
                    map.map_move_by_reg(move_vec, network.regions, network, title, filename)

    return movemat_avg_adj,  movemat_std, movemat_avg_diag

def calc_move_mat_avg_period(mdyn, network, ipar):

    #Period mean move mat per dow
    movemat_avg_adj = np.zeros(mdyn.movemats[0].shape)
    movemat_avg = np.zeros(mdyn.movemats[0].shape)
    
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
            print("Warning, no population to adjust matriz")
            diag_adj = np.diag(mdyn.movemats[i])
            mdyn.movemats_adj[i] = mdyn.movemats[i] 
            mdyn.movemats_adj_norm[i] = mdyn.movemats_norm[i]
        
        title_base = network.domain+" "+network.subdomains+" "+day.strftime("%Y-%m-%d")+" "+mex.weekdays[dow]
                
        movemat_avg_adj = movemat_avg_adj + mdyn.movemats_adj[i]
        movemat_avg = movemat_avg + mdyn.movemats[i]

        #Plot matrix diagonal
        diag_norm = np.diag(mdyn.movemats_norm[i])

        filename =  mdyn.dump_dir+title_base.replace('\n','').replace(' ','_')+"_day_prob_diag.jpg"
        if ipar.daily_plots and not os.path.exists(filename):
            print("  Plotting :", filename)
            map=Map(network)
            map.map_move_by_reg(diag_norm, network.regions, network, title_base+"\nDiagonal Prob Move", filename)

        num_source = ipar.num_source
        
        sources = np.argpartition(diag_orig, -num_source)[-num_source:]
        
        print("Main regions:", sources)
        if ipar.daily_plots:
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
        if np.sum(network.reg_pop) > 0.0 and ipar.daily_plots:
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
    
    #normalize move mat
    if np.sum(np.sum(movemat_avg_adj)) > 0 :
        movemat_avg_adj = movemat_avg_adj / movemat_avg_adj.sum(axis=0)
        movemat_avg = movemat_avg / movemat_avg.sum(axis=0)

    if ipar.daily_plots:
        #Plot avg matrices
        title_base = network.domain+" "+network.subdomains+" "+mdyn.date_ini+" "+mdyn.date_end+" "+mex.weekdays[i]
        filename =  mdyn.dump_dir+title_base.replace('\n','').replace(' ','_')+"avg_prob_move.jpg"
        if np.sum(np.sum((movemat_avg_adj))) > 0 and network.nregions < 20:
            if not os.path.exists(filename):
                mex.plot_matrix(movemat_avg_adj, title_base+"\nMean Move Prob", filename)

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

    return movemat_avg_adj

def simulate_model(mdyn, network, ipar):

    #Analyse movement matrices
    mdyn.collect_move_mat(network)

    movemat_avg, movemat_std, movemat_avg_diag = calc_move_mat_avg_dow(mdyn, network, ipar)
    #movemat_avg = calc_move_mat_avg_period(mdyn, network, ipar)

    #Sources to plot 
    #num_simul_days = ipar.num_simul_days #min(network.nregions-2, 20)
    
    #Initial condition
    data_ini_regv = np.zeros([network.nregions])
    data_ini_reg_names = []
    for key in ipar.data_ini_by_reg:
        data_ini_regv[key] = ipar.data_ini_by_reg[key]
        data_ini_reg_names.append(network.regions_in_names.get(key))

    day_state = data_ini_regv
    ntime = mdyn.days + ipar.num_simul_days
    data_evol = np.zeros((network.nregions, ntime))
    title_base = "Model_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end
    title_base = title_base + "\n_r"+str(ipar.infec_rate).replace(".","") \
        +"_s"+str(ipar.spread_rate).replace(".","")+\
        "_ini_"+str(data_ini_reg_names[0])
    print(title_base.replace("\n", "_"))
    print()

        #+"_s"+str(int(ipar.spread_rate))
    #simulate scenario
    drange = mex.daterange(mdyn.date_ini_obj, mdyn.date_end_obj+timedelta(days=ipar.num_simul_days))
    
    for i, day in enumerate(drange):
    #for j in num_simul_days:
        indx = '{:02d}'.format(i)
        title = title_base+"_day_"+day.strftime("%Y-%m-%d")
        filename = mdyn.dump_dir+title_base+"_day_"+indx+".jpg"
        if ipar.daily_plots and not os.path.exists(filename):
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
            #mat = movemat_avg
            
        day_state = model(day_state, mat, ipar, network)

        sumv = np.sum(day_state)
        maxv = np.max(day_state)
        minv = np.min(day_state)
        print(day, "Num infected, avg, max, min:", sumv, np.average(day_state), maxv, minv)
        
        if maxv > np.sum(network.reg_pop):
            print( "Too many people infected, reached the limit of the model")
            break

    #plot last day
    filename = mdyn.dump_dir+title_base+"_day_"+indx+".jpg"
    filename=filename.replace("\n", "_")
    
    print("Creating plot  ", filename)
    print()    
    map=Map(network, ipar.zoom[0])
    map.map_move_by_reg(day_state, network.regions, network, title, filename)

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

    #title = title_base+"_risk_time_with_lim_"+str(ipar.risk_lim)
    #filename = mdyn.dump_dir+title_base+"_risk_lim_"+str(ipar.risk_lim)+".jpg"
    #print(" Plotting risk time ", filename)
    #map=Map(network)
    #map.map_move_by_reg(risk_time, network.regions, network, title, filename)

    #title = title_base+"_risk_index"
    #filename = mdyn.dump_dir+title_base+"_risk_index.jpg"
    #print(" Plotting risk index ", filename)
    #map=Map(network)
    #map.map_move_by_reg(risk_index, network.regions, network, title, filename)


def decomposition_model(mdyn, network, ipar):

    #Read city vectors
    nkeycities = len(mex.key_cities)
    keycity_names = []
    keycity_ind = []
    keycity_vector = []
    A = np.zeros((network.nreg_in, nkeycities))
    keycity_evol = []
    for i, cityind in enumerate(mex.key_cities):
        keycity_ind.append(cityind)
        keycity_names.append(network.regions_in_names.get(cityind))
        title_base = "Model_"+network.domain+"_"+network.subdomains+"_"+mdyn.date_ini+"_"+mdyn.date_end
        title_base = title_base + "_r"+str(ipar.infec_rate).replace(".","") \
            +"_s"+str(ipar.spread_rate).replace(".","")+\
            "_ini_"+str(keycity_names[i])
        #data_dir = mdyn.dump_dir
        data_dir = "covid/simulations/"
        filename = data_dir+title_base+"data_evol.npy"
        
        if os.path.exists(filename):
            keycity_evol.append(np.load(filename)) #save full evolution
            keycity_vector.append(keycity_evol[i][:,-2]) #save last column with valid data
            A[:, i]=keycity_vector[i]
            print("city:", i, keycity_ind[i], keycity_names[i], filename, "loaded file.")
        else:
            print("city:", i, keycity_ind[i], keycity_names[i], filename, "did not find file.")
            print("Please run simulator with same parameters before running decomposition")
            sys.exit()
        nregin, ndays = keycity_evol[i].shape

    
    #Pseudo-invese matrix for projection operator
    P = np.linalg.pinv(A)
    
    title_base = "Decomp_"+network.domain+"_"+network.subdomains
    print()
    print(title_base.replace("\n", ""))
    print()

    #read covid data
    filename = "covid/obitos.csv"
    deaths = pd.read_csv(filename)
    filename = "covid/casos.csv"
    covid = pd.read_csv(filename)
    
    covid = covid.set_index('ibgeID')
    deaths = deaths.set_index('ibgeID')
    print(len(covid), len(deaths))
    regions=network.regions_in_codes
    df_regions = pd.DataFrame({'regions':regions , 'ibge_regions':regions})
    df_regions = df_regions.set_index("regions")
    print(len(df_regions))

    covid = pd.concat([df_regions, covid], axis=1, sort=False)
    deaths = pd.concat([df_regions, deaths], axis=1, sort=False)
    
    print(covid.columns)


    drange = mex.daterange(mdyn.date_ini_obj, mdyn.date_end_obj+timedelta(days=ipar.num_simul_days))
    ndays = (mdyn.date_end_obj+timedelta(days=ipar.num_simul_days) - mdyn.date_ini_obj).days
    
    covid_proj = np.zeros((nkeycities, ndays))
    
    days_all = []
    days = []
    for i, day in enumerate(drange):
        days_all.append(day)
        indx = '{:02d}'.format(i)
        day_str = day.strftime("%Y-%m-%d")
        days.append(day_str)

        #covidvec=deaths[day_str].values
        covidvec=covid[day_str].values
        where_are_NaNs = np.isnan(covidvec)
        covidvec[where_are_NaNs] = 0
        vec = np.matmul(P, covidvec)
        covid_proj[:,i] = vec / np.sum(vec)


    # Draw Plot
    fig = plt.figure(figsize=(20,10), dpi= 300)
    mycolors = [
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        'tab:red', 'tab:blue', 'tab:green', 
        'tab:orange', 'tab:brown', 'tab:grey', 'tab:pink', 'tab:olive', 
        'red', 'steelblue', 'firebrick', 'mediumseagreen', 'red', 'blue', 'green', 'black', 'purple',
        ]      

    x=days_all
    texts = []
    for i, city in enumerate(keycity_names):        
        y=covid_proj[i,:]
        plt.plot(x, y, color=mycolors[i], linewidth=2, label=city)
        texts.append(plt.text(days_all[-1]+timedelta(days=7), 
            y[-1], city, fontsize=10, color=mycolors[i], alpha=0.5))

    plt.legend(loc='best', fontsize=12, ncol=2)

    #fancy stuff
    ax = plt.gca()

    ax.yaxis.get_offset_text().set_fontsize(14)
    ax.set_ylabel('Projection coefficient relative to spreader', fontsize=14)
    #ax.set_yscale('log')

    plt.yticks(fontsize=14, alpha=.7)

    plt.xlim(days_all[0], days_all[-1]+timedelta(days=10))
    plt.ylim(0, 0.5)
    xtick_location = days_all[::7]
    xtick_labels = days[::7]
    xtick_location.append(xtick_location[-1]+timedelta(days=7))
    xtick_labels.append("")
    plt.xticks(ticks=xtick_location, labels=xtick_labels, ha="right", rotation=45, fontsize=15) #, alpha=.7)
    
    #plt.title(refname, fontsize=16)

    plt.grid(axis='both', alpha=.3)
    plt.gca().spines["top"].set_alpha(0.0)    
    plt.gca().spines["bottom"].set_alpha(0.3)
    plt.gca().spines["right"].set_alpha(0.0)    
    plt.gca().spines["left"].set_alpha(0.3)   

    textstr = "Fonte:\n IME-USP"
    plt.gcf().text(0.98, -0.1, textstr, fontsize=12, horizontalalignment='center', 
        verticalalignment='center', transform=ax.transAxes)

    plt.tight_layout() 

    adjust_text(texts, autoalign="y", only_move={'points': 'y',
        'text':'y', 'objects':'y'})
    
    #Save density plot to folder "dump"
    filename = mdyn.dump_dir+title_base
    plt.savefig(filename+"evol.jpg", dpi=400)
    #plt.savefig(filename+"evol.tiff", dpi=200)
    plt.close()
    

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
        print("    Non source: avg,max,min :",np.average(tmp_state), np.max(tmp_state), np.min(tmp_state))
        print()
    return day_state

