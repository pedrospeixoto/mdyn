#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn
import sys
import os
import pyarrow.orc as orc
import numpy as np
import pandas as pd
import geopandas as gpd
import scipy as scp

import math
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.cm as mplcm
from matplotlib.ticker import ScalarFormatter
from matplotlib.ticker import FormatStrFormatter

import statsmodels.api as sm

dump_dir = "covid/figures/"

def lift(x,y, quartiles=[0, 50, 75, 100]):
    #print()
    #print("Calculating lift")
    xquartiles = []
    yquartiles = []    
    #print("Quartile, x, y")
    for i, q in enumerate(quartiles):
        xquartiles.append(np.percentile(x, q))
        yquartiles.append(np.percentile(y, q))
        #print(q, xquartiles[i], yquartiles[i])

    tab = np.zeros((len(quartiles)-1,len(quartiles)-1))
    x_color = np.copy(x)
    y_color = np.copy(y)
    #print("i, j, marginals")
    
    for j in range(len(quartiles)-1):
        for i in range(len(quartiles)-1):
            if xquartiles[i] == xquartiles[i+1]:
                x_bool = x == xquartiles[i]
            else:
                x_bool = (x >= xquartiles[i]) & (x <  xquartiles[i+1])

            if yquartiles[j] == yquartiles[j+1]:
                y_bool = y == yquartiles[j]
            else:
                y_bool = (y >= yquartiles[j]) & (y <  yquartiles[j+1])
            x_color[x_bool] = i+10*j
            y_color[y_bool] = i+10*j
            #print(i,j, np.count_nonzero(x_bool)/len(y), np.count_nonzero(y_bool)/len(y))
            tab[j,i] = np.count_nonzero(x_bool*y_bool)

    print("observed")
    print(tab)
    if tab.shape == (2,2):
        stat, p = scp.stats.fisher_exact(tab)
        stat_tmp, p_tmp, dof, expected = scp.stats.chi2_contingency(tab)
    else:
        stat, p, dof, expected = scp.stats.chi2_contingency(tab)

    print("Expected")
    print(expected)
    print("p", p, stat)

    lift_mat = (tab-expected)/expected
    print("lift mat")
    print(lift_mat)

    return lift_mat, p, xquartiles, yquartiles

def plot(x,y, namex, namey, title=""):
    fig = plt.figure(figsize=(10, 7.0))
    ax = plt.gca() 
    #ax.set_aspect('equal', 'box')
    plt.scatter(x, y, s=1)
    exp = False
    if exp:
        ax.set_yscale('log')
        ax.set_xscale('log')
    plt.xlabel(namex , fontsize=14)
    plt.ylabel(namey, fontsize=14)
    plt.title(title, fontsize=15)
    #plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
    plt.savefig(dump_dir+"covid_"+namex+"_"+namey+"_"+title+".jpg", dpi=300)
    plt.close()

def plot_lift(lift_mat, quartilesx, quartilesy, namex="", namey="", title="", stats=""):

    n= len(quartilesx)
    quartx = []
    quarty = []
    for i in range(n-1):
        quartx.append("["+str(np.round(quartilesx[i],1))+"-"+str(np.round(quartilesx[i+1],1))+"]")
        quarty.append("["+str(np.round(quartilesy[i],1))+"-"+str(np.round(quartilesy[i+1],1))+"]")

    quarty=quarty[::-1]

    fig = plt.figure(figsize=(10, 7.0))
    ax = plt.gca() 
    lift_mat = lift_mat*100
    lift_mat = lift_mat[::-1, :]
    plt.set_cmap('bwr')
    vmin = np.min(lift_mat)
    vmax = np.max(lift_mat)
    if vmin == vmax:
        return
    norm = mcolors.DivergingNorm(vmin=vmin, vcenter=0, vmax=vmax)
    plt.imshow(lift_mat, vmin=vmin, vmax=vmax, norm=norm) #, cmap=plt.cm.hot)

    cbar = plt.colorbar()
    cbar.set_label('(Obs-Exp)/Exp (%)', rotation=270, labelpad=10)
    

    plt.xlabel(namex , fontsize=14)
    plt.ylabel(namey, fontsize=14)
    plt.title(title, fontsize=15)
    ax.set_xticks(np.arange(0, len(quartx), 1))
    ax.set_yticks(np.arange(0, len(quarty), 1))
    ax.set_xticklabels(quartx, fontsize=8)
    ax.set_yticklabels(quarty, fontsize=8)
    #plt.tight_layout() #pad=0.4, w_pad=0.5, h_pad=1.0)
    plt.gcf().text(0.02, 0.01, stats, fontsize=8)
    plt.savefig(dump_dir+"lift_"+namex+"_"+namey+"_"+title+".jpg", dpi=300)
    plt.close()