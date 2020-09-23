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

    #print("observed")
    #print(tab)
    stat, p, dof, expected = scp.stats.chi2_contingency(tab)
    #print("Expected")
    #print(expected)
    #print("p", p)

    lift_mat = (tab-expected)/expected
    #print("lift mat")
    #print(lift_mat)
    return lift_mat, p, xquartiles, yquartiles