#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
import warnings
warnings.filterwarnings("ignore")

from mdyn_main import MobileDynamics

#Initialize mobile data and load data to dataframe 
mdyn=MobileDynamics(sys.argv)

#Calculate analytics
#mdyn.build_analytics_data(mode="all", state="SP")
mdyn.build_model(mode="reg", state="SP", precompdomain = False)

mdyn.simulate(mode="reg")
