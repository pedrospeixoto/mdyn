#! /usr/bin/env python3
# conda enviroment
# conda activate mdyn

import sys
import os
import warnings
warnings.filterwarnings("ignore")

from incidence_main import Incidence

#Initialize mobile data and load data to dataframe 
dengue=Incidence(sys.argv)

dengue.read_data()

dengue.map()

