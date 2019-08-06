# -*- coding: utf-8 -*-
"""
Created on Fri Mar  8 13:40:19 2019

@author: llavi
"""

#general imports
import os
import glob
from os.path import join
import pandas as pd
import numpy as np
import math
import time
import sys
import datetime

#other scripts that are used
import raw_data_imports
import ordc_sandbox
#import create_ordc
import data_to_csvs

#load user specified inputs for cases
from case_inputs import *

start_time = time.time()
cwd = os.getcwd()

#Directory structure for creation of files
class DirStructure(object):
    """
    Create directory and file structure for the case
    """
    def __init__(self, code_directory, case_folder):
        self.DIRECTORY = code_directory
        self.INPUTS_DIRECTORY = os.path.join(self.DIRECTORY, "raw_data")
        self.SCENARIO_INPUTS_DIRECTORY = os.path.join(self.INPUTS_DIRECTORY, "case_creation_input")
        self.SCENARIO_DIRECTORY = os.path.join(self.DIRECTORY, case_folder)
        
    def case_directories(self):
        self.CASE_DIRECTORY = os.path.join(self.SCENARIO_DIRECTORY, scenario_name)
        self.RESULTS_DIRECTORY = os.path.join(self.CASE_DIRECTORY, "inputs")

    def make_directories(self):
        if not os.path.exists(self.SCENARIO_DIRECTORY):
            os.mkdir(self.SCENARIO_DIRECTORY)
        if not os.path.exists(self.CASE_DIRECTORY):
            os.mkdir(self.CASE_DIRECTORY)
        if not os.path.exists(self.RESULTS_DIRECTORY):
            os.mkdir(self.RESULTS_DIRECTORY)

## USER CHOICES FOR CASE CREATION ##  
#currently done in input csvs, not here

## LOAD RELEVANT DATA ##
code_directory = cwd

#create input data directory
dir_str = DirStructure(code_directory, case_folder)
    
# iteraate over dates specified in case_inputs.py
for date in dates:
    
    make_init = make_init_list[dates.index(date)]
    
    print("creating case for " + str(date) + "...")
    if make_init:
        print("make_init is " + str(make_init) + " for this case, so it should be the day that initializes your run")
    else:
        print("make_init is " + str(make_init) + " for this case, so it should draw on a prior case to initialize its dispatch when run")
    
    
    scenario_name = date
    
    #create directory for storing case data
    dir_str.case_directories()
    dir_str.make_directories()

    # reformat month of case
    month_rename_list = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"] #this must be same as scheduled.outage.rate.by.gen.type.FULL.PERIOD.032519 month naming convention
    numeric_month = int(scenario_name.split('.')[0]) #assumes date will list month first, as is standard in USA
    month = month_rename_list[numeric_month-1] #-1 b/c Python lists index from 0
    
    #pull case-level data
    data = raw_data_imports.load_data(os.path.join(dir_str.INPUTS_DIRECTORY), os.path.join(dir_str.SCENARIO_INPUTS_DIRECTORY), date)
    
    #run and create the main set of csvs
    data_to_csvs.write_data(data, dir_str.RESULTS_DIRECTORY, make_init, dir_str.SCENARIO_INPUTS_DIRECTORY, date, 
                            os.path.join(dir_str.INPUTS_DIRECTORY),primary_reserve_scalar, secondary_reserve_scalar)
    
    #create an additional dynamic ORDC file, if desired
    if create_supp_ordc:
        ordc_df = ordc_sandbox.load_and_run_ordc(dir_str.INPUTS_DIRECTORY, dir_str.RESULTS_DIRECTORY,
                                                month, hydro_cf, VOLL, lowcutLOLP, n_segments, dynamic_ORDC,
                                                date, primary_reserve_scalar, secondary_reserve_scalar)
        ordc_df.to_csv(os.path.join(dir_str.RESULTS_DIRECTORY,"full_ordc.csv"), index=False)
    elif PJM_reserve_heuristic:
        print('you have chosen PJM heuristic reserve requirements (will be based on inputs)')
        ordc_df = ordc_sandbox.PJM_reserves(dir_str.INPUTS_DIRECTORY, dir_str.RESULTS_DIRECTORY,
                                      n_segments, lfe, FOR_fe, contingency)
        ordc_df.to_csv(os.path.join(dir_str.RESULTS_DIRECTORY,"full_ordc.csv"), index=False)
    else:
        print('you have chosen a heuristic ORDC')
        print('you have chosen 0 reserves, so *NO* reserve requirements at all')
        ordc_df = ordc_sandbox.no_ordc(dir_str.INPUTS_DIRECTORY, dir_str.RESULTS_DIRECTORY,
                                      n_segments)
        ordc_df.to_csv(os.path.join(dir_str.RESULTS_DIRECTORY,"full_ordc.csv"), index=False)
        
#how long?
end_time = time.time() - start_time
print ("time elapsed during run is " + str(end_time) + " seconds")