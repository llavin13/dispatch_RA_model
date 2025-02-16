# -*- coding: utf-8 -*-
"""
Created on Sun Feb 10 13:50:50 2019

@author: llavi
"""

#general imports
from __future__ import division
import os
import glob
from os.path import join
import pandas as pd
import numpy as np
import math
import time
import sys
import datetime
from pyutilib.services import TempfileManager
from pyomo.environ import *
from pyomo.opt import SolverFactory
import matplotlib.pyplot as plt

#import other scripts
import input_data
import model_script
import write_results
import create_init
import plotting

from case_inputs import case_folder


'''
explain purpose and use of model here
'''

start_time = time.time()
cwd = os.getcwd()

#folder where cases are pulled from, should only need to activate for TOY
#case_folder = "TOY"

## CREATE SINGLE SCENARIO ##
#currently inactive
#scenario_name = "TOY" #for now
#for loading data from last timeperiod case to initialize this case
#load_init = False
#load_dir = "TEST2"  

#
reference_folder = "Jan_4_10_2014_DynamicORDCMRR_CHECK2"
#reference_folder = "Oct_19_25_2017_DynamicORDCMRR"
use_reference_folder = True

## CREATE LINKED SCENARIO OVER MULTIPLE DAYS ##
#enter this as a list of tuples (probably can automate this / connect to case_inputs.py at some point)
scenario_list = [("1.4.2014",False,""),("1.5.2014",True,"1.4.2014"),("1.6.2014",True,"1.5.2014"),("1.7.2014",True,"1.6.2014"),
                 ("1.8.2014",True,"1.7.2014"),("1.9.2014",True,"1.8.2014"),("1.10.2014",True,"1.9.2014")]
#scenario_list = [("1.4.2014",False,""),("1.5.2014",True,"1.4.2014"),("1.6.2014",True,"1.5.2014")]
#scenario_list = [("10.19.2017",False,"")]
#scenario_list = [("10.19.2017",False,""),("10.20.2017",True,"10.19.2017"),("10.21.2017",True,"10.20.2017"),("10.22.2017",True,"10.21.2017"),
#                 ("10.23.2017",True,"10.22.2017"),("10.24.2017",True,"10.23.2017"),("10.25.2017",True,"10.24.2017")]

#scenario_list = [("1.22.2014",True,"1.21.2014"),("1.23.2014",True,"1.22.2014"),
#                 ("1.24.2014",True,"1.23.2014"),("1.25.2014",True,"1.24.2014"),("1.26.2014",True,"1.25.2014")]
#scenario_list = [("1.9.2014",True,"1.8.2014"),("1.10.2014",True,"1.9.2014")] 

#scenario_list = [("1.4.2014",False,"")]
#scenario_list = [("TOY",False,"")]
#scenario_list = [("10.19.2017",False,"")]
#scenario_list = [("10.9.2017",True,"10.8.2017")]
#scenario_list = [("10.1.2017",False,""),("10.2.2017",True,"10.1.2017"),
#                 ("10.3.2017",True,"10.2.2017"),("10.4.2017",True,"10.3.2017"),
#                 ("10.5.2017",True,"10.4.2017"),("10.6.2017",True,"10.5.2017"),
#                 ("10.7.2017",True,"10.6.2017"),("10.8.2017",True,"10.7.2017"),
#                 ("10.9.2017",True,"10.8.2017"),("10.10.2017",True,"10.9.2017"),
#                 ("10.11.2017",True,"10.10.2017"),("10.12.2017",True,"10.11.2017"),
#                 ("10.13.2017",True,"10.12.2017"),("10.14.2017",True,"10.13.2017"),
#                 ("10.15.2017",True,"10.14.2017"),("10.16.2017",True,"10.15.2017"),
#                 ("10.17.2017",True,"10.16.2017"),("10.18.2017",True,"10.17.2017")]
#scenario_list = [("10.25.2017",True,"10.24.2017"),
#                 ("10.26.2017",True,"10.25.2017"),("10.27.2017",True,"10.26.2017"),
#                 ("10.28.2017",True,"10.27.2017"),("10.29.2017",True,"10.28.2017"),
#                 ("10.30.2017",True,"10.29.2017"),("10.31.2017",True,"10.30.2017")]
#scenario_list = [("10.21.2017",True,"10.20.2017"),("10.22.2017",True,"10.21.2017"),
#                 ("10.23.2017",True,"10.22.2017"),("10.24.2017",True,"10.23.2017"),
#                 ("10.25.2017",True,"10.24.2017"),("10.26.2017",True,"10.25.2017"),
#                 ("10.27.2017",True,"10.26.2017"),("10.28.2017",True,"10.27.2017"),
#                 ("10.29.2017",True,"10.28.2017"),("10.30.2017",True,"10.29.2017")]
#scenario_list = [("10.19.2017",False,"")]
#scenario_list = [("10.20.2017.nordc",True,"10.19.2017.nordc"),("10.21.2017.nordc",True,"10.20.2017.nordc"),
#                 ("10.22.2017.nordc",True,"10.21.2017.nordc"),("10.23.2017.nordc",True,"10.22.2017.nordc"),
#                 ("10.24.2017.nordc",True,"10.23.2017.nordc"),("10.25.2017.nordc",True,"10.24.2017.nordc")]
#
#scenario_list = [("10.25.2017",True,"10.24.2017")]


# Allow user to specify solver path if needed (default assumes solver on path)
executable=""

#Directory structure, using existing files rather than creating case structure for now
class DirStructure(object):
    """
    Create directory and file structure.
    """
    def __init__(self, code_directory, case_folder, load_init, load_dir):
        self.DIRECTORY = code_directory
        #self.DIRECTORY = os.path.join(self.CODE_DIRECTORY, "..")
        self.CASE_DIRECTORY = os.path.join(self.DIRECTORY, case_folder)
        self.INPUTS_DIRECTORY = os.path.join(self.CASE_DIRECTORY, scenario_name, "inputs")
        self.RESULTS_DIRECTORY = os.path.join(self.CASE_DIRECTORY, scenario_name, "results")
        self.LOGS_DIRECTORY = os.path.join(self.DIRECTORY, "logs")
        if load_init:
            self.INIT_DIRECTORY = os.path.join(self.CASE_DIRECTORY, load_dir, "results")

    def make_directories(self):
        if not os.path.exists(self.RESULTS_DIRECTORY):
            os.mkdir(self.RESULTS_DIRECTORY)
        if not os.path.exists(self.LOGS_DIRECTORY):
            os.mkdir(self.LOGS_DIRECTORY)
            
    def subproblem_directories(self,reference_folder):
        self.COMPARISON_INPUTS_DIRECTORY = os.path.join(self.DIRECTORY,reference_folder,scenario_name,"inputs")
            
# Logging
class Logger(object):
    """
    The print statement will call the write() method of any object you assign to sys.stdout,
    so assign the terminal (stdout) and a log file as output destinations.
    """
    def __init__(self, directory_structure):
        self.terminal = sys.stdout
        self.log_file_path = os.path.join(directory_structure.LOGS_DIRECTORY,
                                          datetime.datetime.now().strftime('%Y-%m-%d_%H-%M-%S') + "_" +
                                          str(scenario_name) + ".log")
        self.log_file = open(self.log_file_path, "w", buffering=1)

    def write(self, message):
        self.terminal.write(message)
        self.log_file.write(message)

    def flush(self):
        self.terminal.flush()
        self.log_file.flush()

            
def create_problem_instance(scenario_inputs_directory, load_init, scenario_from_directory):
    """
    Load model formulation and data, and create problem instance.
    """
    # Get model, load data, and solve
    print ("Reading model...")
    model = model_script.dispatch_model
    print ("...model read.")
    
    if load_init:
        print("Creating initial conditions data file...")
        create_init.create_init_file(scenario_from_directory, scenario_inputs_directory, 24)
        print("...initial conditions created.")
    
    print ("Loading data...")
    data = input_data.scenario_inputs(scenario_inputs_directory)
    print ("..data read.")
    
    print ("Compiling instance...")
    instance = model.create_instance(data)
    print ("...instance created.")

    # example code for debugging via printing output
    # getattr(instance, 'MAIN_ZONE_THERMAL_RESOURCES').pprint()

    return instance

def solve(instance, case_type):
    """
    Select solver for the problem instance
    Run instance of model
    """
    #Choose active/inactive objective
    if case_type == "MIP":
        instance.TotalCost2.deactivate() #deactivates the simple objective
    elif case_type == "LP":
        instance.TotalCost2.activate()
        instance.TotalCost.deactivate() #switch objective to exclude start-up and no-load costs
        instance.BindSegmentReserveConstraint.deactivate()
        instance.PminConstraint.deactivate()
    elif case_type == 'CompareLP':
        instance.TotalCost.deactivate()
        instance.TotalCost2.activate()
        instance.BindSegmentReserveConstraint.deactivate()
        instance.PminConstraint.deactivate()
    # ### Solve ### #
    if executable != "":
        solver = SolverFactory("cplex", executable=executable)
        #solver.options['mip_tolerances_absmipgap'] = 0.2 #sets mip optimality gap, which is 1e-06 by default
        #solver.options['mip_tolerances_mipgap'] = 0.01
        #solver.options['parallel'] = -1 #opportunistic
        #solver.options['dettimelimit'] = 1000000
    else:
        solver = SolverFactory("cplex") 
        #solver = SolverFactory("gurobi")
        #solver.options['mip_tolerances_absmipgap'] = 0.2
        #solver.options['mip_tolerances_mipgap'] = 0.0005
        #solver.options['parallel'] = -1 #opportunistic
        #solver.options['dettimelimit'] = 1000000
        
    print ("Solving...")
    
    # to keep human-readable files for debugging, set keepfiles = True
    
    try:
        solution = solver.solve(instance, tee=True, keepfiles=False)

        #solution = solver.solve(instance, tee=True, keepfiles=False, options={'optimalitytarget':1e-5})
    except PermissionError:
        print("Yuck, a permission error")
        for file in glob.glob("*.log"):
            print("removing log files due to Permission Error")
            file_path = open(file)
            file_path.close()
            time.sleep(1)
            os.remove(file)
        return solve(instance, case_type)
    
    return solution

def load_solution(instance, results):
    """
    Load results.
    """
    instance.solutions.load_from(results)

def run_scenario(directory_structure, load_init):
    """
    Run a scenario.
    """

    # Directories
    scenario_inputs_directory = os.path.join(directory_structure.INPUTS_DIRECTORY)
    scenario_results_directory = os.path.join(directory_structure.RESULTS_DIRECTORY) #results not needed yet
    scenario_logs_directory = os.path.join(directory_structure.LOGS_DIRECTORY)
    if load_init:
        scenario_createinputs_directory = os.path.join(directory_structure.INIT_DIRECTORY)
    else:
        scenario_createinputs_directory = None
    
    #additional directory
    reference_scenario_inputs_directory = os.path.join(directory_structure.COMPARISON_INPUTS_DIRECTORY)
    
    # Write logs to this directory
    TempfileManager.tempdir = scenario_logs_directory

    # Create problem instance
    instance = create_problem_instance(scenario_inputs_directory, load_init, scenario_createinputs_directory)
            
    
    # Create a 'dual' suffix component on the instance, so the solver plugin will know which suffixes to collect
    #instance.dual = Suffix(direction=Suffix.IMPORT)
    
    solution = solve(instance,"MIP") #solve MIP with commitment

    print ("Done running MIP, relaxing to LP to obtain duals...")
     
    #fix binary variables to relax to LP
    instance.commitment.fix()
    instance.startup.fix() 
    instance.shutdown.fix()
    instance.preprocess()   
    instance.dual = Suffix(direction=Suffix.IMPORT) 
    solution = solve(instance,"LP") 
    
    #export objective function value
    objective_list = [value(instance.TotalCost2)]
    objective_index = ['OriginalObjectiveValue']

    ###

    # export other results to csv
    write_results.export_results(instance, solution, scenario_results_directory, debug_mode=1)
    
    # THE REST OF THIS LOOP IS ONLY NEEDED FOR PLOTTING RESULTS
    #load up the instance that was just solved
    load_solution(instance, solution)
    #instance.solutions.load_from(solution)
    #write it to an array
    #eventually this should be converted to real results writing, 
    #but for not it's just a single result
    #so OK to do this
    results_dispatch = []
    results_starts = []
    results_shuts = []
    tmps = []
    zone_stamp = []
    results_wind = []
    results_solar = []
    results_curtailment = []
    price_duals = []
    reserve_duals = []
    results_synchreserves = []
    transmission_duals = []
    results_transmission_line_flow = []
    results_nonsynchreserves = []
    results_secondaryreserves = []
    results_subzone_reserves = []
    results_tx_subzone_reserves = []

    for t in instance.TIMEPOINTS:
        tmps.append(instance.TIMEPOINTS[t])
        reserve_duals.append(instance.dual[instance.TotalSynchReserveConstraint[t]])
        results_tx_subzone_reserves.append(instance.txsubzonecontribution[t].value)
        
        for line in instance.TRANSMISSION_LINE:
            transmission_duals.append(instance.dual[instance.TxFromConstraint[t,line]] +\
                                      instance.dual[instance.TxToConstraint[t,line]])
            results_transmission_line_flow.append(instance.transmit_power_MW[t,line].value)
        
        for z in instance.ZONES:
            results_wind.append(instance.windgen[t,z].value)
            results_solar.append(instance.solargen[t,z].value)
            results_curtailment.append(instance.curtailment[t,z].value)
            price_duals.append(instance.dual[instance.LoadConstraint[t,z]])

            
            for g in instance.GENERATORS:
                results_dispatch.append(instance.dispatch[t,g,z].value)
        for g in instance.GENERATORS:
            results_starts.append(instance.startup[t,g].value)
            results_shuts.append(instance.shutdown[t,g].value)
            results_synchreserves.append(instance.synchreserves[t,g].value)
            results_nonsynchreserves.append(instance.nonsynchreserves[t,g].value)
            results_secondaryreserves.append(instance.secondaryreserves[t,g].value)
        for g_sz in instance.SUB_ZONE_GENERATORS:
            results_subzone_reserves.append(instance.synchreserves[t,g_sz].value)
            
    zones = pd.read_csv(join(dir_str.INPUTS_DIRECTORY, 'zones.csv'))
    for z in zones['zone']:
        zone_stamp.append(z)
    
    ### ###
    #re run model
    instance2 = create_problem_instance(reference_scenario_inputs_directory, load_init, scenario_createinputs_directory)
    instance.segmentdispatch.fix()
    instance.synchreserves.fix()
    instance.nonsynchreserves.fix()
    instance.secondaryreserves.fix()
    instance.transmit_power_MW.fix()
    instance.dispatch.fix()
    instance.commitment.fix()
    instance.startup.fix() 
    instance.shutdown.fix()
    instance.preprocess()
    
    for t in instance.TIMEPOINTS:
        for s in instance.SEGMENTS:
            instance.SynchMW[t,s].value = instance2.SynchMW[t,s].value
            instance.NonSynchMW[t,s].value = instance2.NonSynchMW[t,s].value
            instance.SecondaryMW[t,s].value = instance2.NonSynchMW[t,s].value
            instance.price[t,s].value = instance2.price[t,s].value
    solution2 = solve(instance,"CompareLP")
    
    ###
    
    #export objective function value
    objective_list.append(value(instance.TotalCost2))
    objective_index.append('ComparisonObjectiveValue')
    df = pd.DataFrame(objective_list, index=pd.Index(objective_index))
    df.to_csv(os.path.join(scenario_results_directory,"comparison_objective_function_value.csv"))
    
    ### ###
    
    return (results_dispatch, len(tmps), results_wind, results_solar, results_curtailment, results_starts,\
            results_shuts, price_duals, reserve_duals, results_synchreserves, len(zone_stamp),\
            transmission_duals, results_transmission_line_flow, results_nonsynchreserves, results_secondaryreserves,
            results_subzone_reserves,results_tx_subzone_reserves)

### RUN MODEL ###
count_case = 0
for s in scenario_list:
    count_case+=1
    #initialize scenario data in the tuple
    scenario_name = s[0] #for now
    load_init = s[1]
    load_dir = s[2]
    
    #run the case, as usual
    code_directory = cwd
    dir_str = DirStructure(code_directory, case_folder, load_init, load_dir)
    dir_str.make_directories()
    if use_reference_folder:
        dir_str.subproblem_directories(reference_folder)
    logger = Logger(dir_str)
    log_file = logger.log_file_path
    print("Running scenario " + str(count_case) + " of " + str(len(scenario_list)) + "...")
    print ("Running scenario " + str(scenario_name) + "...")
    stdout = sys.stdout
    sys.stdout = logger
    
    scenario_results = run_scenario(dir_str, load_init)
    
    sys.stdout = stdout #return to original
        
    # run diagnostic plots
    plotting.diagnostic_plots(scenario_results, dir_str)

    end_time = time.time() - start_time
    print ("time elapsed during run is " + str(end_time) + " seconds")