# -*- coding: utf-8 -*-
"""
Created on Fri Mar  8 09:24:14 2019

@author: llavi
"""

import os
from os.path import join
import traceback
import sys
import pdb
import pandas as pd
import numpy as np

def export_results(instance, results, results_directory, debug_mode):
    """
    Retrieves the relevant sets over which it will loop, then call functions to export different result categories.
    If an exception is encountered, log the error traceback. If not in debug mode, exit. If in debug mode,
    open an interactive Python session that will make it possible to try to correct the error without having to re-run
    problem; quitting the interactive session will resume running the next export function, not exit.
    :param instance: the problem instance
    :param results: the results
    :param results_directory: directory to export results files to
    :param prod_cost_inputs_directory: directory that contains production cost model inputs
    :param debug_mode:
    :return:
    """

    print ("Exporting results... ")

    # First, load solution
    load_solution(instance, results)

    # Get sets
    # Sort the sets to return a predictable format of results files
    timepoints_set = sorted(instance.TIMEPOINTS)
    generators_set = instance.GENERATORS #don't sort these so order is preserved for future cases
    ordc_segments_set = sorted(instance.SEGMENTS)
    zones_set = sorted(instance.ZONES)
    transmission_lines_set = sorted(instance.TRANSMISSION_LINE)
    generatorsegment_set = sorted(instance.GENERATORSEGMENTS)
    
    # Call various export functions, throw debug errors if problem and desired
    
    # Export generator dispatch
    try:
        export_generator_dispatch(instance, timepoints_set, generators_set, zones_set, results_directory)
    except Exception as err:
        msg = "ERROR exporting generator dispatch! Check export_generator_dispatch()."
        handle_exception(msg, debug_mode)
    
    # Export segmented generator dispatch
    try:
        export_generator_segment_dispatch(instance, timepoints_set, generators_set, zones_set, generatorsegment_set, results_directory)
    except Exception as err:
        msg = "ERROR exporting segmented generator dispatch! Check export_generator_segment_dispatch()."
        handle_exception(msg, debug_mode)
        
    #export zonal prices
    try:
        export_zonal_price(instance, timepoints_set, zones_set, ordc_segments_set, results_directory)
    except Exception as err:
        msg = "ERROR exporting zonal prices! Check export_zonal_price()."
        handle_exception(msg, debug_mode)
    
    #export tx lines
    try:
        export_lines(instance, timepoints_set, transmission_lines_set, results_directory)
    except Exception as err:
        msg = "ERROR exporting transmission lines! Check export_lines()."
        handle_exception(msg, debug_mode)
    
    try:
        export_generator_commits_reserves(instance, timepoints_set, generators_set, results_directory)
    except Exception as err:
        msg = "ERROR exporting transmission lines! Check export_lines()."
        handle_exception(msg, debug_mode)
    
    try:
        export_reserve_segment_commits(instance,timepoints_set, ordc_segments_set, results_directory)
    except Exception as err:
        msg = "ERROR exporting reserve segments! Check export_reserve_segment_commits()."
        handle_exception(msg, debug_mode)

    try:
        export_VREs(instance, results_directory)
    except Exception as err:
        msg = "ERROR exporting VRE results! Check export_VREs()."
        handle_exception(msg, debug_mode)
        
    #return call
    return None

# formatting functions
# return value rounded to two decimal places
def format_2f(input_data):
    """
    :param input_data: The data to format
    :return:
    """
    if input_data is None:
        formatted_data = None
    else:
        formatted_data = '{:0.2f}'.format(input_data)
        # if formatted_data is negative but rounds to zero, it will be printed as a negative zero
        # this gets rid of the negative zero
        if formatted_data == '0.00' or formatted_data == '-0.00':
            formatted_data = 0
    return formatted_data

#debugging call, if desired
def handle_exception(message, debug):
    """
    How to handle exceptions. First print a custom message and the traceback.
    If in debug mode, open the Python debugger (PDB), else exit. To execute multi-line statements in PDB, type this
    to launch an interactive Python session with all the local variables available.
    This function is used by export_results() to handle exceptions.
    :param message:
    :param debug:
    :return:
    """
    print (message)
    print(traceback.format_exc())
    if debug:
        print ("""
           Launching Python debugger (PDB)...
           To execute multi-line statements in PDB, type this:
                import code; code.interact(local=vars())
           (The command launches an interactive Python session with all the local variables available.)
           To exit the interactive session and continue script execution, press Ctrl+D.
           """)
        tp, value, tb = sys.exc_info()
        pdb.post_mortem(tb)
    else:
        print ("Debug option not chosen, so exiting.")
        sys.exit()

# #################### Data Exports #################### #
def load_solution(instance, results):
    """
    Load results. This function is called by export_results().
    :param instance:
    :param results:
    :return:
    """
    instance.solutions.load_from(results)
    
def export_generator_dispatch(instance, timepoints_set, generators_set, zones_set, results_directory):

    results_dispatch = []
    index_name = []
    for z in zones_set:   
        for g in generators_set:
            index_name.append(z+"-"+str(g))
            for t in timepoints_set:
                results_dispatch.append(format_2f(instance.dispatch[t,g,z].value))
    results_dispatch_np = np.reshape(results_dispatch, (int(len(results_dispatch)/len(timepoints_set)), int(len(timepoints_set))))
    df = pd.DataFrame(results_dispatch_np, index=pd.Index(index_name))
    df.to_csv(os.path.join(results_directory,"generator_dispatch.csv"))
    
def export_generator_segment_dispatch(instance, timepoints_set, generators_set, zones_set, generatorsegment_set, results_directory):
    
    results_dispatch = []
    index_name = []
    for z in zones_set:   
        for g in generators_set:
            for gs in generatorsegment_set:
                index_name.append(z+"-"+str(g)+"-" +str(gs))
                for t in timepoints_set:
                    results_dispatch.append(format_2f(instance.segmentdispatch[t,g,z,gs].value))
    results_dispatch_np = np.reshape(results_dispatch, (int(len(results_dispatch)/len(timepoints_set)), int(len(timepoints_set))))
    df = pd.DataFrame(results_dispatch_np, index=pd.Index(index_name))
    df.to_csv(os.path.join(results_directory,"generator_segment_dispatch.csv"))
    
    
def export_zonal_price(instance, timepoints_set, zones_set, ordc_segments_set, results_directory):
    
    results_prices = []
    results_synch_reserve_prices = []
    results_nonsynch_reserve_prices = []
    results_secondary_reserve_prices = []
    results_subzone_synch_reserve_prices = []
    results_penalty_factors = []
    index_name = []
    timepoints_list = []
    for z in zones_set:
        for t in timepoints_set:
            index_name.append(z)
            results_prices.append(format_2f(instance.dual[instance.LoadConstraint[t,z]]))
            results_synch_reserve_prices.append(format_2f(-instance.dual[instance.TotalSynchReserveConstraint[t]]))
            results_nonsynch_reserve_prices.append(format_2f(-instance.dual[instance.TotalNonSynchReserveConstraint[t]]))
            results_secondary_reserve_prices.append(format_2f(-instance.dual[instance.TotalSecondaryReserveConstraint[t]]))
            results_subzone_synch_reserve_prices.append(format_2f(-instance.dual[instance.TotalSubZonalSynchReserveConstraint[t]]))
            #reserves multiplied by -1 to report as positive lost opportunity cost of marginal reserve resource
            timepoints_list.append(t)
    for z in zones_set:
        recorded_timepoints = []
        for t in timepoints_set:
            for s in ordc_segments_set:
                if instance.SynchMW[t,s].value-instance.segmentreserves[t,s].value > 0.1:
                    if t not in recorded_timepoints:
                        results_penalty_factors.append(instance.price[t,s].value)
                        recorded_timepoints.append(t)
                elif s == max(ordc_segments_set):
                    results_penalty_factors.append(0)
                    recorded_timepoints.append(t)           
    
    col_names = ['hour','LMP','PrimarySynchReservePrice','PrimaryNonSynchReservePrice',
                 'SecondaryReservePrice','SubZonePrimarySynchReservePrice','PrimarySynchPenaltyFactor']
    df = pd.DataFrame(data=np.column_stack((np.asarray(timepoints_list), np.asarray(results_prices), 
                       np.asarray(results_synch_reserve_prices),
                       np.asarray(results_nonsynch_reserve_prices),
                       np.asarray(results_secondary_reserve_prices),
                       np.asarray(results_subzone_synch_reserve_prices),
                       np.asarray(results_penalty_factors[0:len(timepoints_list)]))),
                       columns=col_names,
                       index=pd.Index(index_name))
    
    df.to_csv(os.path.join(results_directory,"zonal_prices.csv"))
    
def export_lines(instance, timepoints_set, transmission_lines_set, results_directory):
    
    transmission_duals = []
    results_transmission_line_flow = []
    index_name = []
    for line in transmission_lines_set:
        for t in timepoints_set:
            index_name.append(line+"-"+str(t))
            transmission_duals.append(format_2f(instance.dual[instance.TxFromConstraint[t,line]] +\
                                      instance.dual[instance.TxToConstraint[t,line]]))
            results_transmission_line_flow.append(format_2f(instance.transmit_power_MW[t,line].value))
    col_names = ['congestion price ($/MW)','flow (MW)']
    df = pd.DataFrame(data=np.column_stack((np.asarray(transmission_duals),np.asarray(results_transmission_line_flow))),
                      columns=col_names,index=pd.Index(index_name))
    df.to_csv(os.path.join(results_directory,"tx_flows.csv"))
    

def export_generator_commits_reserves(instance, timepoints_set, generators_set, results_directory):
    
    results_gens = []
    results_time = []
    results_commitment = []
    results_starts = []
    results_shuts = []
    results_hourson = []
    results_hoursoff = []
    results_primarysynchreserves = []
    results_primarynonsynchreserves = []
    results_allreserves = []
    results_secondaryreserves = []
    index_name = []
    for g in generators_set:
        for t in timepoints_set:
            index_name.append(str(g)+","+str(t))
            results_gens.append(g)
            results_time.append(t)
            results_commitment.append(instance.commitment[t,g].value)
            results_starts.append(instance.startup[t,g].value)
            results_shuts.append(instance.shutdown[t,g].value)
            
            if t==1 and instance.commitinit[g]==instance.commitment[t,g].value:
                results_hourson.append(instance.commitinit[g] * instance.upinit[g] + instance.commitment[t,g].value)
                results_hoursoff.append((1-instance.commitinit[g]) * instance.downinit[g] + (1-instance.commitment[t,g].value))
            elif instance.startup[t,g].value==1 or instance.shutdown[t,g].value==1:
                results_hourson.append(instance.commitment[t,g].value)
                results_hoursoff.append((1-instance.commitment[t,g].value))
            else:
                results_hourson.append(results_hourson[-1]+instance.commitment[t,g].value)
                results_hoursoff.append(results_hoursoff[-1]+(1-instance.commitment[t,g].value))
            
            results_primarysynchreserves.append(format_2f(instance.synchreserves[t,g].value))
            results_primarynonsynchreserves.append(format_2f(instance.nonsynchreserves[t,g].value))
            results_allreserves.append(format_2f(instance.synchreserves[t,g].value + instance.nonsynchreserves[t,g].value))
            results_secondaryreserves.append(format_2f(instance.secondaryreserves[t,g].value))
    
    col_names = ['Gen_Index','timepoint','Committed','Started','Shut','TimeOn','TimeOff',
                 'Total Held as Primary Synch Reserves (MW)', 'Total Held as Primary NonSynch Reserves (MW)',
                 'Total Held as Primary Reserves (MW)', 'Total Held as Secondary Reserves (MW)']
    df = pd.DataFrame(data=np.column_stack((np.asarray(results_gens), np.asarray(results_time), np.asarray(results_commitment),
                                            np.asarray(results_starts), np.asarray(results_shuts),np.asarray(results_hourson),
                                            np.asarray(results_hoursoff),
                                            np.asarray(results_primarysynchreserves), np.asarray(results_primarynonsynchreserves),
                                            np.asarray(results_allreserves), np.asarray(results_secondaryreserves))),
                      columns=col_names,index=pd.Index(index_name))
    df.to_csv(os.path.join(results_directory,"generator_commits_reserves.csv"), index=False)
    
def export_reserve_segment_commits(instance, timepoints_set, ordc_segments_set, results_directory):
    
    results_synch_segments = []
    results_nonsynch_segments = []
    results_secondary_segments = []
    index_name = []
    for s in ordc_segments_set:
        for t in timepoints_set:
            index_name.append(str(s)+","+str(t))
            results_synch_segments.append(format_2f(instance.segmentreserves[t,s].value))
            results_nonsynch_segments.append(format_2f(instance.nonsynchsegmentreserves[t,s].value))
            results_secondary_segments.append(format_2f(instance.secondarysegmentreserves[t,s].value))
    col_names = ['MW on primary synch reserve segment', 'MW on primary nonsynch reserve segment',
                 'MW on secondary reserve segment']
    df = pd.DataFrame(data=np.column_stack((np.asarray(results_synch_segments), np.asarray(results_nonsynch_segments),
                                            np.asarray(results_secondary_segments))),
                      columns=col_names,index=pd.Index(index_name))
    df.to_csv(os.path.join(results_directory,"reserve_segment_commit.csv"))
    
def export_VREs(instance, results_directory):
    
    results_wind = []
    results_solar = []
    results_curtailment = []
    tmps = []
    zones = []
    
    for t in instance.TIMEPOINTS:
        for z in instance.ZONES:
            tmps.append(t)
            zones.append(z)
            results_wind.append(instance.windgen[t,z].value)
            results_solar.append(instance.solargen[t,z].value)
            results_curtailment.append(instance.curtailment[t,z].value)
            
    VRE = pd.DataFrame({"timepoint": tmps, "zone": zones, "wind":results_wind, "solar": results_solar, "curtailment":results_curtailment})
    VRE.to_csv(os.path.join(results_directory,"renewable_generation.csv"), index=False)
