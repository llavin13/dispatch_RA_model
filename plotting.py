#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 08:37:33 2019

@author: bsergi
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

#other scripts
from case_inputs import case_folder


def diagnostic_plots(scenario_results, dir_str):
    #PLOTS ONLY
    #plot some basic results with matplotlib
    scenario_results_np = np.reshape(scenario_results[0], (int(scenario_results[1]), int(len(scenario_results[0])/scenario_results[1])))
    start_results_np = np.reshape(scenario_results[5], (int(scenario_results[1]), int(len(scenario_results[5])/scenario_results[1])))
    shut_results_np = np.reshape(scenario_results[6], (int(scenario_results[1]), int(len(scenario_results[6])/scenario_results[1])))
    synch_results_np = np.reshape(scenario_results[9], (int(scenario_results[1]), int(len(scenario_results[9])/scenario_results[1])))
    nonsynch_results_np = np.reshape(scenario_results[13], (int(scenario_results[1]), int(len(scenario_results[13])/scenario_results[1])))
    secondary_results_np = np.reshape(scenario_results[14], (int(scenario_results[1]), int(len(scenario_results[14])/scenario_results[1])))
    subzone_synch_results_np = np.reshape(scenario_results[15],(int(scenario_results[1]), int(len(scenario_results[15])/scenario_results[1])))
    
    wind_results_np = np.reshape(scenario_results[2], (int(scenario_results[1]), int(len(scenario_results[2])/scenario_results[1])))
    solar_results_np = np.reshape(scenario_results[3], (int(scenario_results[1]), int(len(scenario_results[3])/scenario_results[1])))
    curtailment_results_np = np.reshape(scenario_results[4], (int(scenario_results[1]), int(len(scenario_results[4])/scenario_results[1])))
    lmp_duals_np = np.reshape(scenario_results[7], (int(scenario_results[1]), int(len(scenario_results[7])/scenario_results[1])))
    
    line_duals_np = np.reshape(scenario_results[11], (int(scenario_results[1]), int(len(scenario_results[11])/scenario_results[1])))
    transmission_flow_np = np.reshape(scenario_results[12], (int(scenario_results[1]), int(len(scenario_results[12])/scenario_results[1])))
    
    #read in the gen and zone types so aggregation can be done for plots
    gens = pd.read_csv(join(dir_str.INPUTS_DIRECTORY, 'PJM_generators_full.csv'))
    subzone_gens = gens.drop(gens[gens.In_Sub_Zone == 0].index)
    zones = pd.read_csv(join(dir_str.INPUTS_DIRECTORY, 'zones.csv'))
    line_names = pd.read_csv(join(dir_str.INPUTS_DIRECTORY, 'transmission_lines.csv'))
    #full_tx_lines = pd.read_csv(join(dir_str.INPUTS_DIRECTORY, 'transmission_lines_hourly.csv'))
    
    gens_list = []
    zones_list = []
    y = []
    start = []
    shut = []
    synchreserves = []
    nonsynchreserves = []
    secondaryreserves = []
    subzonereserves = []
    wind_power = []
    solar_power = []
    curtail_power = []
            
    
    
    for g in gens['Category'].unique():
        #print(g) #placeholder for now
        gen_type = (gens['Category']==g)
        subzone_gen_type = (subzone_gens['Category']==g)
        #subzone_gen_type = gens
        
        start.append(np.dot(start_results_np,np.array(gen_type)))
        shut.append(np.dot(shut_results_np,np.array(gen_type)))
        synchreserves.append(np.dot(synch_results_np,np.array(gen_type)))
        nonsynchreserves.append(np.dot(nonsynch_results_np,np.array(gen_type)))  
        secondaryreserves.append(np.dot(secondary_results_np,np.array(gen_type)))
        subzonereserves.append(np.dot(subzone_synch_results_np,np.array(subzone_gen_type)))
    
    for z in range(len(zones['zone'])):
        wind_power.append(wind_results_np[:,z])
        solar_power.append(solar_results_np[:,z])
        curtail_power.append(curtailment_results_np[:,z])
        for g in gens['Category'].unique():
            gen_type = (gens['Category']==g)
            y.append(np.dot(scenario_results_np[:,z*len(gen_type):(z+1)*len(gen_type)],np.array(gen_type)))
    
    subzonereserves.append(np.array(scenario_results[16])) #adds in the transmission contribution to subzone reserves
    
    
    # Your x and y axis
    x=range(1,int(scenario_results[1])+1)
    #y is made above
    
    # Basic stacked area chart by zone
    for z in range(len(zones['zone'])):
        
        adder = len(gens['Category'].unique())*z
        
        plt.plot([],[],color='b', label='Hydro', linewidth=5)
        plt.plot([],[],color='m', label='Nuclear', linewidth=5)
        plt.plot([],[],color='k', label='Large Coal', linewidth=5)
        plt.plot([],[],color='slategray', label='Small Coal', linewidth=5)
        plt.plot([],[],color='orange', label='Gas CC', linewidth=5)
        plt.plot([],[],color='sienna', label='Gas CT', linewidth=5)
        plt.plot([],[],color='g', label='Oil', linewidth=5)
        plt.plot([],[],color='silver', label='Demand Response', linewidth=5)
        plt.plot([],[],color='cyan', label='Wind', linewidth=5)
        plt.plot([],[],color='yellow', label='Solar', linewidth=5)
        plt.plot([],[],color='red', label='Curtailment', linewidth=5)
        
        if case_folder == "TOYCASE":
            plt.stackplot(x,y[adder+5],y[adder+6],y[adder+2],y[adder+4],y[adder+0],y[adder+1],y[adder+3],y[adder+7],
                          wind_power[z],solar_power[z],curtail_power[z],
                          colors=['b','m','k','slategray','orange','sienna','g','silver','cyan','yellow','red'])
        else:
            plt.stackplot(x,y[adder+4],y[adder+6],y[adder+2],y[adder+5],y[adder+0],y[adder+1],y[adder+3],y[adder+7],
                          wind_power[z],solar_power[z],curtail_power[z],
                          colors=['b','m','k','slategray','orange','sienna','g','silver','cyan','yellow','red'])
        
        plt.title('Zone ' + zones['zone'][z] + ' Generator Dispatch')
        plt.ylabel('Load (MW)')
        plt.xlabel('Hour')
        plt.legend(loc=4)
        plt.show()
    
    #do also for starts
    plt.plot([],[],color='b', label='Hydro', linewidth=5)
    plt.plot([],[],color='m', label='Nuclear', linewidth=5)
    plt.plot([],[],color='k', label='Large Coal', linewidth=5)
    plt.plot([],[],color='slategray', label='Small Coal', linewidth=5)
    plt.plot([],[],color='orange', label='Gas CC', linewidth=5)
    plt.plot([],[],color='sienna', label='Gas CT', linewidth=5)
    plt.plot([],[],color='g', label='Oil', linewidth=5)
    
    if case_folder == "TOYCASE":
        plt.stackplot(x,start[5],start[6],start[2],start[4],start[0],start[1],start[3],
                  colors=['b','m','k','slategray','orange','sienna','g'])
    else:
        plt.stackplot(x,start[4],start[6],start[2],start[5],start[0],start[1],start[3],
                  colors=['b','m','k','slategray','orange','sienna','g'])
    plt.ylabel('StartUps (# Plants)')
    plt.xlabel('Hour')
    plt.legend()
    plt.show()
    
    #and shuts
    plt.plot([],[],color='b', label='Hydro', linewidth=5)
    plt.plot([],[],color='m', label='Nuclear', linewidth=5)
    plt.plot([],[],color='k', label='Large Coal', linewidth=5)
    plt.plot([],[],color='slategray', label='Small Coal', linewidth=5)
    plt.plot([],[],color='orange', label='Gas CC', linewidth=5)
    plt.plot([],[],color='sienna', label='Gas CT', linewidth=5)
    plt.plot([],[],color='g', label='Oil', linewidth=5)
    
    if case_folder == "TOYCASE":
        plt.stackplot(x,shut[5],shut[6],shut[2],shut[4],shut[0],shut[1],shut[3],
                  colors=['b','m','k','slategray','orange','sienna','g'])
    else:
        plt.stackplot(x,shut[4],shut[6],shut[2],shut[5],shut[0],shut[1],shut[3],
                  colors=['b','m','k','slategray','orange','sienna','g'])
    plt.ylabel('Shutdowns (# Plants)')
    plt.xlabel('Hour')
    plt.legend()
    plt.show()
    
    #and for the held synch reserves by generator type
    plt.plot([],[],color='b', label='Hydro', linewidth=5)
    plt.plot([],[],color='m', label='Nuclear', linewidth=5)
    plt.plot([],[],color='k', label='Large Coal', linewidth=5)
    plt.plot([],[],color='slategray', label='Small Coal', linewidth=5)
    plt.plot([],[],color='orange', label='Gas CC', linewidth=5)
    plt.plot([],[],color='sienna', label='Gas CT', linewidth=5)
    plt.plot([],[],color='g', label='Oil', linewidth=5)
    
    if case_folder == "TOYCASE":
        plt.stackplot(x,synchreserves[5],synchreserves[6],synchreserves[2],synchreserves[4],synchreserves[0],synchreserves[1],synchreserves[3],
                  colors=['b','m','k','slategray','orange','sienna','g'])
    else:
        plt.stackplot(x,synchreserves[4],synchreserves[6],synchreserves[2],synchreserves[5],synchreserves[0],synchreserves[1],synchreserves[3],
                  colors=['b','m','k','slategray','orange','sienna','g'])
    plt.ylabel('Held Synch Reserves (MW)')
    plt.xlabel('Hour')
    plt.legend()
    plt.show()
    
    #and for the held nonsynch reserves by generator type
    plt.plot([],[],color='b', label='Hydro', linewidth=5)
    plt.plot([],[],color='m', label='Nuclear', linewidth=5)
    plt.plot([],[],color='k', label='Large Coal', linewidth=5)
    plt.plot([],[],color='slategray', label='Small Coal', linewidth=5)
    plt.plot([],[],color='orange', label='Gas CC', linewidth=5)
    plt.plot([],[],color='sienna', label='Gas CT', linewidth=5)
    plt.plot([],[],color='g', label='Oil', linewidth=5)
    
    plt.stackplot(x,nonsynchreserves[5],nonsynchreserves[6],nonsynchreserves[2],nonsynchreserves[4],nonsynchreserves[0],nonsynchreserves[1],nonsynchreserves[3],
                  colors=['b','m','k','slategray','orange','sienna','g'])
    plt.ylabel('Held NonSynch Reserves (MW)')
    plt.xlabel('Hour')
    plt.legend()
    plt.show()
    
    #and for the held secondary reserves by generator type
    plt.plot([],[],color='b', label='Hydro', linewidth=5)
    plt.plot([],[],color='m', label='Nuclear', linewidth=5)
    plt.plot([],[],color='k', label='Large Coal', linewidth=5)
    plt.plot([],[],color='slategray', label='Small Coal', linewidth=5)
    plt.plot([],[],color='orange', label='Gas CC', linewidth=5)
    plt.plot([],[],color='sienna', label='Gas CT', linewidth=5)
    plt.plot([],[],color='g', label='Oil', linewidth=5)
    
    plt.stackplot(x,secondaryreserves[5],secondaryreserves[6],secondaryreserves[2],secondaryreserves[4],secondaryreserves[0],secondaryreserves[1],secondaryreserves[3],
                  colors=['b','m','k','slategray','orange','sienna','g'])
    plt.ylabel('Held Secondary Reserves (MW)')
    plt.xlabel('Hour')
    plt.legend()
    plt.show()
    
    #and do held sub-zonal synch reserves (this will be MAD in PJM)
    plt.plot([],[],color='b', label='Hydro', linewidth=5)
    plt.plot([],[],color='m', label='Nuclear', linewidth=5)
    plt.plot([],[],color='k', label='Large Coal', linewidth=5)
    plt.plot([],[],color='slategray', label='Small Coal', linewidth=5)
    plt.plot([],[],color='orange', label='Gas CC', linewidth=5)
    plt.plot([],[],color='sienna', label='Gas CT', linewidth=5)
    plt.plot([],[],color='g', label='Oil', linewidth=5)
    plt.plot([],[],color='silver', label='Deliverable Transmission', linewidth=5)
    
    if case_folder == "TOYCASE":
        plt.stackplot(x,subzonereserves[5],subzonereserves[6],subzonereserves[2],subzonereserves[4],subzonereserves[0],subzonereserves[1],subzonereserves[3],subzonereserves[8],
                  colors=['b','m','k','slategray','orange','sienna','g','silver'])
    else:
        plt.stackplot(x,subzonereserves[4],subzonereserves[6],subzonereserves[2],subzonereserves[5],subzonereserves[0],subzonereserves[1],subzonereserves[3],subzonereserves[8],
                  colors=['b','m','k','slategray','orange','sienna','g','silver'])
    plt.ylabel('Held SubZone Synch Reserves (MW)')
    plt.xlabel('Hour')
    plt.legend()
    plt.show()
    
    #Tx flow plot, as lines
    tx_palette = ['b','m','k','orange','sienna','g','silver','cyan','yellow','red']
    tx_label = []
    for line in range(len(line_names['transmission_line'])):
        if line_names['old'][line] != 0:
            plt.plot(x, transmission_flow_np[:,line], color=tx_palette[line])
            tx_label.append(line_names['transmission_line'][line])
    plt.title('Transmission Flows on Existing Lines')
    plt.ylabel('Flow on Line (MW)')
    plt.xlabel('Hour')
    plt.legend(tx_label, loc='upper left')
    plt.show()
    
    #and finally, plot the energy LMP dual
    lmp_palette = ['r','blue','black','green','sienna']
    legend_label = []
    for z in range(len(zones['zone'])):
        plt.plot(x, lmp_duals_np[:,z], color=lmp_palette[z])
        legend_label.append('Zone ' + zones['zone'][z])
    plt.ylabel('Energy Price ($/MWh)')
    plt.xlabel('Hour')
    plt.legend(legend_label, loc='upper left')
    plt.show()
    
    #transmission ("congestion") dual
    tx_palette = ['b','m','k','orange','sienna','g','silver','cyan','yellow','red']
    tx_label = []
    for line in range(len(line_names['transmission_line'])):
        if line_names['old'][line] != 0:
            plt.plot(x, line_duals_np[:,line], color=tx_palette[line])
            tx_label.append(line_names['transmission_line'][line])
    plt.ylabel('Congestion Price of Line ($/MW)')
    plt.xlabel('Hour')
    plt.legend(tx_label, loc='upper left')
    plt.show()
    
    #synch reserve duals
    plt.plot(x, -np.asarray(scenario_results[8]), color='black')
    plt.title('Negative Synchronized Primary Reserve Duals')
    plt.ylabel('Synchronized Primary Reserve Price ($/MW)')
    plt.xlabel('Hour')
    plt.show()