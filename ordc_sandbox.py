# -*- coding: utf-8 -*-
"""
Created on Tue May 21 15:43:10 2019

@author: llavi
"""

import pandas as pd
from os.path import join
import os
import numpy as _np
import scipy.signal
import time
import itertools
from datetime import datetime, timedelta


from case_inputs import *

start_time = time.time() #record start time so can know how long this version of the script takes

def no_ordc(raw_input_dir, case_dir,n_segments):
    '''
    this gets called and creates a ORDC timepoint file with *NO* (=0) price for all segements
    so effectively no value for reserves. 
    
    as of 8/5/2019 this is updated to create all 3 types of reserve products with no prices
    '''
    print('running creation of ORDC, but will have 0 prices')
    
    load_df = pd.read_csv(os.path.join(case_dir,"timepoints_zonal.csv"))
    
    timepoint_list = list(load_df.timepoint.unique())
    segment_list = list(range(1,n_segments+1))
    full_timepoint_list = []
    full_segment_list = []
    
    for t in timepoint_list:
        for s in segment_list:
            full_timepoint_list.append(t)
            full_segment_list.append(s)
    
    segment_dummy = 100
    full_PrimarySynchMW_list = [segment_dummy]*len(full_segment_list)
    full_PrimaryNonSynchMW_list = [1.5*segment_dummy]*len(full_segment_list)
    full_SecondaryMW_list = [3.*segment_dummy]*len(full_segment_list)
    full_price_list = [0]*len(full_segment_list)
    
    
    segment_df = pd.DataFrame({'timepoint':full_timepoint_list,
              'segments':full_segment_list,
              'SynchMW':full_PrimarySynchMW_list,
              'NonSynchMW':full_PrimaryNonSynchMW_list,
              'SecondaryMW':full_SecondaryMW_list,
              'Price':full_price_list})
        
    return segment_df  

def PJM_reserves(raw_input_dir, case_dir,
                 n_segments, lfe, FOR_fe, reserve_short_penalty):
    '''
    This is meant to pseudo-imitate previous PJM practice (approx 2012-2019 practice, with second step implemented 2014)
    This means for DA cases, only secondary reserves are required
    And though techincally they weren't penalized for lack of holding reserves in DA (just RT),
    there was a $850/MW penalty in RT, so I'll implement that for the DA run too
    note this could easily be updated to just create any "current" type of heuristic PJM reserve requirements
    '''
    
    #load the load data, so it can be scaled by forecast errors to get reserve requirements
    load_df = pd.read_csv(os.path.join(case_dir,"timepoints_zonal.csv"))
    
    #create hourly load
    hourly_loads = load_df.groupby("timepoint")["gross_load"].sum() #as pd series
    hourly_loads = pd.DataFrame({'timepoint':hourly_loads.index, 'gross_load':hourly_loads.values})
    
    da_forecast_error = lfe+FOR_fe #as decimal
    
    #multiply forecast error by hourly load to get reserve requirement, as appears to have been practice
    #really this should take into account the probability of being short, but it doesn't appear historically that's what PJM did
    hourly_loads['da_reserve_requirement'] = hourly_loads.gross_load*da_forecast_error
    reserve_requirement_np = hourly_loads["da_reserve_requirement"].values #will give the np array
    
    #now create the lists for output
    timepoint_list = list(load_df.timepoint.unique())
    segment_list = list(range(1,n_segments+1))
    
    full_timepoint_list = []
    full_segment_list = []
    full_PrimarySynchMW_list = []
    full_PrimaryNonSynchMW_list = []
    full_SecondaryMW_list = []
    full_price_list = []
    
    for t in timepoint_list:
        for s in segment_list:
            full_timepoint_list.append(t)
            full_segment_list.append(s)
            
            if  s==1:
                #first step of ORDC as heuristically defined by PJM
                full_PrimarySynchMW_list.append(1400) #1400 seems best approximation of n-1
                full_PrimaryNonSynchMW_list.append(2100)
                full_SecondaryMW_list.append(reserve_requirement_np[t-1]) #has to be offset by 1 because Python indexes from 0
                full_price_list.append(reserve_short_penalty)
            elif s==2:
                #second step of ORDC as heuristically defined by PJM
                full_PrimarySynchMW_list.append(190)
                full_PrimaryNonSynchMW_list.append(190)
                full_SecondaryMW_list.append(190) #this didn't really exist but doubt it affects results
                full_price_list.append(reserve_short_penalty*(3./8.5))
            else:
                full_SecondaryMW_list.append(0)
                full_price_list.append(0)
                full_PrimarySynchMW_list.append(0) #0 for now but note ~1.5 GW must be held as synch in RT
                full_PrimaryNonSynchMW_list.append(0)
                
    segment_df = pd.DataFrame({'timepoint':full_timepoint_list,
              'segments':full_segment_list,
              'SynchMW':full_PrimarySynchMW_list,
              'NonSynchMW':full_PrimaryNonSynchMW_list,
              'SecondaryMW':full_SecondaryMW_list,
              'Price':full_price_list})
        
    return segment_df  


def load_and_run_ordc(raw_input_dir, case_dir,
                      month, hydro_cf, VOLL, lowcutLOLP, n_segments, dynamic_ORDC,
                      datestr, primary_reserve_scalar, secondary_reserve_scalar,
                      MRR_method, MRRs, lfe, FOR_fe): #this should be run from the main script, if desired, with appropriate inputs
    print('running creation of new ORDC for ' + datestr)
    print('case folder is for creating ORDC is' + case_dir)
    #things to eventually pass
    #(1) Month of case
    #(2) hydro cf
    #(3) VOLL
    #(4) Lowcut LOLP
    #(5) number of segments in ORDC
    
    #general inputs from raw
    planned_out_df = pd.read_csv(os.path.join(raw_input_dir,"scheduled.outage.rate.by.gen.type.FULL.PERIOD.032519.csv"))
    forced_out_df = pd.read_csv(os.path.join(raw_input_dir,"Forced.outage.rates.by.temperature.and.unit.type.102918.csv"))
    fixed_forced_out_df = pd.read_csv(os.path.join(raw_input_dir,"fixed_FORs.csv"))
    
    #additional initialization inputs
    init_avail_df = pd.read_csv(os.path.join(raw_input_dir,"fraction.unavailable.SFU1U2U3D1D2D3.all.052319.csv"),index_col=0)
    prob_fail_df = pd.read_csv(os.path.join(raw_input_dir,"failure.probabilities.1845.gens.1995.2018.xRS.and.pooled.fits.050919.csv"),index_col=0)
    prob_recover_df = pd.read_csv(os.path.join(raw_input_dir,"recovery.probabilities.1845.gens.1995.2018.xRS.and.pooled.fits.050919.csv"),index_col=0)
    generator_level_outage_magnitude_df = pd.read_csv(os.path.join(raw_input_dir,"PJM.average.normalized.unscheduled.outage.all.units.1995.2018.xRS.csv"),index_col=0)
    unit_type_outage_magnitude_df = pd.read_csv(os.path.join(raw_input_dir,"PJM.average.normalized.unscheduled.outage.magnitude.by.gen.type.052319.csv"),index_col=0)
    
    
    #user-defined inputs
    scenario_inputs_directory = os.path.join(raw_input_dir, "case_creation_input")
    zonal_inputs = pd.read_csv(os.path.join(scenario_inputs_directory,"LDA_to_zone.csv"),index_col=0)
    
    #based on the specific case
    load_df = pd.read_csv(os.path.join(case_dir,"timepoints_zonal.csv"))
    wind_solar_df = pd.read_csv(os.path.join(case_dir,"zones.csv"))
    gen_df = pd.read_csv(os.path.join(case_dir,"PJM_generators_full.csv"))
    temp_df = pd.read_csv(os.path.join(case_dir,"timepoints_index_allweather.csv"),index_col=0)
    scheduled_outage_df = pd.read_csv(os.path.join(case_dir,"PJM_generators_scheduled_outage.csv"))
    
    #and get the da forecast errors
    
    #create hourly load
    hourly_loads = load_df.groupby("timepoint")["gross_load"].sum() #as pd series
    hourly_loads = pd.DataFrame({'timepoint':hourly_loads.index, 'gross_load':hourly_loads.values})
    
    da_forecast_error = lfe+FOR_fe #as decimal
    
    #multiply forecast error by hourly load to get reserve requirement, as appears to have been practice
    #really this should take into account the probability of being short, but it doesn't appear historically that's what PJM did
    hourly_loads['da_reserve_requirement'] = hourly_loads.gross_load*da_forecast_error
    reserve_requirement_np = hourly_loads["da_reserve_requirement"].values #will give the np array
    
    if MRR_method:
        print("MRR method is " + str(MRR_method) + " so will set MRR per instructions and look only at hourly marginal failure probabilities (most similar to PJM)")
    else:
        print("MRR method is " + str(MRR_method) + " so will evolve generator availablility through day without mins (old method)")
    
    return create_ordc(gen_df, planned_out_df, load_df, wind_solar_df, temp_df, forced_out_df,
                       month, hydro_cf, VOLL, lowcutLOLP, n_segments, fixed_forced_out_df, 
                       dynamic_ORDC, scheduled_outage_df, zonal_inputs,
                       init_avail_df, prob_fail_df, prob_recover_df, generator_level_outage_magnitude_df,unit_type_outage_magnitude_df,
                       datestr, primary_reserve_scalar, secondary_reserve_scalar, MRR_method, MRRs, reserve_requirement_np)


#this is a function for getting the ORDC formulation to work
def create_ordc(gen_df, planned_out_df, load_df, wind_solar_df, temp_df, forced_out_df,
                month, hydro_cf, VOLL, lowcutLOLP, n_segments, fixed_forced_out_df, 
                dynamic_ORDC, scheduled_outage_df, zonal_inputs,
                init_avail_df, prob_fail_df, prob_recover_df, generator_level_outage_magnitude_df,unit_type_outage_magnitude_df,
                datestr, primary_reserve_scalar, secondary_reserve_scalar,
                MRR_method, MRRs, reserve_requirement_np):
    
    #check loading of new frames
    #print(init_avail_df)
    #print(prob_recover_df)
    
    #create generator stack on unit marginal cost
    gen_type_capacity = gen_df
    
    #dec generator stack by monthly planned outages
    #month="Jan"
    planned_out_df.rename(columns ={'Unnamed: 0': "Category"}, inplace =True)
    gen_type_capacity = pd.merge(gen_type_capacity, planned_out_df, on='Category')
    gen_type_capacity["Month_Capacity"]=gen_type_capacity["Capacity"]-gen_type_capacity[month]*gen_type_capacity["Capacity"]
    gen_type_capacity = gen_type_capacity.set_index("Gen_Index")
    gen_type_capacity = gen_type_capacity.sort_values("Fuel_Cost") #sorts on marginal dispatch cost
    
    #derate hydro by cf
    #hydro_cf = 0.5
    derate_capacity = []
    for g in list(gen_type_capacity.index):
        if gen_type_capacity.loc[g]["Category"]=="HD":
            derate_capacity.append(gen_type_capacity.loc[g]["Month_Capacity"]*hydro_cf) 
        else:
            derate_capacity.append(gen_type_capacity.loc[g]["Month_Capacity"])
    gen_type_capacity['Month_Capacity_Derated'] = derate_capacity
    
    #create hourly load
    hourly_loads = load_df.groupby("timepoint")["gross_load"].sum() #as pd series
    hourly_loads = pd.DataFrame({'timepoint':hourly_loads.index, 'gross_load':hourly_loads.values})
    #dec load by wind and solar to get net load to be served by dispatchable gens
    hourly_cf = load_df.groupby("timepoint")["wind_cf","solar_cf"].mean()
    hourly_all = pd.merge(hourly_loads, hourly_cf, on="timepoint")
    #do wind
    wind_list = [wind_solar_df.wind_cap.sum()]*len(hourly_all.index)
    hourly_all["Wind_Capacity"] = wind_list
    hourly_all["Wind_Output"] = hourly_all["Wind_Capacity"]*hourly_all["wind_cf"] 
    #do solar
    solar_list = [wind_solar_df.solar_cap.sum()]*len(hourly_all.index)
    hourly_all["Solar_Capacity"] = solar_list
    hourly_all["Solar_Output"] = hourly_all["Solar_Capacity"]*hourly_all["solar_cf"] 
    #create net load column
    hourly_all["net_load"] = hourly_all["gross_load"] - hourly_all["Solar_Output"] - hourly_all["Wind_Output"]
    hourly_all=hourly_all.set_index("timepoint")
    
    #create hourly gen stacks
    time_list = []
    gen_list = []
    gentype_list = []
    zone_list = []
    temperature_list = []
    dispatch_list = []
    w_sch_out_dispatch_list = []
    avail_cap = []
    util_unit_list = []
    
    for t in hourly_all.index.tolist():
        load = hourly_all.loc[t]["net_load"]
        merit_order_list = gen_type_capacity.index.tolist()
        for genindex in merit_order_list:
            ###
            #grab the utilunit to match against the other df
            unit_util_unit = gen_type_capacity.loc[genindex]["UTILUNIT"]
            #run a check on whether this gen is available for the hour in question
            try:
                init_state = init_avail_df.loc[convert_datestr_to_unavail_index(datestr)][unit_util_unit]
            except KeyError:
                init_state = 0 #assume a generator with no info about its initial state is fully available
            w_sch_out_dispatch_list.append(init_state)
            if init_state != 0:
                dispatch = 0
            #####
            elif load - gen_type_capacity.loc[genindex]["Month_Capacity_Derated"] < 0:
                dispatch = load
                load = 0
            else:
                dispatch = gen_type_capacity.loc[genindex]["Month_Capacity_Derated"]
                load = load-dispatch
            time_list.append(t)
            gen_list.append(genindex)
            gentype_list.append(gen_type_capacity.loc[genindex]["Category"])
            matched_temperature_ID = zonal_inputs.loc[gen_type_capacity.loc[genindex]["Zone"]]["assigned_WBAN"]
            matched_temperature = temp_df.loc[_np.int64(t)][str(matched_temperature_ID)]
            temperature_list.append(matched_temperature) #need real temps here soon
            zone_list.append(gen_type_capacity.loc[genindex]["Zone"])
            dispatch_list.append(dispatch)
            avail_cap.append(gen_type_capacity.loc[genindex]["Month_Capacity_Derated"])
            util_unit_list.append(gen_type_capacity.loc[genindex]["UTILUNIT"])
    
    hourly_stack_wtemp = pd.DataFrame({'timepoint':time_list, 
                  'Index':gen_list,
                  'Category':gentype_list,
                  'Zone':zone_list,
                  'Temperature': temperature_list,
                 'Dispatch':dispatch_list,
                 'with Sch Out Dispatch': w_sch_out_dispatch_list,
                 'avail capacity': avail_cap,
                 'UTILUNIT':util_unit_list})
    
    #append hourly prevailing pjm temperature to df
    #hourly_stack_wtemp = pd.merge(hourly_stack, temp_df, on="timepoint")
    
    #round temperatures to nearest 5 degrees, append C.
    hourly_stack_wtemp['rounded_temp']=hourly_stack_wtemp.Temperature.apply(lambda x: custom_round(x, 5, True))
    #round temperatures to nearest 1 degree, append C.
    hourly_stack_wtemp['1_rounded_temp']=hourly_stack_wtemp.Temperature.apply(lambda x: custom_round(x, 1, False))
    
    #hourly_stack_wtemp.to_csv('htemp.csv')
    
    #append outage probability to the df based on temperature and unit type
    #only do based on temperature if case chooses to do so...
    forced_out_df.rename(columns ={'Unnamed: 0': "Category"}, inplace =True)
    fixed_forced_out_df.rename(columns ={'Unit_Type': "Category"}, inplace =True)
    try:
        forced_out_df = forced_out_df.set_index("Category")
    except KeyError:
        pass
    try:
        fixed_forced_out_df = fixed_forced_out_df.set_index("Category")
    except KeyError:
        pass
    
    matched_forced_outage = []
    new_forced_outage = []
    n_gens = len(hourly_stack_wtemp.UTILUNIT.unique())
    
    
    prob_recover_df.columns = prob_recover_df.columns.astype(str)
    prob_fail_df.columns = prob_fail_df.columns.astype(str)
    
    for index in hourly_stack_wtemp.index.tolist():
        #new
        utilunit = hourly_stack_wtemp.loc[index]['UTILUNIT']
        degree_temp = hourly_stack_wtemp.loc[index]['1_rounded_temp']
        degree_temp = str(degree_temp)
        
        recover_prob = prob_recover_df.loc[utilunit][degree_temp]
        fail_prob = prob_fail_df.loc[utilunit][degree_temp]
        
        if MRR_method:
            #print('trying pjm')
            try:
                init_state = init_avail_df.loc[convert_datestr_to_unavail_index(datestr)][utilunit]
            except KeyError:
                init_state = 0 #assume a generator with no info about its initial state is fully available
            new_forced_outage.append(enumerate_state_probs(init_state,1,recover_prob,fail_prob))
            #if utilunit == '417-102':
            #    print(prob_fail_df.loc[utilunit][-20])
            #    print(prob_fail_df.loc[utilunit][0])
            #    print(degree_temp,recover_prob,fail_prob)
            #    print(enumerate_state_probs(init_state,1,recover_prob,fail_prob))
        elif hourly_stack_wtemp.loc[index]['timepoint'] == 1:
            #print('default')
            try:
                init_state = init_avail_df.loc[convert_datestr_to_unavail_index(datestr)][utilunit]
            except KeyError:
                init_state = 0 #assume a generator with no info about its initial state is fully available
            new_forced_outage.append(enumerate_state_probs(init_state,1,recover_prob,fail_prob))
        else:
            #print('default')
            prev_FOR = new_forced_outage[index-n_gens]
            calc_FOR = prev_FOR*enumerate_state_probs(1,1,recover_prob,fail_prob) + (1-prev_FOR)*enumerate_state_probs(0,1,recover_prob,fail_prob)
            new_forced_outage.append(calc_FOR)
        
       
        
        
        #old
        if dynamic_ORDC:
            #this is entirely defunct from use
            match_temp = hourly_stack_wtemp.loc[index]['rounded_temp']
            match_category = hourly_stack_wtemp.loc[index]['Category']
            matched_forced_outage.append(float(forced_out_df.loc[match_category][match_temp]))
        elif MRR_method:
             matched_forced_outage.append(.003346) #this is strictly a placeholder and is obviously wrong, but for now in the absence of more data from Sinnott
             #my method here is to take the unweighted capacity average failure probability  (from failure.probabilities.1845.gens.1995.2018.xRS.and.pooled.fits.050919)
             #at the average CONUS temperature (12 degrees C; https://www.currentresults.com/Weather/US/average-annual-state-temperatures.php)
        else:
            match_category = hourly_stack_wtemp.loc[index]['Category']
            matched_forced_outage.append(float(fixed_forced_out_df.loc[match_category]['FOR']))
            
    hourly_stack_wtemp['GenFOR']=matched_forced_outage
    hourly_stack_wtemp['NEWFOR']=new_forced_outage
    #hourly_stack_wtemp.to_csv('stack_dispatch.csv')
    
    
    #manual outage dist for now but can add later
    manual_outage_dist = _np.array([[.05, 0.16887827],
        [.1, 0.11924128],
        [.15, 0.05676093],
        [.2, 0.0660266],
        [.25, 0.0441866],
        [.3, 0.030344],
        [.35, 0.02460712],
        [.4, 0.02042499],
        [.45, 0.02162338],
        [.5, 0.02205312],
        [.55, 0.04304062],
        [.6, 0.00962204],
        [.65, 0.0061247],
        [.7, 0.00523377],
        [.75, 0.00321084],
        [.8, 0.00336457],
        [.85, 0.00405984],
        [.9,0.01485232],
        [.95,0.00916784],
        [1.,0.32717719]])

    #voll = 3500 #probably want to input/change this at some point but OK for now
    store_lolp =[]
    store_primarysynch_segment = []
    store_primarynonsynch_segment = []
    store_secondary_segment = []
    store_timepoint = []
    
    if MRR_method:
        n_segments -= 1 #n_segments -= 1 #cut one segment since you set a MRR on the first segment
    
    for t in list(hourly_stack_wtemp.timepoint.unique()):
        if MRR_method:
            #synch_val = 1500
            store_lolp.append(VOLL) #will want to replace this will
            store_primarysynch_segment.append(MRRs['synch'])
            store_primarynonsynch_segment.append(MRRs['nonsynch'])
            store_secondary_segment.append(reserve_requirement_np[t-1]) #must offset by 1 due to pythonic indexing
            store_timepoint.append(t)
            
    #for t in range(1,2):                
        print('original conventional dispatch for hour ' + str(t) + ' is ' + str(hourly_stack_wtemp.Dispatch[hourly_stack_wtemp.timepoint==t].sum()))
        copt_table = copt_calc(hourly_stack_wtemp[hourly_stack_wtemp.timepoint==t],manual_outage_dist,
                               generator_level_outage_magnitude_df,unit_type_outage_magnitude_df,dynamic_ORDC)
        #if t==1:
            #copt_df = pd.DataFrame(copt_table)
            #copt_df.to_csv("copt_t1.csv")
        #hourly_copt_list.append(copt_calc(hourly_stack_wtemp[hourly_stack_wtemp.timepoint==t],manual_outage_dist))
        
        #lowcut_lolp = .00001 #cutoff lolp for min segment, probably want to do this as input but OK for now
        #n_segments = 10 #segments in ORDC, probably want to do this as input but OK for now
        #print(copt_table)
        max_disp = int(hourly_stack_wtemp.Dispatch[hourly_stack_wtemp.timepoint==(t)].sum())
        cutoff_disp_array = _np.where((_np.cumsum(copt_table[:,1])>lowcutLOLP)==True)
        cutoff_disp_index = min(cutoff_disp_array[0])
        cutoff_disp = copt_table[cutoff_disp_index,0]
        #print(cutoff_disp, max_disp, (max_disp-cutoff_disp)/n_segments)
    
        seg_length = int((max_disp-cutoff_disp)/(n_segments)) #divvy up ORDC segments in equal lengths
        
        for v in range(max_disp, int(cutoff_disp), -seg_length):
            #get midpoint
            midpoint = v - seg_length/2
            if midpoint > int(cutoff_disp):
                #find cumulative lolp at midpoint
                cutoff_array = _np.where((copt_table[:,0]<midpoint)==True)
                cutoff_index = max(cutoff_array[0])
                cumulative_lolp = max(_np.cumsum(copt_table[:(cutoff_index+1),1]))
                #store lolp*voll
                store_lolp.append(cumulative_lolp*VOLL)
                #create ordc segments
                #store_primarysynch_segment.append() #yikes
                store_primarysynch_segment.append((2./3.)*seg_length*primary_reserve_scalar)
                store_primarynonsynch_segment.append(seg_length*primary_reserve_scalar) #scaled by fraction of hour for now, though not exact method
                store_secondary_segment.append(seg_length*secondary_reserve_scalar)
                #store timepoint
                store_timepoint.append(t)
    
    #note that synchMW is now 2/3 of non-synch, based on historic PJM practice. This means these requirements
    #are **NOT** based on the single largest contingency
    
    if MRR_method:
        n_segments += 1 #add the segment that was cut earlier back in
    
    segment_df = pd.DataFrame({'timepoint':store_timepoint,
              'segments':list(range(1,n_segments+1))*int(max(hourly_stack_wtemp.timepoint.unique())),
              'SynchMW': store_primarysynch_segment,
              'NonSynchMW':store_primarynonsynch_segment,
              'SecondaryMW':store_secondary_segment,
              'Price':store_lolp})
    #segment_df =  pd.DataFrame({'timepoint':store_timepoint,
    #          'segments':list(range(1,n_segments+1))*int(1),
    #          'MW':store_secondary_segment,
    #          'Price':store_lolp})
        
    return segment_df

###      ####        ###
### HELPER FUNCTIONS ###
###      ####        ###
    
def custom_round(x, base, addC):
    """
    rounds temperatures to nearest "base" degrees as defined when the function is called
    if chosen, then appends a C and returns as a string so it matches input format
    there is no check currently that temperatures are in-range (i.e., between -30 and 40C)
    """
    if addC:
        return str(int(base * round(float(x)/base)))+"C"
    else:
        return int(base * round(float(x)/base))
    

def copt_calc(case_data, manual_outage_dist, gen_mag_df, unit_mag_df, is_dynamic):
    """
    Similar to how done in RECAP, but modified for our use case
    assumes df in our format, as well as outage distribution
    returns n-by-2 numpy array of hourly available capacity levels and their associated probability
    """
    COPT = _np.array([1.])

    for i in case_data.index.tolist():
        gen_capacity = case_data.loc[i]["Dispatch"]
        
        #is_dynamic makes the new method the default for dyanmic ORDC. Old ORDC will still use heurstic manual FOR dist and magnitudes
        if is_dynamic:
            gen_for = case_data.loc[i]["NEWFOR"]
            try: #try finding the outage magnitude by generator
                man_FOR = gen_mag_df.loc[case_data.loc[i]['UTILUNIT']]['x']
                manual_outage_dist = create_manual_dist(man_FOR,0.32717719) #overwrite the manual FOR dist
            except KeyError: #if the generator ID is not found, do it by generic unit type
                man_FOR = unit_mag_df.loc[case_data.loc[i]['Category']]['x']
                manual_outage_dist = create_manual_dist(man_FOR,0.32717719)
        else:
            gen_for = case_data.loc[i]["GenFOR"]
        
        dist = makegendist(gen_capacity, gen_for, manual_outage_dist)
        COPT = scipy.signal.fftconvolve(COPT,
                                        space_dist(_np.vstack((_np.arange(len(dist)),dist)).T)[:,1])
            
    COPT[_np.nonzero(COPT<0)]=0
    
    lowprobcut = 10**(-9) #same as in RECAP, to get rid of rounding error
    min_capacity = min(_np.nonzero(_np.cumsum(COPT)>lowprobcut)[0])
    COPT/=sum(COPT)
    
    return _np.vstack((_np.arange(min_capacity,len(COPT)),COPT[min_capacity:])).T

def space_dist(dist):
    """
    Takes a distribution and spaces it in integer increments merging the probability distribution where necessary
    """
    dist = dist[dist[:,0].argsort(),] #sorts the distribution on the first column
    
    mini = dist[min(_np.nonzero(_np.cumsum(dist[:,1])>10**(-9))[0]),0]
    maxi = dist[-min(_np.nonzero(_np.cumsum(dist[::-1,1])>10**(-9))[0])-1,0]
    
    dist = dist[_np.all([dist[:,0]>=mini,dist[:,0]<=maxi], axis = 0)]
    
    temp = dist_of_zeros(mini, maxi)
    
    if _np.all(_np.round(dist[:,0])==dist[:,0]):
        temp[_np.array(_np.round(dist[:,0])-mini, dtype = int),1] = dist[:,1]
        return temp
    else:
        dist[:,0] = _np.round(dist[:,0])
        for n in _np.unique(dist[:,0]):
            temp[int(n-mini),1] = _np.sum((dist[:,0]==n)*dist[:,1])
        return temp

def dist_of_zeros(mini, maxi):
    return _np.vstack((_np.arange(round(mini), round(maxi)+1), _np.zeros(int(round(maxi))-int(round(mini))+1))).T
    
def makegendist(capacity, FOR, outagedist):
    
    if capacity<1.: #changed this from 0.5 to 1. to solve some rounding errors with low capacity units
        return _np.array([1.]) #simplified for low capacity units

    dist = outage_dist(capacity, FOR, outagedist)
    dist = _np.hstack((_np.zeros(int(min(dist[:,0]))), dist[:,1]))
    
    #print(capacity, round(sum(dist),6))
    assert round(sum(dist),6)==1. #check that distribution still makes FORs sum to 1

    return dist

def outage_dist(cap, FOR, outagedist):
    return_dist = _np.ones((len(outagedist)+1,2))
    if _np.dot(outagedist[:,0], outagedist[:,1]) != 0 and (1.-FOR/_np.dot(outagedist[:,0], outagedist[:,1]))>0:
        return_dist[0,1] = (1.-FOR/_np.dot(outagedist[:,0], outagedist[:,1]))
        return_dist[1:,0]-=outagedist[:,0]
        return_dist[1:,1] = outagedist[:,1]*(1-return_dist[0,1])
    else:
        return_dist[1:,1] = outagedist[:,1][:]*FOR
        return_dist[0,1] = 1-FOR
        return_dist[1:,0]-=outagedist[:,0]
        
    return_dist[:,0]*=cap
    return space_dist(return_dist)

## NEW HELPER FUNCTIONS ##
#for dealing with Markov approach to generator outages
    
def create_manual_dist(FOR, p_full):
    min_partial = .1 #a bit arbitrary, but assume for starters that the average partial outage min is a 10% derate for all gens
    if FOR < min_partial: #keeps recursion from depthing out if a generator has a lower outage level than the min_partial
        min_partial = FOR
    p_partial = 1.-p_full
    mag_partial = (FOR - p_full)/p_partial
    if mag_partial < min_partial:
        return create_manual_dist(FOR, p_full*.5)
    
    return _np.array([[mag_partial,p_partial],[1,p_full]])

    
def sliding(a, n):
    return (a[i:i+n] for i in range(len(a) - n + 1))

def substring_count(a, b):
    return sum(s == b for s in sliding(a, len(b)))

def enumerate_state_probs(init_state,transitions,trans_prob_up,trans_prob_down):
    possible_transitions = 2**transitions
    combos = list(itertools.product(["U", "D"], repeat=transitions))
    if init_state <= .01:
        combos = [["U"]+list(tup) for tup in combos]
    else:
        combos = [["D"]+list(tup) for tup in combos]
    state_match = ["U,U","D,U","U,D","D,D"]
    prob_match = [1-trans_prob_down,trans_prob_up,trans_prob_down,1-trans_prob_up]
    
    #initialize lists
    init_state_list = []
    transitions_combo = []
    final_state = []
    probability = []
    #run loop
    for p in range(1,possible_transitions+1):
        init_state_list.append(init_state)
        transitions_combo.append(combos[p-1])
        final_state.append(transitions_combo[-1][-1])
        
        transition_counts = []
        for s in range(len(state_match)):
            combo_as_str = ','.join(combos[p-1])
            transition_counts.append(substring_count(combo_as_str, state_match[s]))
        trans_probs = _np.power(prob_match,transition_counts)
        probability.append(_np.prod(trans_probs))
    df = pd.DataFrame(
    {'init_state': init_state_list,
     'transitions_combo': transitions_combo,
     'final_state': final_state,
     'probability': probability
    })
    #print(df)
    #p_up = sum(df.probability[df.final_state=="U"])
    p_down = sum(df.probability[df.final_state=="D"])
    return p_down

def convert_datestr_to_unavail_index(datestr):
    datestr = datestr.replace(".","/")
    as_datetime = datetime.strptime(datestr, '%m/%d/%Y')-timedelta(days=1)+timedelta(hours=23) #hours needs to be as for Sinnott's thing
    back_to_str = as_datetime.strftime('%#m/%#d/%Y %H:%M') #hashes may only work on Windows, so be careful if running on Mac
    datestr = back_to_str
    
    #datestr = datestr + ' 23:00' #needs to be hour in Sinnott's df
    return datestr

## END HELPER FUNCTIONS ##

#run this script internally to check if desired

'''
month = "Jan"
input_directory = "C:\\Users\\Luke\\Desktop\\dispatch_RA_model-master\\raw_data"
results_directory = "C:\\Users\\Luke\\Desktop\\dispatch_RA_model-master\\Jan_4_10_2014_SimpleORDCMRR\\1.4.2014\\inputs"
###some key inputs to overwrite
dynamic_ORDC_input = False
MRR_method_input = True
###

#ordc_df = load_and_run_ordc(input_directory, results_directory,
#                                                month, hydro_cf, VOLL, lowcutLOLP, n_segments, True,
#                                                dates[0])
ordc_df = load_and_run_ordc(input_directory, results_directory,
                                                month, hydro_cf, VOLL, lowcutLOLP, 10, dynamic_ORDC_input,
                                                dates[0], primary_reserve_scalar, secondary_reserve_scalar,
                                                MRR_method_input, MRRs, lfe, FOR_fe)
print(ordc_df)
ordc_df.to_csv('ordc_check.csv')


#how long?
end_time = time.time() - start_time
print ("time elapsed during run is " + str(end_time) + " seconds")
'''