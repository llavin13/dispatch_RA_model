# -*- coding: utf-8 -*-
"""
Created on Sun Apr 14 19:41:54 2019

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
#import raw_data_imports

## DATA MANIPULATION TO CREATE INPUT FILES ##

def create_gens_init(gens):
    gens = gens.sort_values('X')
    gens = gens.set_index('UNITNAME')
    gen_index = []
    commit = []
    up = []
    down = []
    for g in list(gens.index):
        gen_index.append(g)
        commit.append(1)
        up.append(200)
        down.append(0)
    df = pd.DataFrame(
    {'Gen_Index': gen_index,
     'commit_init': commit,
     'time_up_init': up,
     'time_down_init': down
    })
    return df

def create_generator_segments(segments):
    segment_length = 1./float(segments)
    index = []
    segment_length_list = []
    for gs in range(1,segments+1):
        index.append(gs)
        segment_length_list.append(segment_length)
    df = pd.DataFrame(
        {'generator_segment': index,
         'length': segment_length_list
         })
    return df

def create_generator_segment_mc(segments, gens):
    
    gens_reindex = gens.sort_values('X')
    gens_reindex = gens_reindex.set_index('UNITNAME')
    
    gen_index = []
    segment_index = []
    marginal_cost_list = []    
    for gs in range(1,segments+1):
        col_name = "segment"+str(gs) #MUST ADD THESE NAMES INTO FILE!!
        for g in list(gens_reindex.index):
            gen_index.append(g)
            segment_index.append(gs)
            
            piecewise_heatrate_lookup = gens_reindex.loc[g][col_name]
            modeled_unit_mc = gens_reindex.loc[g]['FuelCost']*gens_reindex.loc[g]['GEN_HEATRATE']*piecewise_heatrate_lookup + gens_reindex.loc[g]['NREL_V_OM']
            marginal_cost_list.append(modeled_unit_mc)

    df = pd.DataFrame(
        {'Gen_Index': gen_index,
         'generator_segment': segment_index,
         'marginal_cost': marginal_cost_list
         })
    
    return df

def create_zonal_timepoints(zone_df, zone_list, load_df, wind_shape, solar_shape, lda_load_df, hydro_df, hydro_fracs):

    #get the lda load df ready
    lda_load_df['ZONE'] = lda_load_df['ISO Zone']
    lda_load_df_wzone = pd.merge(lda_load_df, zone_df, on='ZONE')
    #want to re-index by timepoint
    tmps = lda_load_df_wzone.shape[0]/len(lda_load_df_wzone.ZONE.unique())
    tmp_indices = len(lda_load_df_wzone.ZONE.unique())*list(range(1,int(tmps)+1))
    lda_load_df_wzone['new_index'] = tmp_indices
    lda_load_df_wzone = lda_load_df_wzone.set_index('new_index')
    lda_load_df_wzone['loadmw'] = lda_load_df_wzone['Load MW']
    lda_load_df_wzone['isozone'] = lda_load_df_wzone['ISO Zone']
    
    hydro_df['indices'] = list(range(1,load_df.shape[0]+1))
    hydro_df = hydro_df.set_index('indices')
    hydro_fracs = hydro_fracs.set_index('zone')
    
    zone_index = []
    time_index = []
    assigned_load = []
    wind_cf = []
    solar_cf = []
    max_hydro = []
    min_hydro = []
    ramp_hydro = []
    for z in zone_list:
        for t in range(1,load_df.shape[0]+1):
            #print(load_df.iloc[t-1])
            #print(lda_load_df_wzone.isozone[(lda_load_df_wzone.index==t) & (lda_load_df_wzone.Assigned_Zone==z)])
            #print(sum(lda_load_df_wzone.loadmw[(lda_load_df_wzone.index==t) & (lda_load_df_wzone.Assigned_Zone==z)]))
            time_index.append(t)
            zone_index.append(z)
            #assigned_load.append(load_df.iloc[t-1]*sum(zone_df.Frac_Load[zone_df.Assigned_Zone==z])) ##OLDMETHOD
            assigned_load.append(sum(lda_load_df_wzone.loadmw[(lda_load_df_wzone.index==t) & (lda_load_df_wzone.Assigned_Zone==z)]))
            wind_cf.append(wind_shape.iloc[t-1])
            solar_cf.append(solar_shape.iloc[t-1])
            max_hydro.append(hydro_df.iloc[t-1]['Max']*hydro_fracs.loc[z]['hydro_frac'])
            min_hydro.append(hydro_df.iloc[t-1]['Min']*hydro_fracs.loc[z]['hydro_frac'])
            ramp_hydro.append(hydro_df.iloc[t-1]['RampLim']*hydro_fracs.loc[z]['hydro_frac'])
    df = pd.DataFrame(
    {'timepoint': time_index,
     'zone': zone_index,
     'gross_load': assigned_load,
     'wind_cf': wind_cf,
     'solar_cf': solar_cf,
     'max_hydro': max_hydro,
     'min_hydro': min_hydro,
     'ramp_hydro': ramp_hydro
    })
    
    return df
'''
def create_lines(lines, zone_list):
    line_names = []
    from_zone = []
    to_zone = []
    min_flow = []
    max_flow = []
    losses_frac = []
    count_z = 0
    for z in zone_list:
        count_l = 0
        for l in zone_list:
            if len(lines[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)]) == 1:
                index_val = lines[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)].index[0]
                from_str = str(lines.tx_from_zone[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)][index_val])
                to_str = str(lines.tx_to_zone[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)][index_val])
                line_names.append((str(from_str)+"_to_"+str(to_str)))
                from_zone.append(from_str)
                to_zone.append(to_str)
                min_flow.append(lines.min_flow[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)][index_val])
                max_flow.append(lines.max_flow[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)][index_val])
                losses_frac.append(.02)
            elif count_l > count_z:
                line_names.append(z+"_to_"+l)
                from_zone.append(z)
                to_zone.append(l)
                min_flow.append(0)
                max_flow.append(0)
                losses_frac.append(.02)
            count_l+=1
        count_z+=1
    df = pd.DataFrame({'transmission_line': line_names,
     'transmission_from': from_zone,
     'transmission_to': to_zone,
     'min_flow': min_flow,
     'max_flow': max_flow,
     'line_losses_frac': losses_frac
    })

    return df

'''

def create_lines(lines, zone_list):
    line_names = []
    old = []
    count_z = 0
    for z in zone_list:
        count_l = 0
        for l in zone_list:
            if len(lines[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)]) == 1:
                index_val = lines[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)].index[0]
                from_str = str(lines.tx_from_zone[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)][index_val])
                to_str = str(lines.tx_to_zone[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)][index_val])
                line_names.append((str(from_str)+"_to_"+str(to_str)))
                old.append(0.1)
            elif count_l > count_z:
                line_names.append(z+"_to_"+l)
                old.append(0)
            count_l+=1
        count_z+=1
    df = pd.DataFrame({'transmission_line': line_names,
     'old': old
    })

    return df

def create_hourly_lines(lines, zone_list, load_df_for_timepoints):
    time_index = []
    line_names = []
    from_zone = []
    to_zone = []
    min_flow = []
    max_flow = []
    hurdle_rate = []
    count_z = 0
    #print(lines.columns)
    for z in zone_list:
        count_l = 0
        for l in zone_list:
            if len(lines[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)]) >= 1:
                #print(lines[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)])
                for t in range(1,load_df_for_timepoints.shape[0]+1):
                    time_index.append(t)
                    index_val = lines[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)].index[t-1]
                    from_str = str(lines.tx_from_zone[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)][index_val])
                    to_str = str(lines.tx_to_zone[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)][index_val])
                    line_names.append((str(from_str)+"_to_"+str(to_str)))
                    from_zone.append(from_str)
                    to_zone.append(to_str)
                    min_flow.append((-1.)*lines.limit_mw[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)][index_val])
                    max_flow.append(lines.limit_mw[(lines.tx_from_zone==z) & (lines.tx_to_zone==l)][index_val])
                    hurdle_rate.append(2)
            elif count_l > count_z:
                for t in range(1,load_df_for_timepoints.shape[0]+1):
                    time_index.append(t)
                    line_names.append(z+"_to_"+l)
                    from_zone.append(z)
                    to_zone.append(l)
                    min_flow.append(0)
                    max_flow.append(0)
                    hurdle_rate.append(.25) #hurdle rate hopefully just reflects incremental loss cost
            count_l+=1
        count_z+=1
    df = pd.DataFrame({'timepoint': time_index,
    'transmission_line': line_names,
     'transmission_from': from_zone,
     'transmission_to': to_zone,
     'min_flow': min_flow,
     'max_flow': max_flow,
     'hurdle_rate': hurdle_rate
    })

    return df

def create_zones(zone_df, zone_list, subzone_status, wind, solar, hydro_df, hydro_fracs):
    #hydro_df['indices'] = list(range(1,load_df.shape[0]+1))
    #hydro_df = hydro_df.set_index('indices')
    hydro_fracs = hydro_fracs.set_index('zone')
    
    zone_index = []
    wind_cap = []
    solar_cap = []
    total_hydro = []
    
    for z in zone_list:
        zone_index.append(z)
        wind_cap.append(wind.iloc[0][str(z)])
        solar_cap.append(solar.iloc[0][str(z)])
        total_hydro.append(sum(hydro_df.Avg)*hydro_fracs.loc[z]['hydro_frac'])
        
        #wind_cap.append(sum(zone_df.wind_capacity_MW[zone_df.Assigned_Zone==z]))
        #solar_cap.append(sum(zone_df.solar_capacity_MW[zone_df.Assigned_Zone==z]))
    df = pd.DataFrame(
    {'zone': zone_index,
     'wind_cap': wind_cap,
     'solar_cap': solar_cap,
     'total_hydro': total_hydro,
     'in_sub_zone': subzone_status
    })
    
    return df

def knit_generator_zone(gens, zones, subzone_status, hydro_df):
    '''
    takes list of zones, and df of generators, and knits together to get the capacity of 
    each generator in a zone
    modifies capacity of hydro resources according to their monthly CF (a bit clunky for now)
    for now, format is for capacity to be 0 in zones where generator doesn't exist
    '''
    gens = gens.sort_values('X')
    
    minutes_in_tmp = 60 #number of minutes in timepoint in run, for scaling ramp rates
    hours_in_month = 744 #assumes 744 hours in month for calculating CF for hydro
    
    hydro_df['UTILUNIT_y']=hydro_df['UTILUNIT']
    gens_w_hydro = pd.merge(gens,hydro_df,on="UTILUNIT_y")
    gens_w_hydro = gens_w_hydro.set_index('UNITNAME')
    cap_factor = gens_w_hydro.NETACTGEN/(gens_w_hydro.RATINGMW_y*hours_in_month) 
    gens = gens.set_index('UNITNAME')
    gen_index = []
    zone_index = []
    gen_zone_cap = []
    ramp_rate = []
    ramp_start = []
    ramp_shut = []
    in_subzone = []
    for z in range(len(zones)):
        for g in list(gens.index):
            try:
                cf = cap_factor[g]
            except KeyError:
                cf = 1
            gen_index.append(g)
            zone_index.append(zones[z])
            cf=1 #overwrites to full capacity
            if zones[z] == gens.Assigned_Zone[g]:
                gen_zone_cap.append(max(0,gens.RATINGMW_y[g]*cf))
            else:
                gen_zone_cap.append(0)
            if zones[z] == gens.Assigned_Zone[g] and subzone_status[z]==1:
                in_subzone.append(1)
            else:
                in_subzone.append(0)
            ramp_rate.append(gens.RATINGMW_y[g]*gens.ramp_rate[g]*minutes_in_tmp) #gets the hourly ramp rate
            ramp_start.append(gens.RATINGMW_y[g]*gens.Pmin[g]*1.001) #I think ok
            ramp_shut.append(gens.RATINGMW_y[g]*gens.Pmin[g]*.1) #derate this bc of shutdown concerns
    df = pd.DataFrame(
    {'Gen_Index': gen_index,
     'zone': zone_index,
     'capacity': gen_zone_cap,
     'Ramp_Rate': ramp_rate,
     'Ramp_Start': ramp_start,
     'Ramp_Shutdown': ramp_shut,
     'In_Sub_Zone': in_subzone
    })
    return df

def create_scheduled_outage_file(n_timepoints, list_gens, unitmatch_ID, outage_schedule):
    '''
    takes a number of timepoints, and a list of the generators, and creates an outage schedule
    also takes in data on which units have scheduled outages
    scheduled outages are based on what was known at 6PM the previous day; i.e. around when the Day Ahead...
    ...Market would have cleared
    '''
    time_list = []
    gens_list = []
    scheduled_list = []
    length_day = 24 #will eventually want to change this, but OK to assume 24 daily timepoints for now

    for t in range(1,n_timepoints+1):
        for g in list_gens:
            match_ID = unitmatch_ID[g]
            try:
                match_time = int((t-1)/length_day) 
                scheduled_out = outage_schedule.iloc[match_time][match_ID] #reindexed because these are daily now
                #scheduled_out = outage_schedule.iloc[t-1][match_ID] #old indexing
            except (KeyError, TypeError) as e: #this is for when the ID doesn't work
                scheduled_out = 0 #just assume generator is fully available if it cannot match
            time_list.append(t)
            gens_list.append(g)
            scheduled_list.append(1-scheduled_out)
    df = pd.DataFrame(
    {'timepoint': time_list,
     'Gen_Index': gens_list,
     'available': scheduled_list
    })
    return df

def create_operating_reserve_curve(n_segments, price_cap):
    '''
    creates operating reserve demand curve
    is single curve for now, but should eventually be determined hourly
    quantity and price are dummy inputs for now
    '''
    segment_list = []
    primary_synch_segment_quantity = []
    primary_nonsynch_segment_quantity = []
    secondary_segment_quantity = []
    segment_price = []
    segment_dummy = 100 #made up for now
    for s in range(1,n_segments+1):
        segment_list.append(s)
        primary_synch_segment_quantity.append(segment_dummy)
        primary_nonsynch_segment_quantity.append(1.5*segment_dummy)
        secondary_segment_quantity.append(3.*segment_dummy)
        segment_price.append(price_cap/(s**2)) #made up for now
    df = pd.DataFrame(
    {'segments': segment_list,
     'SynchMW': primary_synch_segment_quantity,
     'NonSynchMW': primary_nonsynch_segment_quantity,
     'SecondaryMW': secondary_segment_quantity,
     'Price': segment_price
    })
    return df

def create_zone_assign_col(row):
    'not used right now'
    if row['Zone'] == "WEST":
        val = 0
    else:
        val = 1
    return val

def modify_min_up_down(orig_df, modifying_df):
    '''
    takes (1) dataframe of generator characteristics, and
    (2) dataframe of empirical min up/down times calculated from 2014 CEMS data provided by N. Muller
    
    returns modified df where matched ORIS code generators have their min up and down modified
    '''
    #match with oris codes, get unit type
    orig_df['index'] = orig_df.index #add index as col
    merged_up_down = orig_df.merge(modifying_df, left_on='ORISPL_x', right_on='ORISPL')
    merged_up_down.set_index('index',inplace=True)
    #print(merged_up_down.index)
    
    new_min_up = []
    new_min_down = []
    
    #do I also want to aggregate on unit type and assign that when unk?
    for i in orig_df.index:
        if i not in list(merged_up_down.index):
            #print(i)
            new_min_up.append(orig_df.loc[i]['minup'])
            new_min_down.append(orig_df.loc[i]['mindown'])
        else:
            up_val = min(merged_up_down.loc[i]['minup'],merged_up_down.loc[i]['MinUp'])
            new_min_up.append(up_val)
        
            down_val = min(merged_up_down.loc[i]['mindown'],merged_up_down.loc[i]['MinDown'])
            new_min_down.append(down_val)
    
    out_df = orig_df
    out_df['final_minup'] = new_min_up
    out_df['final_mindown'] = new_min_down    
    
    #print(out_df.head())
    
    return out_df

def reassign_fuel_costs(df,fuelcostcol,eia923df,gasprice_df):
    new_fuel_costs = []
    
    df_noDR = df[df['ID6_y']!='NA'] #get rid of DR
    
    merged_df = pd.merge(df_noDR,eia923df,how='left',left_on='ORISPL_x',right_on='Plant Id')
    #print(merged_df.columns)
    
    ## coal grouper ##
    st1_ids = merged_df.index[(merged_df['FUEL_GROUP']=='Coal') & (merged_df['ID6_y']=='ST1')]
    st2_ids = merged_df.index[(merged_df['FUEL_GROUP']=='Coal') & (merged_df['ID6_y']=='ST2')]
    coal_ids = list(st1_ids)+list(st2_ids)
    coal_df = merged_df.loc[coal_ids,:]
    coal_df_oris_group = coal_df.groupby(['ORISPL_y','STATE','Plant Name','Purchase Type','FUEL_GROUP','FUEL_COST','Regulated'],as_index=False).mean()
    coal_df_oris_group['$TOTAL_PAYMENT'] = coal_df_oris_group['FUEL_COST']*coal_df_oris_group['QUANTITY']*.01 #total pmt
    coal_df_oris_only = coal_df_oris_group.groupby(['ORISPL_y']).sum() #group on ORISPL ONLY
    #calculate avg price at oris level
    coal_df_oris_only['coal_price_per_mmbtu'] = coal_df_oris_only['$TOTAL_PAYMENT']/coal_df_oris_only['QUANTITY']
    #also create a frame that groups on state for later use
    coal_df_state = coal_df_oris_group.groupby(['STATE']).sum()
    coal_df_state['state_avg_$mmbtu'] = coal_df_state['$TOTAL_PAYMENT']/coal_df_state['QUANTITY']
    
    ## gas grouper ##
    cc_ids = merged_df.index[(merged_df['FUEL_GROUP']=='Natural Gas') & (merged_df['ID6_y']=='CC')]
    ct_ids = merged_df.index[(merged_df['FUEL_GROUP']=='Natural Gas') & (merged_df['ID6_y']=='CT')]
    gas_ids = list(cc_ids)+list(ct_ids)
    gas_df = merged_df.loc[gas_ids,:]
    
    if 'Natural Gas Transportation Service' in gas_df.columns:
        gas_transport_string = 'Natural Gas Transportation Service'
    elif 'Natural Gas Delivery Contract Type':
        gas_transport_string = 'Natural Gas Delivery Contract Type'
    else:
        print('ugh,again!')
    
    gas_df_oris_group = gas_df.groupby(['ORISPL_y','STATE','Plant Name','Purchase Type','FUEL_GROUP','FUEL_COST','Regulated',gas_transport_string],as_index=False).mean()
    gas_df_oris_group['$TOTAL_PAYMENT'] = gas_df_oris_group['FUEL_COST']*gas_df_oris_group['QUANTITY']*.01 #total pmt
    gas_df_oris_only = gas_df_oris_group.groupby(['ORISPL_y']).sum() #group on ORISPL ONLY
    #calculate avg price at oris level
    gas_df_oris_only['gas_price_per_mmbtu'] = gas_df_oris_only['$TOTAL_PAYMENT']/gas_df_oris_only['QUANTITY']
    #also create a frame that groups on state for later use
    gas_df_state = gas_df_oris_group.groupby(['STATE']).sum()
    gas_df_state['state_avg_$mmbtu'] = gas_df_state['$TOTAL_PAYMENT']/gas_df_state['QUANTITY']
    
    #print(gas_df_state)
    #print(gas_df_oris_only)
    
    for i in df.index:
        if df.loc[i,'new_ID']=='ST1' or df.loc[i,'new_ID']=='ST2':
            if df.loc[i,'ORISPL_y'] in coal_df_oris_only.index:
                new_price = coal_df_oris_only.loc[df.loc[i,'ORISPL_y'],'coal_price_per_mmbtu']
            elif df.loc[i,'STATE'] in coal_df_state.index:
                new_price = coal_df_state.loc[df.loc[i,'STATE'],'state_avg_$mmbtu']
            else:
                new_price = coal_df_state.loc['VA','state_avg_$mmbtu']
            new_fuel_costs.append(new_price)
        elif df.loc[i,'new_ID']=='CC' or df.loc[i,'new_ID']=='CT':
            if df.loc[i,'ORISPL_y'] in gas_df_oris_only.index:
                new_price = gas_df_oris_only.loc[df.loc[i,'ORISPL_y'],'gas_price_per_mmbtu']
            elif df.loc[i,'STATE'] in gas_df_state.index:
                new_price = gas_df_state.loc[df.loc[i,'STATE'],'state_avg_$mmbtu']
            else: #in the case of gas, prefer to assign prevailing hub price here for unobserved states
                hub = df.loc[i,'assigned_gashub']
                new_price = gasprice_df.loc[hub]['DeliveryPrice']
            new_fuel_costs.append(new_price)
        else:
            new_fuel_costs.append(df.loc[i,'FuelCost'])
              
    return new_fuel_costs

def redo_capacity_HR(gens_df,epa_df):
    
    #add a column for capacity-weighted HR contribution in epa_df
    matcher = epa_df.groupby("ORIS Plant Code")["Capacity (MW)"].sum()

    newcapacity = []
    for i in epa_df.index:
        origcap = epa_df.loc[i,'Capacity (MW)']
        capsum = matcher[epa_df.loc[i,'ORIS Plant Code']]
        newcapacity.append(origcap/capsum)
    epa_df['capacityfraction'] = newcapacity
    epa_df['HRcontribution'] = epa_df['capacityfraction']*epa_df['Heat Rate (Btu/kWh)']
    epa_oris_group_sum = epa_df.groupby(['ORIS Plant Code']).sum()
    epa_oris_group_sum_relevantcols = epa_oris_group_sum[['Capacity (MW)','HRcontribution']]
    
    #now replace the relevant columns in gens_df
    
    for i in gens_df.index:
        oris = gens_df.loc[i,'ORISPL_x']
        count = (gens_df['ORISPL_x'] == oris).sum()
        try:
            potential_newcap = epa_oris_group_sum_relevantcols.loc[oris,'Capacity (MW)']/count
        except KeyError:
            potential_newcap = 0.0
            
        try:
            potential_newheatrate = epa_oris_group_sum_relevantcols.loc[oris,'HRcontribution']
        except KeyError:
            potential_newheatrate = 0.0
            
        if potential_newcap < 0.1 and potential_newheatrate < 0.1: #
            continue
            #finalcapacity.append(merged_df.loc[i,'RATINGMW'])
        elif potential_newheatrate < 0.1:
            capscalar = potential_newcap/gens_df.loc[i,'RATINGMW_y']
            gens_df.loc[i,'startcost'] = gens_df.loc[i,'startcost']*capscalar
            gens_df.loc[i,'RATINGMW_y'] = potential_newcap
        elif potential_newcap < 0.1:
            HRscalar = (potential_newheatrate/1000)/gens_df.loc[i,'GEN_HEATRATE']
            gens_df.loc[i,'NO_LOAD_MMBTU'] = gens_df.loc[i,'NO_LOAD_MMBTU']*HRscalar
            gens_df.loc[i,'GEN_HEATRATE'] = potential_newheatrate/1000
        else:
            capscalar = potential_newcap/gens_df.loc[i,'RATINGMW_y']
            HRscalar = (potential_newheatrate/1000)/gens_df.loc[i,'GEN_HEATRATE']
            gens_df.loc[i,'NO_LOAD_MMBTU'] = gens_df.loc[i,'NO_LOAD_MMBTU']*capscalar*HRscalar
            gens_df.loc[i,'GEN_HEATRATE'] = potential_newheatrate/1000
            gens_df.loc[i,'startcost'] = gens_df.loc[i,'startcost']*capscalar
            gens_df.loc[i,'RATINGMW_y'] = potential_newcap
            #gens_df.loc[i,'startcost']
    
    return gens_df

## DUMP TO OUTPUT FILES ##

def write_data(data, results_directory, init, scenario_inputs_directory, date, inputs_directory,
               input_primary_reserve_scalar, input_secondary_reserve_scalar):
    print('writing results to output files...')
    loadMW = data[3]
    
    #write the timepoints of the input case to a file, just to have a record
    #this is IN NO WAY used by the optimization run
    timepoint_match_df = pd.DataFrame(
    {'model_timepoint': list(range(1,len(data[7])+1)),
     'input_datetime': list(data[7])
    })
    timepoint_match_df.to_csv(os.path.join(results_directory,"timepoint_input_record.csv"), index=False)
    
    #create segmented ORDC based on hourly load and temperature
    
    #write generators files
    gens = data[2]
    gen_types = pd.read_csv(os.path.join(scenario_inputs_directory,"gentype_inputs.csv"))
    merged_gens = pd.merge(gens, gen_types, on='ID6_y')
    merged_gens['startcost'] = merged_gens.start_scalar * merged_gens.RATINGMW_y
    
    
    #rewrite using gas price data
    print("re-doing gas prices")
    gasprice_data = data[13]
    is_firm = data[17] #new
    eia_923_data = data[18]
    dual_fuel = data[19]
    
    #print(gasprice_data)
    #print(gasprice_data.columns)
    #print(is_firm)
    #print(is_firm.columns)
    #print(eia_923_data)
    #print(eia_923_data.columns)
    #print(dual_fuel)
    #print(dual_fuel.columns)
    
    
    #join data on whether firm contract is held
    #print(gasprice_data)
    
    #add delivery charge to the original data
    delivery = 0.4 #in $/mmbtu
    gasprice_data['DeliveryPrice'] = gasprice_data['Wtd Avg Index $'] + delivery
    
    #gasprice_data.to_csv(os.path.join(inputs_directory,'gasprice.csv'),index=False)
    #eia_923_data.to_csv(os.path.join(inputs_directory,'eia923.csv'),index=False)
    #dual_fuel.to_csv(os.path.join(inputs_directory,'dualfuel.csv'),index=False)
    
    #initial load of the zone match file, though we'll use only for gas prices here
    zone_file = pd.read_csv(os.path.join(scenario_inputs_directory,"LDA_to_zone.csv"))
    gens_w_zone = pd.merge(merged_gens, zone_file, on='ZONE')
    gens_w_zone = gens_w_zone.sort_values('X')
    #print(gens_w_zone.head())
    
    gens_w_zone['index'] = gens_w_zone.index #add index as col
    gens_w_zone_and_firm = gens_w_zone.merge(is_firm, left_on='ORISPL_x', right_on='ORISPL')
    gens_w_zone_and_firm.set_index('index',inplace=True)
    
    # replace gas generators fuel cost with hub price
    #it'll be slow but do as an iterated list for now
    
    
    #actually another thing I may use is diesel fuel price, so note that
    
    diesel_fuel_cost = gens_w_zone['FuelCost'][gens_w_zone['ID6_y']=="DS"].mean()
    #print(diesel_fuel_cost)
    #print('some info on dates!')
    #print(date)
    #print(type(date))
    #print('end info on dates')
    
    '''
    new_fuel_price = []
    for g in gens_w_zone.index:
        if gens_w_zone.loc[g]['ID6_y'] == 'CC' or gens_w_zone.loc[g]['ID6_y'] == 'CT':
            fuel_flag = False
            if g in list(gens_w_zone_and_firm.index):
                firmbool = gens_w_zone_and_firm.loc[g]['FirmBool']
                if math.isnan(float(firmbool)):
                    fuel_flag = False
                elif float(firmbool)>=0.1: #flag as don't use spot price if firm contract is held for gas plant
                    #fuel_flag=False
                    fuel_flag = True
                else: #known that generator holds interruptible contract
                    #fuel_flag=True
                    fuel_flag = False
            hub = gens_w_zone.loc[g]['assigned_gashub']
            if fuel_flag: #this is the default options
                fuel_price = gasprice_data.loc[hub]['DeliveryPrice']
            else: #for the exception
                #fuel_price = gens_w_zone.loc[g]['FuelCost']
                #take oil price if higher than gas price, otherwise gas price
                #this REALLY should ONLY bind on select days with gas delivery problems
                #known examples are 1.6.2014-1.8.2014
                fuel_price = max(diesel_fuel_cost,gens_w_zone.loc[g]['FuelCost'])
            new_fuel_price.append(fuel_price)
        else:
            new_fuel_price.append(gens_w_zone.loc[g]['FuelCost'])
    '''
    
    new_lab = []
    for i in gens_w_zone.index:
        new_lab.append(gens_w_zone.loc[i,'ID6_y'])
        #if gens_w_zone.loc[i,'ID6_y']=='ST1':
        #    new_lab.append('CT')
        #else:
        #   new_lab.append(gens_w_zone.loc[i,'ID6_y'])
    gens_w_zone['new_ID'] = new_lab
    new_fuel_price = reassign_fuel_costs(gens_w_zone,'FuelCost',eia_923_data, gasprice_data)
    #line_df.to_csv(os.path.join(results_directory,"transmission_lines.csv"), index=False)
    #gens_w_zone.to_csv(os.path.join(inputs_directory,'gens_before_fuelchange.csv'),index=False)
    gens_w_zone['FuelCost'] = new_fuel_price #replaces old fuel costs with new fuel costs
    #print(len(new_fuel_price))
    #print(len(gens_w_zone.index))
    #gens_w_zone.to_csv(os.path.join(inputs_directory,'gens_after_fuelchange.csv'),index=False)
    #print(gens_w_zone['FuelCost'])
    print("finished re-doing gas prices")
    
    ###now also want to re-do capacity and heat rates
    print("re-doing capacity and heat rates...")
    epa_needs_data = data[20]
    #print(epa_needs_data.head())
    
    #print(gens_w_zone.columns)
    
    #gens_w_zone.to_csv(os.path.join(inputs_directory,'beforechanges.csv'),index=False)    
    new_gens_w_zone = redo_capacity_HR(gens_w_zone,epa_needs_data)
    #new_gens_w_zone.to_csv(os.path.join(inputs_directory,'afterchanges.csv'),index=False)  
    gens_w_zone = new_gens_w_zone #in case this owrks
    print("...end re-doing capacity and heat rates")
    
    '''
    Brian's Code
    
    # overwrite single gas price with henry hub values
    hubPrice = pd.read_excel(os.path.join(inputs_directory,"gas_price_pull.xlsx"))
    
    # subset to henry hub (for now) and downselect date
    price = hubPrice.loc[(hubPrice['Price Hub'] == "Henry") & (hubPrice['Delivery Date'] == pd.to_datetime(date))]
    price = price['Wtd Avg Index $'].iloc[0]
    
    # add delivery charge ( $ per mmBtu)
    delivery = 0.4
    price = price + delivery
    
    # replace gas generators fuel cost with hub price
    merged_gens.loc[merged_gens['ID6_y'].isin(['CC', 'CT']) , 'FuelCost'] = price
    '''
    
    # using fuel cost and heat rate to calculate marginal costs
    gens_w_zone['marginalcost'] = gens_w_zone.FuelCost * gens_w_zone.GEN_HEATRATE + gens_w_zone.NREL_V_OM
    gens_w_zone['noloadcost'] = gens_w_zone.NO_LOAD_MMBTU * gens_w_zone.FuelCost
    gens_w_zone = gens_w_zone.sort_values('X')
    #pjm_out = gens_w_zone[['UNITNAME','marginalcost','Pmin','startcost','can_spin','can_nonspin','minup','mindown','noloadcost']]
    #pjm_out.columns = ['Gen_Index',	'Fuel_Cost	','Pmin','start_cost','Can_Spin','Can_NonSpin','Min_Up','Min_Down','No_Load_Cost']
    ## ##
    pjm_out_new = modify_min_up_down(gens_w_zone,data[16])
    #final_minup
    pjm_out = pjm_out_new[['UNITNAME','marginalcost','Pmin','startcost','can_spin','can_nonspin','final_minup','final_mindown','noloadcost']]
    pjm_out.columns = ['Gen_Index',	'Fuel_Cost	','Pmin','start_cost','Can_Spin','Can_NonSpin','Min_Up','Min_Down','No_Load_Cost']
    ## ##
    pjm_out.to_csv(os.path.join(results_directory,"PJM_generators.csv"), index=False)
    
    if init:
        out_init = create_gens_init(gens_w_zone)
        out_init.to_csv(os.path.join(results_directory,"initialize_generators.csv"), index=False)
    pjm_out_full = gens_w_zone[['X','UNITNAME','ZONE','ID6_y','RATINGMW_y','marginalcost','can_spin','UTILUNIT_y','In_Sub_Zone']]
    pjm_out_full = pjm_out_full.sort_values('X')
    pjm_out_full.columns = ['Gen_Index',	'Name', 'Zone',	'Category',	'Capacity',	'Fuel_Cost',	'Can_Spin', 'UTILUNIT','In_Sub_Zone']
    #pjm_out_full['In_Sub_Zone'] = pjm_out_full.apply(create_zone_assign_col, axis=1)
    pjm_out_full.to_csv(os.path.join(results_directory,"PJM_generators_full.csv"), index=False)
    
    #create generator segments
    segment_df = create_generator_segments(data[0].value[11])
    segment_df.to_csv(os.path.join(results_directory,"generator_segments.csv"), index=False)
    
    #create marginal costs on segments
    mc_segment_df = create_generator_segment_mc(data[0].value[11], gens_w_zone)
    mc_segment_df.to_csv(os.path.join(results_directory,"generator_segment_marginalcost.csv"), index=False)
    
    #knit with zones, write zones file
    #this perhaps should be in the "only if zonal" clause of the script
    #recall we have now loaded the zone file a bit earlier
    zone_list = list(zone_file.Assigned_Zone.unique())
    if len(zone_list)==5: #my purposeful pjm sorting
        print('re-ordering zones according to my criteria and noting their subzone status')
        new_zone_list = []
        mad_subzone_status = []
        for z in [0,3,4,1,2]:
            new_zone_list.append(zone_list[z])
            if zone_list[z]=="WEST":
                mad_subzone_status.append(0)
            else:
                mad_subzone_status.append(1)
        zone_list = new_zone_list
    
    hydro_df = data[12]
    pjm_gens_zones = knit_generator_zone(gens_w_zone, zone_list, mad_subzone_status, hydro_df)
    pjm_gens_zones.to_csv(os.path.join(results_directory,"PJM_generators_zone.csv"), index=False)
    
    #attempt to re-do the hydro
    gens_w_zone_hydro_only = gens_w_zone[gens_w_zone.ID6_y=="HD"] #gets you only the hydro
    hydro_frac_list = []
    count_list = []
    count = 0
    for z in zone_list:
        count += 1
        hydro_frac_list.append(sum(gens_w_zone_hydro_only.RATINGMW_y[gens_w_zone_hydro_only.Assigned_Zone==z])/sum(gens_w_zone_hydro_only.RATINGMW_y))
        count_list.append(count)
    hydro_frac_df = pd.DataFrame(
    {'zone': zone_list,
     'hydro_frac': hydro_frac_list,
     'counter': count_list})
    
    #write scheduled outage file
    merged_gens_reindex = gens_w_zone.sort_values('X')
    merged_gens_reindex = merged_gens_reindex.set_index('UNITNAME')
    outage_schedule = data[11]
    scheduled_outage_df = create_scheduled_outage_file(loadMW.shape[0],list(merged_gens_reindex.index), merged_gens_reindex.UTILUNIT_y, outage_schedule)
    scheduled_outage_df.to_csv(os.path.join(results_directory,'PJM_generators_scheduled_outage.csv'), index=False)
    
    #write operating reserve file
    segment_int = int(data[0].value[6])
    cost_int = data[0].value[8]
    operating_reserve_df = create_operating_reserve_curve(segment_int,cost_int)
    operating_reserve_df.to_csv(os.path.join(results_directory,'operating_reserve_segments.csv'), index=False)
    
    #write timepoints file (just has temperatures for now)
    temperatures = pd.DataFrame(data[4])
    #temperatures = temperatures.rename(columns={temperatures.columns[0]: "temperature" })
    temperatures['timepoint']=list(range(1, temperatures.shape[0]+1))
    temperatures = temperatures.iloc[:, ::-1] #reorder to get timepoint first
    temperatures.to_csv(os.path.join(results_directory,"timepoints_index_allweather.csv"),index=False) 
    
    temperatures_short = temperatures.iloc[:,0:2] #grabs first two columns
    temperatures_short = temperatures_short.rename(columns={temperatures_short.columns[1]: "temperature" })
    #hourly_loads['ones'] = len(hourly_loads.index)*[1]
    temperatures_short['primaryreservescalar'] = len(temperatures_short.index)*[input_primary_reserve_scalar]
    temperatures_short['secondaryreservescalar'] = len(temperatures_short.index)*[input_secondary_reserve_scalar]
    temperatures_short['reservescalarratio'] = len(temperatures_short.index)*[input_secondary_reserve_scalar/input_primary_reserve_scalar]
    temperatures_short.to_csv(os.path.join(results_directory,"timepoints_index.csv"),index=False)
    
    if not data[0].value[5]:
        loadMW.to_csv(os.path.join(results_directory,"timepoints_zonal.csv"))
        zone_df = pd.DataFrame([["PJM", 15, 14]], columns = ['zone', 'wind_cap','solar_cap']) 
        zone_df.to_csv(os.path.join(results_directory,"zones.csv"))
    else:
        #create zones csv
        zone_df = create_zones(zone_file, zone_list, mad_subzone_status, data[9], data[10], data[15], hydro_frac_df) #now adds wind and solar installed data
        zone_df.to_csv(os.path.join(results_directory,"zones.csv"), index=False)
        #create timepoints/zones csv
        timepoints_zonal_df = create_zonal_timepoints(zone_file, zone_list, loadMW, data[5], data[6], data[14], data[15], hydro_frac_df)
        timepoints_zonal_df.to_csv(os.path.join(results_directory,"timepoints_zonal.csv"), index=False)
        #loadMW.to_csv(os.path.join(results_directory,"timepoints_zonal.csv"))
        #create transmission lines csv
        input_lines = pd.read_csv(os.path.join(scenario_inputs_directory,"transmission_lines_inputs.csv"))
        line_df = create_lines(input_lines, zone_list)
        line_df.to_csv(os.path.join(results_directory,"transmission_lines.csv"), index=False)
        
        #create alternate transmission lines csv
        hourly_lines = pd.merge(data[8], input_lines, on='interface_limit_name')
        hourly_line_output = create_hourly_lines(hourly_lines, zone_list, loadMW)
        hourly_line_output.to_csv(os.path.join(results_directory,"transmission_lines_hourly.csv"), index=False)
        
    print('...results written')
    return None