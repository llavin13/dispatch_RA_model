# -*- coding: utf-8 -*-
"""
Created on Fri Mar  8 13:40:52 2019

@author: llavi
"""

import os
from os.path import join
import pandas as pd
import numpy as np
import sys
import math
from datetime import datetime
from datetime import timedelta
from dateutil import parser

from case_inputs import *

def load_data(inputs_directory, scenario_inputs_directory, date):
    print('begin loading data...')
    #load base data; this is not pickled so it's always re-loaded by case
    
    #load other stuff
    #this stuff is loaded from pickles for speed, but be careful if you edit the underlying file
    try:
        forced_outage_rates = pd.read_pickle(".Forced.outage.rates.by.temperature.and.unit.type.102918")
    except FileNotFoundError:
        print('loading forced outage rates from csv because not yet pickled')
        forced_outage_rates = input_to_pickle(inputs_directory, "Forced.outage.rates.by.temperature.and.unit.type.102918.csv")
    
    #load loads
    try:
        loads = pd.read_pickle(".PJM.2006.pres.loads")
    except FileNotFoundError:
        print('loading load data from csv because not yet pickled')
        loads = input_to_pickle(inputs_directory,"PJM.2006.pres.loads.csv")
        
    #load wind/solar shape
    wind_solar = pd.read_csv(os.path.join(inputs_directory,"wind_solar_hour_shape.csv"))
    
    #load wind/solar zonal capacity
    wind_capacity_df = pd.read_excel(open(os.path.join(inputs_directory,'EIA_860_2017_Wind.xlsx'), 'rb'),sheet_name='for_tool')
    solar_capacity_df = pd.read_excel(open(os.path.join(inputs_directory,'EIA_860_2017_Solar.xlsx'), 'rb'),sheet_name='for_tool')
    
    #load generator dependent stuff
    try:
        temperature_matches = pd.read_pickle(".PJM.temperature.series.forward.interp.64.WBANs.052718")
    except FileNotFoundError:
        print('loading temperature data from csv because not yet pickled')
        temperature_matches = input_to_pickle(inputs_directory,"PJM.temperature.series.forward.interp.64.WBANs.052718.csv")
    units = pd.read_csv(os.path.join(inputs_directory,"PJM.units.processed.071818.csv"))
    units_zonal_match = pd.read_csv(os.path.join(inputs_directory,"GENERATORS_LL.csv"))
    
    #load zonal loads
    load_data = pd.read_csv(os.path.join(inputs_directory,"PJM_zonal_loads.csv"))
    #load also da forecast loads, added 6.12.20
    load_da_data = pd.read_csv(os.path.join(inputs_directory,"da_rt_loads.csv"))
    
    #load generator scheduled outages
    #scheduled_outages = pd.read_csv(os.path.join(inputs_directory,"fraction.unavailable.Jan14.csv"), index_col=0)
    scheduled_outages = pd.read_csv(os.path.join(inputs_directory,"fraction.unavailable.xNCRSU7.all.050919.csv"), index_col=0)
    
    #load hub-level gas price data
    gas_hub_prices = pd.read_csv(os.path.join(inputs_directory,"gasprice_jan2014.csv"), index_col=0) #index will be the hubname
    
    #load hydro derates
    hydro_sheet_csv = str(hydro_sheet)+".csv"
    hydro_derates = pd.read_csv(os.path.join(inputs_directory,hydro_sheet_csv))
    
    #load hydro params
    hydro_params = pd.read_csv(os.path.join(inputs_directory,"hydro_params.csv"), index_col=0)
    
    #load line flow data
    line_limits = pd.read_csv(os.path.join(inputs_directory,"da_interface_flows_and_limits_full.csv"))
    
    #load CEMS data for calculating min up/down times
    #could eventually be used for emissions factors
    #added 10.29.19
    cems_data = pd.read_csv(os.path.join(inputs_directory,'Lavin_RFC_Hourly_2014.txt'), delimiter="\t")
    
    #load info on contract status of gas plants from Gerad (added here 10.29.19)
    eia_923_firmgas = pd.read_csv(os.path.join(inputs_directory,'FractionFirmTable1222019.csv'))
    
    #load info on dual-fuel capability of gas plants from Gerad (added here 11.15.19)
    dual_fuel = pd.read_csv(os.path.join(inputs_directory,'PJM.unit.attributes.dual.LDC.11122019.csv'))
    
    #load epa needs database data (added here 12.2.19)
    epa_needs = pd.read_excel(os.path.join(inputs_directory,'needs_v6_09-30-19.xlsx'),sheet_name='NEEDS v6_active')
    
    #create cleaning dates
    startdate = parser.parse(date)
    enddate = startdate + timedelta(days, hours=-1)
    enddate_max = startdate + timedelta(math.ceil(days), hours=-1) #use only for things that need to look through whole day
    enddate_min = startdate + timedelta(1,hours=-1) #always just the same day
    
    #also month and year
    month = enddate.month
    year = enddate.year
    
    #EIA 923, csvs are split by year in their own special subfolder so wait until here to do it
    eia_inputs_directory = inputs_directory+"\\EIA923"
    eia_xlsx_name = "EIA923_Schedules_2_3_4_5_M_12_"+str(year)+"_Final_Revision.xlsx"
    eia_923_full = pd.read_excel(os.path.join(eia_inputs_directory,eia_xlsx_name), sheet_name='Page 5 Fuel Receipts and Costs', header=4)
    #print(eia_923_full.head())
    
    print('end loading data, begin cleaning data...')
    
    #clean hydro params
    clean_hydro_params = hydro_param_clean(hydro_params, startdate+timedelta(0,hours=1), enddate+timedelta(0,hours=1))
    
    #clean loads
    clean_zonal_loads = zonal_load_clean(load_data, startdate+timedelta(0,hours=1), enddate+timedelta(0,hours=1))
    #clean da loads, added 6.12.20
    clean_da_zonal_loads = zonal_load_clean(load_da_data, startdate+timedelta(0,hours=1), enddate+timedelta(0,hours=1))
    
    #clean scheduled generator outages
    #-1 date because scheduled outages are based on what was known the previous day
    clean_scheduled_outages = scheduled_outage_clean(scheduled_outages, 
                                                     startdate+timedelta(-1), enddate_max+timedelta(-1))
    
    #clean gas prices
    clean_gas_hub_prices = gas_price_clean(gas_hub_prices, startdate, enddate_min) #so that this always pulls only one gas price
    
    #clean line flow limits
    clean_line_limits = line_clean(line_limits, startdate, enddate)
    
    #clean wind and solar data
    wind = vre_time_clean("wind", wind_solar, startdate, enddate)
    solar = vre_time_clean("solar", wind_solar, startdate, enddate)
    
    #clean wind and solar capacities to get the appropriate level for that month
    wind_capacity = vre_capacity_time_clean("wind", wind_capacity_df, startdate, enddate)
    solar_capacity = vre_capacity_time_clean("solar", solar_capacity_df, startdate, enddate)
    
    #clean temperatures
    temperatures = temperature_time_clean(temperature_matches, startdate, enddate)
    
    #clean gens
    gens = generator_module(units,units_zonal_match)
    gens = gens_time_clean(gens, startdate, enddate)
    
    #add dr
    DR_cost = VOLL   #cost of DR is set to the VOLL by default

    listOfSeries = [pd.Series([1846,(max(gens.X)+1),1,1,1,'DR1','DR1','NA','NA','NA','NA','NA','NA','NA','NA'
          ,'NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA'
          ,'NA','NA','NA','NA','NA','NA','1/1/1980','NA','NA','NA','NA','NA','NA','NA','NA','NA','DR'
          ,'NA',10000,'NA','NA','NA','NA','PECO',DR_cost,0,1,1,1,1], index=gens.columns),
    pd.Series([1847,(max(gens.X)+2),1,1,1,'DR2','DR2','NA','NA','NA','NA','NA','NA','NA','NA'
          ,'NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA'
          ,'NA','NA','NA','NA','NA','NA','1/1/1980','NA','NA','NA','NA','NA','NA','NA','NA','NA','DR'
          ,'NA',10000,'NA','NA','NA','NA','COMED',DR_cost,0,1,1,1,1], index=gens.columns),
    pd.Series([1848,(max(gens.X)+3),1,1,1,'DR3','DR3','NA','NA','NA','NA','NA','NA','NA','NA'
          ,'NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA'
          ,'NA','NA','NA','NA','NA','NA','1/1/1980','NA','NA','NA','NA','NA','NA','NA','NA','NA','DR'
          ,'NA',10000,'NA','NA','NA','NA','PEPCO',DR_cost,0,1,1,1,1], index=gens.columns),
    pd.Series([1849,(max(gens.X)+4),1,1,1,'DR4','DR4','NA','NA','NA','NA','NA','NA','NA','NA'
          ,'NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA'
          ,'NA','NA','NA','NA','NA','NA','1/1/1980','NA','NA','NA','NA','NA','NA','NA','NA','NA','DR'
          ,'NA',10000,'NA','NA','NA','NA','DOM',DR_cost,0,1,1,1,1], index=gens.columns),
    pd.Series([1850,(max(gens.X)+5),1,1,1,'DR5','DR5','NA','NA','NA','NA','NA','NA','NA','NA'
          ,'NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA','NA'
          ,'NA','NA','NA','NA','NA','NA','1/1/1980','NA','NA','NA','NA','NA','NA','NA','NA','NA','DR'
          ,'NA',10000,'NA','NA','NA','NA','PPL',DR_cost,0,1,1,1,1], index=gens.columns)]
    gens = gens.append(listOfSeries, ignore_index=True)
    
    #clean loads
    #this is the part that's taking the longest, so work on it at some point
    loadMW = loads_time_clean(loads, startdate+timedelta(0,hours=1), enddate+timedelta(0,hours=1))
    
    #clean CEMS data
    min_up_down_df = clean_cems(cems_data,'ORISPL')
    
    #clean EIA firmgas data
    eia_firmgas_df = clean_eia(eia_923_firmgas,month,year)
    
    #clean EIA 923 data
    eia_923_df = clean_923(eia_923_full,month)
    
    #printing to just quick check for now
    #print(min_up_down_df.head())
    #print(eia_923_df.head())
    
    # summarize base_inputs for use later (note: could simplify now that inputs are via script in case_inputs.py)
    base_inputs = pd.DataFrame({"value":[date, days, wfe, sfe, lfe, zones, n_segments, VOLL, contingency, lowcutLOLP, hydro_cf, n_generator_segments]},
                               index= ["Begin Date", "Duration", "Wind forecast error", "Solar forecast error", "Load forecast error",
                                       "Zones", "Demand Curve Segments", "VOLL", "Contingency Reserve Shed", "lowcutLOLP", "hydrocf", "n_generator_segments"])
    
    print('...return loaded and cleaned data as tuple')
    return (base_inputs, forced_outage_rates, gens, loadMW[0], temperatures, wind, solar, loadMW[1], 
            clean_line_limits, wind_capacity, solar_capacity, clean_scheduled_outages, hydro_derates,
            clean_gas_hub_prices, clean_zonal_loads, clean_hydro_params, min_up_down_df, eia_firmgas_df,
            eia_923_df, dual_fuel,epa_needs,clean_da_zonal_loads)

def str_to_datetime(my_str):
    '''
    used to convert pd series of dates that are originally strings to pandas datetimes
    '''
    if type(my_str) == float:
        return (my_str)
    else:
        return parser.parse(my_str)

def generator_module(units, zones):
    unit_zone = pd.merge(units, zones, on='X', how='outer')
    return unit_zone

def zonal_load_clean(load_df, startdate, enddate):
    '''
    takes df of loads, cleans to only include active timeperiod
    '''
    load_output = load_df
    load_output['date'] = load_output['Local Datetime (Hour Ending)']
    return load_output[(load_output.date.apply(str_to_datetime) >= startdate) & (enddate >= load_output.date.apply(str_to_datetime))]

def scheduled_outage_clean(outage_df, startdate, enddate):
    '''
    takes df of scheduled outage by unit, cleans to include only active timeperiod for case
    '''
    scheduled_outage_output = outage_df
    scheduled_outage_output['date']=scheduled_outage_output.index
    return scheduled_outage_output[(scheduled_outage_output.date.apply(str_to_datetime) >= startdate) & (enddate >= scheduled_outage_output.date.apply(str_to_datetime))]

def gas_price_clean(gasprice_df, startdate, enddate):
    '''
    takes df of gas prices at different hubs, cleans to include only active timeperiod for case
    '''
    gasprice_df['date'] = gasprice_df['Delivery Date']
    clean_gasprice_df = gasprice_df[(gasprice_df.date.apply(str_to_datetime) >= startdate) & (enddate >= gasprice_df.date.apply(str_to_datetime))]
    return clean_gasprice_df

def line_clean(lines, startdate, enddate):
    '''
    takes df of lines, subsets and outputs the active timeperiod
    '''
    line_output = lines[(lines.datetime_beginning_ept.apply(str_to_datetime) >= startdate) & (enddate >= lines.datetime_beginning_ept.apply(str_to_datetime))]
    return line_output
    
def gens_time_clean(gens, startdate, enddate):
    '''
    takes df of generators, subsets those that were active during the entire timeperiod of dispatch
    '''
    
    gens = gens[gens.COMMISSION_DATE.apply(str_to_datetime) <= startdate]
    gens = gens[(gens.RETIRED == 0) | (gens.RETIRED_DATE.apply(str_to_datetime) >= enddate)]
    
    return gens

def vre_time_clean(vre_type, vre_df, startdate, enddate):
    '''
    takes df of hourly vre normalized cap factor, subsets based on vre type what you want
    '''
    if vre_type == "solar":
        vre_df = vre_df[(vre_df.date.apply(str_to_datetime) >= startdate) & (enddate >= vre_df.date.apply(str_to_datetime))]
        return vre_df.pjm_pv_jeremy_scaled_3_28
    elif vre_type == "wind":
        vre_df = vre_df[(vre_df.date.apply(str_to_datetime) >= startdate) & (enddate >= vre_df.date.apply(str_to_datetime))]
        return vre_df.NREL_wind_scaled
    else:
        print('you can only return solar or wind data, this is a problem!!')
        return None
    
def vre_capacity_time_clean(vre_type, vre_df, startdate, enddate):
    '''
    very similar to vre_time_clean, but handles the input capacities rather than the input hourly shapes
    '''
    if vre_type == "solar":
        vre_df = vre_df[(vre_df.Equiv_End_Date.apply(str_to_datetime) >= startdate) & (enddate >= vre_df.Equiv_Begin_Date.apply(str_to_datetime))]
        return vre_df
    elif vre_type == "wind":
        vre_df = vre_df[(vre_df.Equiv_End_Date.apply(str_to_datetime) >= startdate) & (enddate >= vre_df.Equiv_Begin_Date.apply(str_to_datetime))]
        return vre_df
    else:
        print('you need to say whether this is wind or solar data. You input something else. Problem!')
        return None
    
def loads_time_clean(loads, startdate, enddate):
    '''
    takes df of loads, subsets those from the time period
    '''
    loads = loads[(loads.HourEnd.apply(str_to_datetime) >= startdate) & (enddate >= loads.HourEnd.apply(str_to_datetime))]
    return (loads.LoadMW,loads.HourEnd)

def temperature_time_clean(temps, startdate, enddate):
    '''
    takes df of temperatures, subsets and combines relevant location temperatures
    '''
    temps = temps.rename(columns={ temps.columns[0]: "date" })
    #temps = temps.rename(columns={ temps.columns[1]: "usedweather" }) #if you just wanted first column, but now defunct
    temps = temps[(temps.date.apply(str_to_datetime) >= startdate) & (enddate >= temps.date.apply(str_to_datetime))]
    return temps

def hydro_param_clean(hydro_df, startdate, enddate):
    '''
    cleans hydro parameters to leave only the active dates
    '''
    clean_hydro_df = hydro_df
    clean_hydro_df['date']=clean_hydro_df.index
    return clean_hydro_df[(clean_hydro_df.date.apply(str_to_datetime) >= startdate) & (enddate >= clean_hydro_df.date.apply(str_to_datetime))]

def clean_cems(cems_df,col_name):
    '''
    '''
    assert(col_name=='ORISPL')
    gens_oris_unique = cems_df[str(col_name)].unique()
    oris_code_list = []
    min_up_list = []
    min_down_list = []
    cap_compare = []
    
    for g in gens_oris_unique: #for each unique generator in RFC, determine min online time

        subframe = cems_df.loc[cems_df[col_name]==g] #df.loc[df['shield'] > 6]'
        subframe.reset_index(inplace=True)
        min_down = 8760
        min_up = 8760
        
        min_up_calc = 0
        min_down_calc = 0
        flag = 'down'
  
        for h in subframe.index:
            if h==subframe.index[0]:
                if subframe.loc[h,'grossloadmw']>=0.0:
                    flag = 'up'
                    min_up_calc += 1
                else:
                        min_down_calc += 1
            else:    
                if subframe.loc[h,'grossloadmw']>=0.0 and subframe.loc[h-1,'grossloadmw']>=0.0:
                    #print(h)
                    min_up_calc += 1
                elif math.isnan(subframe.loc[h,'grossloadmw']) and math.isnan(subframe.loc[h-1,'grossloadmw']):
                    min_down_calc+=1
                elif subframe.loc[h,'grossloadmw']>=0.0 and math.isnan(subframe.loc[h-1,'grossloadmw']):
                    #print(h)
                    flag = 'up'
                    min_up_calc=1
                    if min_down_calc < min_down:
                        min_down = min_down_calc
                    min_down_calc = 0
                elif math.isnan(subframe.loc[h,'grossloadmw']) and subframe.loc[h-1,'grossloadmw']>=0.0:
                    flag='down'
                    min_down_calc=1
                    if min_up_calc < min_up:
                        min_up = min_up_calc
                    min_up_calc = 0
    
        oris_code_list.append(g)
        min_up_list.append(min_up)
        min_down_list.append(min_down)
        cap_compare.append(subframe.loc[0,'nameplatecap'])

    #create output dataframe
    min_up_down_df = pd.DataFrame(list(zip(oris_code_list, min_up_list, min_down_list,cap_compare)), 
                                  columns =['ORISPL', 'MinUp','MinDown','Nameplate_CEMs']) 
    
    return min_up_down_df

def clean_eia(df, month, year):
    '''
    '''
    datetime_list = list(df['DateTime']) 
    df.drop('DateTime',axis=1,inplace=True)
    cols_list = []
    oris_list = []

    for c in df.columns:
        cols_list += list(df[c]) 
        c_list = [c]*len(df[c])
        oris_list += c_list

    datetime_list = datetime_list * len(df.columns)

    #zip up new df
    eia_923_stack_df = pd.DataFrame(list(zip(datetime_list, oris_list, cols_list)), 
                                    columns =['DateTime', 'ORISPL','FirmBool']) 

    #add column with month and year for future cleaning if desired
    month_list = []
    year_list = []
    for i in eia_923_stack_df.index:
        datetime = parser.parse(eia_923_stack_df.loc[i,'DateTime'])
        month_list.append(datetime.month)
        year_list.append(datetime.year)
    
    eia_923_stack_df['month'] = month_list
    eia_923_stack_df['year'] = year_list
    
    eia_month_df = eia_923_stack_df[(eia_923_stack_df['year']==year) & (eia_923_stack_df['month']==month)]
    
    #gas_df_oris_group.loc[:,'ORISPL']
    for i in eia_month_df.index:
        eia_month_df.loc[i,'ORISPL']=np.int64(eia_month_df.loc[i,'ORISPL']) #convert to np.int64 for matching
    #eia_month_df['ORISPL'].astype(np.int64)
    
    return eia_month_df

def clean_923(df,month):
    '''
    takes loaded eia 923 df (read in sheet 5 above) and month and retains only contracts from that month
    also cleans out entries without contract data
    '''
    df_month = df[df['MONTH']==int(month)]
    df_monthandfuelclean = df_month[df_month['FUEL_COST']!="."]
    return df_monthandfuelclean

def input_to_pickle(data_path, csv_string):
    '''
    takes input csv file and pickles it for faster loading in future
    '''
    csv_path = join(data_path, csv_string)
    df = pd.read_csv(csv_path)
    pickle_name = "." + csv_string[:-4]
    df.to_pickle(pickle_name)
    return df