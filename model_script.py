# -*- coding: utf-8 -*-
"""
Created on Sun Feb 10 13:38:29 2019

@author: llavi
"""

from __future__ import division
import os
import glob
from os.path import join
import pandas as pd
import numpy as np
import math
import time
from pyomo.environ import *

'''
this is the formulation of the Pyomo optimization model
'''

start_time = time.time()
cwd = os.getcwd()

dispatch_model = AbstractModel()

###########################
# ######## SETS ######### #
###########################

#time
dispatch_model.TIMEPOINTS = Set(domain=PositiveIntegers, ordered=True)

#generators
dispatch_model.GENERATORS = Set(ordered=True)

#operating reserve segments
dispatch_model.SEGMENTS = Set(domain=PositiveIntegers, ordered=True)

#zones
dispatch_model.ZONES = Set(doc="study zones", ordered=True)

#lines
dispatch_model.TRANSMISSION_LINE = Set(doc="tx lines", ordered=True)

#generator segments
dispatch_model.GENERATORSEGMENTS = Set(ordered=True)

#generator types? fuel types?

###########################
# ####### PARAMS ######## #
###########################

#time and zone-dependent params
dispatch_model.grossload = Param(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, within=NonNegativeReals)
dispatch_model.windcf = Param(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, within=NonNegativeReals)
dispatch_model.solarcf = Param(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, within=NonNegativeReals)
dispatch_model.maxhydro = Param(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, within=NonNegativeReals)
dispatch_model.minhydro = Param(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, within=NonNegativeReals)
dispatch_model.ramphydro = Param(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, within=NonNegativeReals)

#timepoint-dependent params
dispatch_model.temperature = Param(dispatch_model.TIMEPOINTS, within=Reals)
dispatch_model.primaryreservescalar = Param(dispatch_model.TIMEPOINTS, within=NonNegativeReals)
dispatch_model.secondaryreservescalar = Param(dispatch_model.TIMEPOINTS, within=NonNegativeReals)
dispatch_model.reservescalarratio = Param(dispatch_model.TIMEPOINTS, within=NonNegativeReals)

#zone-dependent params
dispatch_model.windcap = Param(dispatch_model.ZONES, within=NonNegativeReals)
dispatch_model.solarcap = Param(dispatch_model.ZONES, within=NonNegativeReals)
dispatch_model.totalhydro = Param(dispatch_model.ZONES, within=NonNegativeReals)
dispatch_model.insubzone = Param(dispatch_model.ZONES, within=Binary)

#generator-dependent params
dispatch_model.fuelcost = Param(dispatch_model.GENERATORS, within=NonNegativeReals)
dispatch_model.pmin = Param(dispatch_model.GENERATORS, within=NonNegativeReals)
dispatch_model.startcost = Param(dispatch_model.GENERATORS, within=NonNegativeReals)
dispatch_model.canspin = Param(dispatch_model.GENERATORS, within=Binary)
dispatch_model.cannonspin = Param(dispatch_model.GENERATORS, within=Binary)
dispatch_model.minup = Param(dispatch_model.GENERATORS, within=NonNegativeIntegers)
dispatch_model.mindown = Param(dispatch_model.GENERATORS, within=NonNegativeIntegers)
dispatch_model.noloadcost = Param(dispatch_model.GENERATORS, within=NonNegativeReals)

#generator-dependent initialization parameters
dispatch_model.commitinit = Param(dispatch_model.GENERATORS, within=Binary)
dispatch_model.upinit = Param(dispatch_model.GENERATORS, within=NonNegativeIntegers)
dispatch_model.downinit = Param(dispatch_model.GENERATORS, within=NonNegativeIntegers)

#time and zone-dependent params
dispatch_model.scheduledavailable = Param(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, within=PercentFraction)

#generator and zone-dependent params
dispatch_model.capacity = Param(dispatch_model.GENERATORS, dispatch_model.ZONES, within=NonNegativeReals)
dispatch_model.ramp = Param(dispatch_model.GENERATORS, dispatch_model.ZONES, within=NonNegativeReals) #rate is assumed to be equal up and down
dispatch_model.rampstartuplimit = Param(dispatch_model.GENERATORS, dispatch_model.ZONES, within=NonNegativeReals) #special component of the ramping constraint on the startup hour
dispatch_model.rampshutdownlimit = Param(dispatch_model.GENERATORS, dispatch_model.ZONES, within=NonNegativeReals) #special component of the ramping constraint on the shutdown hour ---- NEW
dispatch_model.insubzonegen = Param(dispatch_model.GENERATORS, dispatch_model.ZONES, within=Binary)

#reserve segment-dependent params
### THESE ARE NO LONGER USED IN THE MODEL AS OF 4.14.19 ###
dispatch_model.synchsegmentMW = Param(dispatch_model.SEGMENTS, within=NonNegativeReals)
dispatch_model.nonsynchsegmentMW = Param(dispatch_model.SEGMENTS, within=NonNegativeReals)
dispatch_model.secondarysegmentMW = Param(dispatch_model.SEGMENTS, within=NonNegativeReals)
dispatch_model.segmentprice = Param(dispatch_model.SEGMENTS, within=NonNegativeReals)

#reserve segment *and* timepoint dependent params
dispatch_model.SynchMW = Param(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS, within=NonNegativeReals)
dispatch_model.NonSynchMW = Param(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS, within=NonNegativeReals)
dispatch_model.SecondaryMW = Param(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS, within=NonNegativeReals)
dispatch_model.price = Param(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS, within=NonNegativeReals)

#transmission line only depedent params
dispatch_model.old = Param(dispatch_model.TRANSMISSION_LINE, within=PercentFraction)

#time and transmission line-dependent params
dispatch_model.transmission_from = Param(dispatch_model.TIMEPOINTS, dispatch_model.TRANSMISSION_LINE, within=dispatch_model.ZONES)
dispatch_model.transmission_to = Param(dispatch_model.TIMEPOINTS, dispatch_model.TRANSMISSION_LINE, within=dispatch_model.ZONES)
dispatch_model.transmission_from_capacity = Param(dispatch_model.TIMEPOINTS, dispatch_model.TRANSMISSION_LINE, within=Reals)
dispatch_model.transmission_to_capacity = Param(dispatch_model.TIMEPOINTS, dispatch_model.TRANSMISSION_LINE, within=Reals)
dispatch_model.hurdle_rate = Param(dispatch_model.TIMEPOINTS, dispatch_model.TRANSMISSION_LINE, within=NonNegativeReals)

#generator segment params
dispatch_model.generatorsegmentlength = Param(dispatch_model.GENERATORSEGMENTS, within=PercentFraction)

#generator and generator segment-dependent params
dispatch_model.generatormarginalcost = Param(dispatch_model.GENERATORS, dispatch_model.GENERATORSEGMENTS, within=NonNegativeReals)

###########################
# ###### SUBSETS ####### #
###########################

#subsets hydro resources so can determine different operational characteristics for them
def hydro_resources_init(model):
    hydro_resources = list()
    for g in model.GENERATORS:
        if model.fuelcost[g]==0: #this should have different index but OK for now
            hydro_resources.append(g)
    return hydro_resources

dispatch_model.HYDRO_GENERATORS = Set(within=dispatch_model.GENERATORS, initialize=hydro_resources_init)

#this isn't currently used because ability to quickstart is implemented as a boolean parameter, but one coudl subset the quick start generators and give them certain characteristics
def quick_start_resources_init(model):
    quick_start_resources = list()
    for g in model.GENERATORS:
        if model.cannonspin[g]==1:
            quick_start_resources.append(g)
    return quick_start_resources

dispatch_model.QUICK_START_GENERATORS = Set(within=dispatch_model.GENERATORS, initialize=quick_start_resources_init)

#subsets generators in zones that are part of a SINGLE reserve subzone for which want to check reserve sufficiency
def reserve_subzone_gens_init(model):
    sub_zone_gens = list()
    for z in model.ZONES:
        for g in model.GENERATORS:
            if model.insubzonegen[g,z]==1:
                sub_zone_gens.append(g)
    return sub_zone_gens

dispatch_model.SUB_ZONE_GENERATORS = Set(within=dispatch_model.GENERATORS, initialize=reserve_subzone_gens_init)

#reserve subzones
def reserve_subzone_init(model):
    sub_zones = list()
    for z in model.ZONES:
        if model.insubzone[z]==1:
            sub_zones.append(z)
    return sub_zones

dispatch_model.SUB_ZONES = Set(within=dispatch_model.ZONES, initialize=reserve_subzone_init)

#main or external to sub-zone zones
def not_reserve_subzone_init(model):
    not_sub_zones = list()
    for z in model.ZONES:
        if model.insubzone[z]==0:
            not_sub_zones.append(z)
    return not_sub_zones

dispatch_model.NOT_SUB_ZONES = Set(within=dispatch_model.ZONES, initialize=not_reserve_subzone_init)

###########################
# ######## VARS ######### #
###########################

dispatch_model.dispatch = Var(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, dispatch_model.ZONES,
                              within = NonNegativeReals, initialize=0)

dispatch_model.segmentdispatch = Var(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS,
                                     dispatch_model.ZONES, dispatch_model.GENERATORSEGMENTS,
                                     within=NonNegativeReals, initialize=0)

dispatch_model.synchreserves = Var(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS,
                                  within = NonNegativeReals, initialize=0)

dispatch_model.nonsynchreserves = Var(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS,
                                  within = NonNegativeReals, initialize=0)

dispatch_model.secondaryreserves = Var(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS,
                                  within = NonNegativeReals, initialize=0)

dispatch_model.segmentreserves =  Var(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS,
                                      within = NonNegativeReals, initialize=0)

dispatch_model.nonsynchsegmentreserves = Var(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS,
                                      within = NonNegativeReals, initialize=0)

dispatch_model.secondarysegmentreserves = Var(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS,
                                      within = NonNegativeReals, initialize=0)

dispatch_model.subzonesegmentreserves = Var(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS,
                                      within = NonNegativeReals, initialize=0)

dispatch_model.txsubzonecontribution = Var(dispatch_model.TIMEPOINTS, within = NonNegativeReals, initialize=0)

dispatch_model.windgen = Var(dispatch_model.TIMEPOINTS, dispatch_model.ZONES,
                              within = NonNegativeReals, initialize=0)

dispatch_model.solargen = Var(dispatch_model.TIMEPOINTS, dispatch_model.ZONES,
                              within = NonNegativeReals, initialize=0)

dispatch_model.curtailment = Var(dispatch_model.TIMEPOINTS,  dispatch_model.ZONES,
                                 within = NonNegativeReals, initialize=0)

dispatch_model.transmit_power_MW = Var(dispatch_model.TIMEPOINTS, dispatch_model.TRANSMISSION_LINE,
                                       within = Reals, initialize=0)

#the following vars will make problem integer when implemented
dispatch_model.commitment = Var(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS,
                                within=Binary, initialize=0)

dispatch_model.startup = Var(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS,
                               within=Binary, initialize=0)

dispatch_model.shutdown = Var(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS,
                               within=Binary, initialize=0)

###########################
# ##### CONSTRAINTS ##### #
###########################

## RENEWABLES ##

#wind output, should allow for curtailment but has $0 cost for now
def WindRule(model, t, z):
    return (model.windcap[z]*model.windcf[t,z] >= model.windgen[t,z])
dispatch_model.WindMaxConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, rule=WindRule)

#solar output, should allow for curtailment but has $0 cost for now
def SolarRule(model, t, z):
    return (model.solarcap[z]*model.solarcf[t,z] >= model.solargen[t,z])
dispatch_model.SolarMaxConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, rule=SolarRule)

#curtailment probably won't get used, but let's put it in for now
def CurtailmentRule(model, t, z):
    return (model.curtailment[t,z] == (model.windcap[z]*model.windcf[t,z]-model.windgen[t,z]) + (model.solarcap[z]*model.solarcf[t,z]-model.solargen[t,z]))
dispatch_model.CurtailmentConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, rule=CurtailmentRule)

## HYDRO ##

def TotalHydroRule(model, z):
    return (sum(sum(model.dispatch[t,h,z] for h in model.HYDRO_GENERATORS) for t in model.TIMEPOINTS) + sum(sum(model.synchreserves[t,h] for h in model.HYDRO_GENERATORS) for t in model.TIMEPOINTS) == model.totalhydro[z])
dispatch_model.TotalHydroConstraint = Constraint(dispatch_model.ZONES, rule=TotalHydroRule)

def HydroMaxRule(model, t, z):
    return model.maxhydro[t,z] >= sum(model.dispatch[t,h,z] for h in model.HYDRO_GENERATORS)
dispatch_model.MaxHydroConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, rule=HydroMaxRule)

def HydroMinRule(model, t, z):
    return sum(model.dispatch[t,h,z] for h in model.HYDRO_GENERATORS) >= model.minhydro[t,z]
dispatch_model.MinHydroConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, rule=HydroMinRule)

def HydroRampUpRule(model, t, z):
    if t==1:
        return Constraint.Skip
    else:
        return (sum(model.dispatch[t-1,h,z] for h in model.HYDRO_GENERATORS) + model.ramphydro[t,z]  >= sum(model.dispatch[t,h,z] for h in model.HYDRO_GENERATORS))
dispatch_model.HydroRampUpConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, rule=HydroRampUpRule)

def HydroRampDownRule(model, t, z):
    if t==1:
        return Constraint.Skip
    else:
        return(sum(model.dispatch[t,h,z] for h in model.HYDRO_GENERATORS) >= sum(model.dispatch[t-1,h,z] for h in model.HYDRO_GENERATORS) - model.ramphydro[t,z])
dispatch_model.HydroRampDownConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, rule=HydroRampDownRule)


## TRANSMISSION LINES ##

#flow rules, simple for now but could eventually include line ramp limits or etc.
def TxFromRule(model, t, line):
    return (model.transmit_power_MW[t,line] >= model.transmission_from_capacity[t, line])
dispatch_model.TxFromConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.TRANSMISSION_LINE, rule=TxFromRule)

def TxToRule(model, t, line):
    return (model.transmission_to_capacity[t, line] >= model.transmit_power_MW[t,line])
dispatch_model.TxToConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.TRANSMISSION_LINE, rule=TxToRule)


## LOAD BALANCE ##

#load/gen balance
def LoadRule(model, t, z):
    
    #implement total tx flow
    imports_exports = 0
    for line in model.TRANSMISSION_LINE:
        if model.transmission_to[t, line] == z or model.transmission_from[t, line] == z:
            if model.transmission_to[t, line] == z:
                imports_exports += model.transmit_power_MW[t, line]
            elif model.transmission_from[t, line] == z:
                imports_exports -= model.transmit_power_MW[t, line]
            #add additional note to dec import/exports by line losses
            #no, this will just be done as a hurdle rate 
    #full constraint, with tx flow now
    return (sum(model.dispatch[t,g,z] for g in model.GENERATORS) + model.windgen[t,z] +\
            model.solargen[t,z] + imports_exports == model.grossload[t,z])
dispatch_model.LoadConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.ZONES, rule=LoadRule)

## GENERATORS ###

#gen capacity with scheduled outage factored in
def CapacityMaxRule(model, t, g, z):
    return (model.capacity[g,z]*model.commitment[t,g]*model.scheduledavailable[t,g] >= model.dispatch[t,g,z])
dispatch_model.CapacityMaxConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, dispatch_model.ZONES, rule=CapacityMaxRule)

#pmin
def PminRule(model,t,g,z):
    #print(t,g,z)
    #if model.dispatch[t,g,z] == 0: #allows for no load commitment of spinning reserves (currently bound @ ramp rate)
    #    return Constraint.Skip
    #else: #otherwise pmin will bind
        #print('we in!')
        #print(t,g,z)
        #if g == "Notch Cliff 3":
        #    print(t,z)
        #    print (model.capacity[g,z], model.scheduledavailable[t,g], model.pmin[g])
    return (model.dispatch[t,g,z] >= model.capacity[g,z]*model.commitment[t,g]*model.scheduledavailable[t,g]*model.pmin[g])
dispatch_model.PminConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, dispatch_model.ZONES, rule=PminRule)

### GENERATOR SEGMENT DISPATCH ###

#max on segment
def GeneratorSegmentDispatchMax(model, t, g, z, gs):
    return model.generatorsegmentlength[gs]*model.capacity[g,z]*model.commitment[t,g]*model.scheduledavailable[t,g] >= model.segmentdispatch[t,g,z,gs]
dispatch_model.GeneratorSegmentMaxConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS,
                                                       dispatch_model.ZONES, dispatch_model.GENERATORSEGMENTS
                                                       ,rule=GeneratorSegmentDispatchMax)

#sum of generator segment dispatch equivalent to total generator dispatch
#we are implicitly assuming the first segment will be the most efficient, but since heat rates are 
#modeled as quadratic for all generators, this is true by definition
def GeneratorSegmentDispatchSegmentSummation(model,t,g,z):
    return model.dispatch[t,g,z] == sum(model.segmentdispatch[t,g,z,gs] for gs in model.GENERATORSEGMENTS)
dispatch_model.GeneratorSegmentSumConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS,
                                                       dispatch_model.ZONES,rule=GeneratorSegmentDispatchSegmentSummation)

## GENERATOR RAMP ##

def GeneratorRampUpRule(model,t,g,z):
    if t==1:
        return Constraint.Skip
    else:
        return (model.dispatch[t-1,g,z] + model.ramp[g,z]*model.commitment[t-1,g] + model.startup[t,g]*model.rampstartuplimit[g,z]  >= model.dispatch[t,g,z])
dispatch_model.GeneratorRampUpConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, dispatch_model.ZONES, rule=GeneratorRampUpRule)

def GeneratorRampDownRule(model,t,g,z): ### NEW
    if t==1: ##guessing it's worthwhile to have this to guard against weirdness, even though a generator will never "get shutdown" in hour 1... 
        return Constraint.Skip 
    else:
        return (model.dispatch[t,g,z] >= model.dispatch[t-1,g,z] - model.ramp[g,z]*model.commitment[t-1,g] + model.shutdown[t,g]*model.rampshutdownlimit[g,z])
dispatch_model.GeneratorRampDownConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, dispatch_model.ZONES, rule=GeneratorRampDownRule)

## GENERATOR STARTUP/SHUTDOWN ##

#startups
def StartUpRule(model,t,g):
    if t==1:
        return 1-model.commitinit[g] >= model.startup[t,g]
    else:
        return (1-model.commitment[t-1,g] >= model.startup[t,g])
dispatch_model.StartUpConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, rule=StartUpRule)

#shutdowns
def ShutDownRule(model,t,g):
    if t==1:
        return model.commitinit[g] >= model.shutdown[t,g]
    else:
        return (model.commitment[t-1,g] >= model.shutdown[t,g])
dispatch_model.ShutDownConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, rule=ShutDownRule)

#assign shuts and starts
def AssignStartShutRule(model,t,g):
    if t==1: #binds commitment in first hour based on initialization commitment from input (could be last timeperiod of previous run)
        return model.commitment[t,g] - model.commitinit[g] == model.startup[t,g] - model.shutdown[t,g]
    else: #general rule
        return (model.commitment[t,g] - model.commitment[t-1,g] == model.startup[t,g] - model.shutdown[t,g])
dispatch_model.AssignStartShutConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, rule=AssignStartShutRule)

#force de-comitted generator if unit unavailable due to scheduled outage
def ScheduledAvailableRule(model,t,g):
    if model.scheduledavailable[t,g]==0:
        return (model.scheduledavailable[t,g] == model.commitment[t,g])
    else:
        return Constraint.Skip
dispatch_model.ScheduledAvailableConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, rule=ScheduledAvailableRule)

## GENERATOR MIN UP AND DOWN ##

#min uptime constraint
def MinUpRule(model,t,g):
    recent_start_bool = float() #initialize our tracker; boolean because you'll just never see multiple starts
    
    #allow minup to be overruled if generator is scheduled not available
    if model.scheduledavailable[t,g] == 0:
        return Constraint.Skip

    if t - model.minup[g] <1: #i.e. in the lookback period to the initialization condition
        for tp in range(1,t+1):
            if model.scheduledavailable[tp,g]==0: #if the generator previously could have scheduled shutdown, nullify this constraint
                return Constraint.Skip
        if model.minup[g] >= t+model.upinit[g] and model.commitinit[g]==1: #if generator started online, and hasn't been up long enough
            return model.commitment[t,g] >= model.commitinit[g]
        else:
            return Constraint.Skip

    else: #define subperiod
        for tp in range(1, model.minup[g]+1): #b/c exclusive upper bound!
            recent_start_bool += model.startup[t-tp,g]
        return model.commitment[t,g] >= recent_start_bool
dispatch_model.MinUpConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, rule=MinUpRule)

#min downtime constraint
def MinDownRule(model,t,g):
    recent_shut_bool = float()

    if t - model.mindown[g] <1: 
        if model.mindown[g] >= t+model.downinit[g] and model.commitinit[g]==0:
            return model.commitinit[g] >= model.commitment[t,g]
        else:
            return Constraint.Skip
    else:
        for tp in range(1, model.mindown[g]+1):
            recent_shut_bool += model.shutdown[t-tp,g]
        return (1-recent_shut_bool) >= model.commitment[t,g]
dispatch_model.MinDownConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, rule=MinDownRule)

## HOLD SUFFICIENT RESERVES ##

## PRIMARY SYNCHRONIZED RESERVES - HELD BY INDIVIDUAL GENERATOR ##

#caps the amount of synchronized reserve a generator can provide as delta between its max and current power output
#and provides only if generator is eligible to provide spinning reserve
def GenSpinUpReserveRule(model,t,g):
    return sum((model.capacity[g,z]*model.commitment[t,g]*model.scheduledavailable[t,g] - model.dispatch[t,g,z]) for z in model.ZONES)*model.canspin[g] >= model.synchreserves[t,g]
dispatch_model.GenSpinUpReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, rule=GenSpinUpReserveRule)

#caps amount of synchronized reserve a generator can provide as delta between its current output and max ramping capability 
#implemented to be within the timepoint (hourly for right now), but in practice system operators often use shorter timescales
#for example, PJM uses 10 minute (primary) and 30 minute (secondary) reserve horizon
def GenSpinUpReserveRampRule(model,t,g,z):
    return model.ramp[g,z]*model.primaryreservescalar[t] >= model.synchreserves[t,g]
dispatch_model.GenSpinUpReserveRampConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, dispatch_model.ZONES, rule=GenSpinUpReserveRampRule)

## PRIMARY NON-SYNCHRONIZED RESERVES - HELD BY INDIVIDUAL GENERATOR ##

#caps generator level non-synchronized reserve as delta between current output and pmin
#can provide only if generator is offline
#also can provide only if generator is eligible to provide non-synch reserve (i.e., is quick-start capacity)
#as written this assumes a generator can reach its pmin within the primary time window (currently, 10 minutes for PJM)
def GenNonSynchReserveRule(model,t,g):
    return sum((model.capacity[g,z]*model.pmin[g]*(1-model.commitment[t,g])*model.scheduledavailable[t,g]) for z in model.ZONES)*model.cannonspin[g] >= model.nonsynchreserves[t,g]
dispatch_model.GenNonSynchReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, rule=GenNonSynchReserveRule)


## SECONDARY RESERVES - HELD BY INDIVIDUAL GENERATOR ##   

#secondary reserves held by individual generator must be less than held primary reserves times temporal multiplier
#temporal multiplier is ratio of how much longer time window is for secondary vs. primary reserves (30 mins v. 10 mins in PJM)

def SecondaryReserveRule(model,t,g):
    return model.reservescalarratio[t]*(model.synchreserves[t,g]+model.nonsynchreserves[t,g]) >= model.secondaryreserves[t,g]
dispatch_model.SecondaryReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, rule=SecondaryReserveRule)

#secondary reserves must still, however, be less than pmax - generator set point 

def SecondaryReservePmaxRule(model,t,g):
    return sum((model.capacity[g,z] - model.dispatch[t,g,z]) for z in model.ZONES) >= model.secondaryreserves[t,g]
dispatch_model.SecondaryReservePmaxConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.GENERATORS, rule=SecondaryReservePmaxRule)

## TOTAL PRIMARY SYNCHRONIZED RESERVES HELD ##

#these reserve constraints record the level of held reserves on each segment of the ORDC
def TotalSynchReserveRule(model,t):
    return (sum(model.synchreserves[t,g] for g in model.GENERATORS) >= sum(model.segmentreserves[t,s] for s in model.SEGMENTS))
dispatch_model.TotalSynchReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, rule=TotalSynchReserveRule)

#defunct hard constraint on reserves
#def SpinUpReserveHardRule(model,t):
#    return sum(model.spinreserves[t,g] for g in model.GENERATORS) >= 9000
#dispatch_model.SpinUpReserveHardConstraint = Constraint(dispatch_model.TIMEPOINTS, rule=SpinUpReserveHardRule)
#binds the ratio of non-spin to spin reserves
#originally set to be 50% like in https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=6609097
#def SpintoNonSpinRatioRule(model,t):
#    return (sum(model.spinreserves[t,g] for g in model.GENERATORS) >= 0.5*sum(model.segmentreserves[t,s] for s in model.SEGMENTS))
#dispatch_model.SpintoNonSpinRatioConstraint = Constraint(dispatch_model.TIMEPOINTS, rule=SpintoNonSpinRatioRule)

def SegmentReserveRule(model,t,s):
    #return model.segmentreserves[t,s] >= model.SynchMW[t,s] #implements as hard constraint
    return model.SynchMW[t,s] >= model.segmentreserves[t,s] #penalty facor constraint must be linked with objective function
dispatch_model.SegmentReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS, rule=SegmentReserveRule)

## TOTAL PRIMARY NON-SYNCH RESERVES HELD ##

def TotalNonSynchReserveRule(model,t):
    return (sum((model.synchreserves[t,g] + model.nonsynchreserves[t,g]) for g in model.GENERATORS) >= sum(model.nonsynchsegmentreserves[t,s] for s in model.SEGMENTS))
dispatch_model.TotalNonSynchReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, rule=TotalNonSynchReserveRule)

def NonSynchSegmentReserveRule(model,t,s):
    return model.NonSynchMW[t,s] >= model.nonsynchsegmentreserves[t,s]
dispatch_model.NonSynchSegmentReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS, rule=NonSynchSegmentReserveRule)

## TOTAL SECONDARY RESERVES HELD ##

def TotalSecondaryReserveRule(model,t):
    return (sum(model.secondaryreserves[t,g] for g in model.GENERATORS) >= sum(model.secondarysegmentreserves[t,s] for s in model.SEGMENTS))
dispatch_model.TotalSecondaryReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, rule=TotalSecondaryReserveRule)

def SecondarySegmentReserveRule(model,t,s):
    return model.SecondaryMW[t,s] >= model.secondarysegmentreserves[t,s]
dispatch_model.SecondarySegmentReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS, rule=SecondarySegmentReserveRule)

## HOLDING OF SUB-ZONAL PRIMARY SYNCH RESERVES ##

def TotalSubZonalSynchReserveRule(model,t):
    #find out how much excess capacity is on lines that deliver to constrained zone(s)
    
    tx_line_reserves = 0
    for line in model.TRANSMISSION_LINE:
        for sz in model.SUB_ZONES:
            for nsz in model.NOT_SUB_ZONES:
                if model.transmission_to[t, line] == sz or model.transmission_from[t, line] == sz:
                    if model.transmission_to[t, line] == sz and model.transmission_from[t,line] == nsz:
                        tx_line_reserves += model.transmission_to_capacity[t, line]-model.transmit_power_MW[t, line]
                    elif model.transmission_from[t, line] == sz and model.transmission_to[t,line]==nsz:
                        tx_line_reserves -= model.transmission_from_capacity[t, line]-model.transmit_power_MW[t, line]
    
    
    #return
    #return (sum(model.synchreserves[t,g_sz] for g_sz in model.SUB_ZONE_GENERATORS)+tx_line_reserves >= 500)
    return (sum(model.synchreserves[t,g_sz] for g_sz in model.GENERATORS)+tx_line_reserves >= sum(model.subzonesegmentreserves[t,s] for s in model.SEGMENTS))
dispatch_model.TotalSubZonalSynchReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, rule=TotalSubZonalSynchReserveRule)

def LineSynchReserveContributionRule(model,t):
    
    tx_line_reserves = 0
    for line in model.TRANSMISSION_LINE:
        for sz in model.SUB_ZONES:
            for nsz in model.NOT_SUB_ZONES:
                if model.transmission_to[t, line] == sz or model.transmission_from[t, line] == sz:
                    if model.transmission_to[t, line] == sz and model.transmission_from[t,line] == nsz:
                        tx_line_reserves += model.transmission_to_capacity[t, line]-model.transmit_power_MW[t, line]
                    elif model.transmission_from[t, line] == sz and model.transmission_to[t,line]==nsz:
                        tx_line_reserves -= model.transmission_from_capacity[t, line]-model.transmit_power_MW[t, line]
    return tx_line_reserves == model.txsubzonecontribution[t]

dispatch_model.LineSynchReserveContributionConstraint = Constraint(dispatch_model.TIMEPOINTS, rule=LineSynchReserveContributionRule)
    

def SubZonalSegementReserveRule(model,t,s):
    return model.SynchMW[t,s] >= model.subzonesegmentreserves[t,s]
dispatch_model.SubZonalSynchSegmentReserveConstraint = Constraint(dispatch_model.TIMEPOINTS, dispatch_model.SEGMENTS, rule=SubZonalSegementReserveRule)

###########################
# ###### OBJECTIVE ###### #
###########################

def objective_rule(model): 
    #min dispatch cost for objective
    return sum(sum(sum(sum(model.segmentdispatch[t,g,z,gs] for z in model.ZONES) for t in model.TIMEPOINTS)*model.generatormarginalcost[g,gs] for g in model.GENERATORS) for gs in model.GENERATORSEGMENTS)+\
           sum(sum(model.commitment[t,g] for t in model.TIMEPOINTS)*model.noloadcost[g] for g in model.GENERATORS)+\
           sum(sum(model.startup[t,g] for t in model.TIMEPOINTS)*model.startcost[g] for g in model.GENERATORS)-\
           sum(sum(model.price[t,s]*model.segmentreserves[t,s] for s in model.SEGMENTS) for t in model.TIMEPOINTS)-\
           sum(sum(model.price[t,s]*model.nonsynchsegmentreserves[t,s] for s in model.SEGMENTS) for t in model.TIMEPOINTS)-\
           sum(sum(model.price[t,s]*model.secondarysegmentreserves[t,s] for s in model.SEGMENTS) for t in model.TIMEPOINTS)-\
           sum(sum(model.price[t,s]*model.subzonesegmentreserves[t,s] for s in model.SEGMENTS) for t in model.TIMEPOINTS)+\
           sum(sum(model.hurdle_rate[t, line]*model.transmit_power_MW[t, line] for line in model.TRANSMISSION_LINE) for t in model.TIMEPOINTS)
    #sum(sum(model.price[t,s]*model.segmentreserves[t,s] for s in model.SEGMENTS) for t in model.TIMEPOINTS)-\
    #sum(sum(model.price[t,s]*model.subzonesegmentreserves[t,s] for s in model.SEGMENTS) for t in model.TIMEPOINTS)+\
    #sum(sum(model.price[t,s]*model.segmentreserves[t,s] for s in model.SEGMENTS) for t in model.TIMEPOINTS)-\
    #return(sum(sum(sum(model.dispatch[t,g,z] for z in model.ZONES) for t in model.TIMEPOINTS)*model.fuelcost[g] for g in model.GENERATORS) +\
    #DESCRIPTION OF OBJECTIVE
    #(1) dispatch cost
    #(2) no load cost of committed gen
    #(3) start up costs when generators brought online
    #(4)-(6) utility of reserve demand
    #(7) hurdle rates on use of Tx lines
dispatch_model.TotalCost = Objective(rule=objective_rule, sense=minimize)

end_time = time.time() - start_time
print ("time elapsed during run is " + str(end_time) + " seconds")