# -*- coding: utf-8 -*-
"""
Created on Sun Feb 10 13:41:25 2019

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
from pyomo.opt import SolverFactory

import model_script

start_time = time.time()
cwd = os.getcwd()

def scenario_inputs(inputs_directory):
    data = DataPortal()
    
#    data.load(filename=os.path.join(inputs_directory, "timepoints.csv"),
#              index=model_script.dispatch_model.TIMEPOINTS,
#              param=(model_script.dispatch_model.grossload,
#                     model_script.dispatch_model.windcap,
#                     model_script.dispatch_model.windcf,
#                     model_script.dispatch_model.solarcap,
#                     model_script.dispatch_model.solarcf)
#              )
    
    data.load(filename=os.path.join(inputs_directory, "PJM_generators.csv"),
              index=model_script.dispatch_model.GENERATORS,
              param=(model_script.dispatch_model.fuelcost,
                     model_script.dispatch_model.pmin,
                     model_script.dispatch_model.startcost,
                     model_script.dispatch_model.canspin,
                     model_script.dispatch_model.cannonspin,
                     model_script.dispatch_model.minup,
                     model_script.dispatch_model.mindown,
                     model_script.dispatch_model.noloadcost)
              )
              
    data.load(filename=os.path.join(inputs_directory, "initialize_generators.csv"),
              param=(model_script.dispatch_model.commitinit,
                     model_script.dispatch_model.upinit,
                     model_script.dispatch_model.downinit)
              )

    data.load(filename=os.path.join(inputs_directory, "PJM_generators_scheduled_outage.csv"),
              param=(model_script.dispatch_model.scheduledavailable)
              )
    
    data.load(filename=os.path.join(inputs_directory, "PJM_generators_zone.csv"),
              param=(model_script.dispatch_model.capacity,
                     model_script.dispatch_model.ramp,
                     model_script.dispatch_model.rampstartuplimit,
                     model_script.dispatch_model.rampshutdownlimit,
                     model_script.dispatch_model.insubzonegen)
              )

    #this is effectively defunct and used only for loading the index
    data.load(filename=os.path.join(inputs_directory, "operating_reserve_segments.csv"),
              index=model_script.dispatch_model.SEGMENTS,
              param=(model_script.dispatch_model.synchsegmentMW,
                     model_script.dispatch_model.nonsynchsegmentMW,
                     model_script.dispatch_model.secondarysegmentMW,
                     model_script.dispatch_model.segmentprice)
              )          
              
    data.load(filename=os.path.join(inputs_directory, "timepoints_index.csv"),
              index=model_script.dispatch_model.TIMEPOINTS,
              param=(model_script.dispatch_model.temperature,
                     model_script.dispatch_model.primaryreservescalar,
                     model_script.dispatch_model.secondaryreservescalar,
                     model_script.dispatch_model.reservescalarratio)
              )
    
    data.load(filename=os.path.join(inputs_directory, "full_ordc.csv"),
              param=(model_script.dispatch_model.SynchMW,
                     model_script.dispatch_model.NonSynchMW,
                     model_script.dispatch_model.SecondaryMW,
                     model_script.dispatch_model.price)
              )    

    data.load(filename=os.path.join(inputs_directory, "zones.csv"),
              index=model_script.dispatch_model.ZONES,
              param=(model_script.dispatch_model.windcap,
                     model_script.dispatch_model.solarcap,
                     model_script.dispatch_model.totalhydro,
                     model_script.dispatch_model.insubzone)
              )          

    data.load(filename=os.path.join(inputs_directory, "timepoints_zonal.csv"),
              param=(model_script.dispatch_model.grossload,
                     model_script.dispatch_model.windcf,
                     model_script.dispatch_model.solarcf,
                     model_script.dispatch_model.maxhydro,
                     model_script.dispatch_model.minhydro,
                     model_script.dispatch_model.ramphydro)
              ) 

    data.load(filename=os.path.join(inputs_directory, "transmission_lines.csv"),
              index=model_script.dispatch_model.TRANSMISSION_LINE,
              param=(model_script.dispatch_model.old)
              )
    
    data.load(filename=os.path.join(inputs_directory, "transmission_lines_hourly.csv"),
              param=(model_script.dispatch_model.transmission_from,
                     model_script.dispatch_model.transmission_to,
                     model_script.dispatch_model.transmission_from_capacity,
                     model_script.dispatch_model.transmission_to_capacity,
                     model_script.dispatch_model.hurdle_rate)
              )
    
    data.load(filename=os.path.join(inputs_directory,"generator_segments.csv"),
              index=model_script.dispatch_model.GENERATORSEGMENTS,
              param=(model_script.dispatch_model.generatorsegmentlength)
              )
    
    data.load(filename=os.path.join(inputs_directory,"generator_segment_marginalcost.csv"),
              param=(model_script.dispatch_model.generatormarginalcost)
              )
              
    return data

end_time = time.time() - start_time
print ("time elapsed during run is " + str(end_time) + " seconds")