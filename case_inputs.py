#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 08:46:48 2019

@author: bsergi
"""

#scenario_name = "TEST2" cases are now named below based on their corresponding date
#case_folder = "Oct_19_25_2017_SimpleORDCMRR"
case_folder = "Oct_19_25_2017_PJMDynORDColdOct28updatedmpinrelaxetc"
#then do one where change SUC of CTs
#case_folder = "TOYCASE"

# initialization boolean list should match length of dates
#make_init_list = [True]
make_init_list = [True, False, False, False, False, False, False]

create_supp_ordc = True #this chooses whether to create ORDC

#if create_supp_ordc=False, chooses whether to use PJM heuristic or NO reserves
PJM_reserve_heuristic = True

#if supp ORDC, chooses whether to dynamically calculate it
dynamic_ORDC = True #setting this to False again needs testing

# MRR specifications
#if create_supp_ordc = True AND dynamic_ORDC = True,
#chooses whether to and how to create MRR in front of ORDC
#should be more similar to PJM method, though arguably less accurate at day-ahead
MRR_method = False
MRRs = {'synch':1400,'nonsynch':2100} #secondary minimum reserve requirements will be based on forecast error inputs 

#where to pull hydro data
#hydro_sheet = "PJM.hydro.gen.jan.2014"
hydro_sheet = "PJM.hydro.gen.oct.2017"

#generator segment pieces
n_generator_segments = 4

#(1) specify dates to run in list (note: each day is run separately)
#dates = ['10.19.2017']
#dates = ['1.9.2014','1.10.2014']
dates = ['10.19.2017', '10.20.2017','10.21.2017', '10.22.2017','10.23.2017', '10.24.2017', '10.25.2017']
#dates = ['1.4.2014','1.5.2014','1.6.2014','1.7.2014','1.8.2014','1.9.2014','1.10.2014']
#(2) hydro cf
hydro_cf = 0.3

#(3) VOLL (in US $)
VOLL = 2000 #they won't call it a VOLL but this is the price cap in PJM, and the proposed reserve penalty factor in their 2019 FERC docket

#(4) Lowcut LOLP
lowcutLOLP = 0.00001

#(5) number of segments in ORDC
n_segments = 10

#(6) number of days to run after each start date
days = 1.25 #additional .25 creates a six hour look ahead period

#(7) forecast errors, only lfe and FOR_fe currently active in PJM cases
#may add wind and solar forecast errors at some point, but note they are historically pretty small for pjm due to low installation
#values for 2014 DASR taken from https://www.pjm.com/-/media/committees-groups/committees/mic/20140625-energy/20140625-item-04-overview-of-dasr.ashx
# for 2018 and 2019, see here https://pjm.com/-/media/committees-groups/committees/mrc/20181025/20181025-item-09-2019-day-ahead-scheduling-reserve-dasr-requirement-presentation.ashx
#note that DASR requirements update at least annually based on 3 year look-back, but seems to be around 5-7% in most years with decline in recent years (FORs are declining)
wfe = 0.01
sfe = 0.01 
lfe = 0.0211 #from document for 2014
FOR_fe = 0.0416 #from document for 2014

#(8) use zones?
zones = True #this should really always be true

#(9) contingency reserves shed (US $), 850 is the old PJM penalty factor (including in 2014; I believe this value dates to about 2012)
contingency = 850

#(10) Hourly scalars
primary_reserve_scalar = 1./6. #primary reserves are 10 minutes, so 1/6 of an hour
secondary_reserve_scalar = 1./2. #secondary reserves are 30 minutes, so 1/2 of an hour 


