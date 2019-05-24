#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 08:46:48 2019

@author: bsergi
"""

#scenario_name = "TEST2" cases are now named below based on their corresponding date
case_folder = "Jan_4_10_2014_withORDC"

# initialization boolean list should match length of dates
make_init_list = [True, False, False, False, False, False, False]
#make_init_list = [True]
create_supp_ordc = True #this chooses whether to create supplemental ORDC, if False there will be no value for reserves in the model
dynamic_ORDC = True

#where to pull hydro data
hydro_sheet = "PJM.hydro.gen.jan.2014"
#hydro_sheet = "PJM.hydro.gen.oct.2017"

#generator segment pieces
n_generator_segments = 4

#(1) specify dates to run in list (note: each day is run separately)
#dates = ['10.19.2017']
#dates = ['10.19.2017', '10.20.2017','10.21.2017', '10.22.2017','10.23.2017', '10.24.2017', '10.25.2017']
dates = ['1.4.2014','1.5.2014','1.6.2014','1.7.2014','1.8.2014','1.9.2014','1.10.2014']
#(2) hydro cf
hydro_cf = 0.3

#(3) VOLL (in US $)
VOLL = 2000 #this is more traditionally $2k in PJM
#used to be 3500

#(4) Lowcut LOLP
lowcutLOLP = 0.00001

#(5) number of segments in ORDC
n_segments = 10

#(6) number of days to run after each start date
days = 1.25 #additional .25 creates a six hour look ahead period

#(7) forecast errors, though not currently active
wfe = 0.01
sfe = 0.01
lfe = 0.01

#(8) use zones?
zones = True #this should really always be true

#(9) contingency reserves shed (US $)
contingency = 850
