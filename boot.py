# coding=utf-8

__author__ = 'Tatenda Muganyi'

__version__ = '0.1'

import sqlite3
import pandas as pd
import numpy as np
from datetime import datetime
from numpy import inf
import random
import os, sys

sys.path.insert(1, os.path.join(sys.path[0], '.'))

from bcl import BCLMethod

#Bootstrap method based on BCL

class BTMethod:
	#First step in bootstrap simulations - randomising the link ratios selected
	def Bootstrap_init(self, df_link, df_cum):
		df_link = pd.DataFrame(df_link)
		df_cum = pd.DataFrame(df_cum)
		df_link_sim = []
		links = []
		cum_tri_boot = []
		cum_dev_factors = []
		proj_triangle = []
		proj_triangle_tail = []
		for row in range(0,df_link.shape[0]):
			df_link_sim_row = []
			for col in range(0,df_link.shape[1]):
				if (col+row+1 < df_cum.shape[1]):
					df_link_sim_row.append(random.randint(0,1)*df_link.iloc[row,col]) #randomising the link ratios selected
				else:
					df_link_sim_row.append("NA")
			df_link_sim.append(df_link_sim_row)

		links = pd.DataFrame(df_link_sim)
		links_temp = pd.DataFrame(df_link_sim)
		cum_tri_boot = BCLMethod.Boot_Cum(df_cum,links)
		cum_dev_factors = BCLMethod.Development_Factors(cum_tri_boot,links_temp)
		proj_triangle = BCLMethod.Forecasted_Triangle(cum_dev_factors,df_cum)
		proj_triangle_tail = proj_triangle.iloc[:,proj_triangle.shape[1]-1]-df_cum.iloc[:,df_cum.shape[1]-1]
		
		return pd.DataFrame(proj_triangle_tail)

	#Bootstrap simulations - using the number of simulations
	def Bootstrap_Simu(self, sim, df_link, df_cum):
		df_simulated = []
		for s in range(0,int(sim)):
			simulated = self.Bootstrap_init(df_link,df_cum) #calling the initial bootstrap function
			simulated.columns = [s]
			df_simulated.append(simulated)
		simulation = pd.concat(df_simulated,axis = 1)
		return simulation #the output needs to be summed across rows

	def Boot_Sum(self, boot_output):
		return pd.DataFrame(boot_output.sum(axis = 0))

BTMethod = BTMethod()