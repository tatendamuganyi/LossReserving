# coding=utf-8

__author__ = 'Tatenda Muganyi'

__version__ = '0.1'

import sqlite3
import pandas as pd
import numpy as np
from datetime import datetime
from numpy import inf
import random

#Basic Chain Ladder

class BCLMethod:
	#deriving link ratios
	def Link_Ratios(self, df):
		df = pd.DataFrame(df)
		link_ratios = []
		for row in range(0,((df.shape[0]))):
			link_ratio = []
			for column in range(0,((df.shape[1])-1)):
				if (column+row < (df.shape[0]-1)):
					link_ratio.append(df.iloc[row,(column+1)]/df.iloc[row,column])
				else:
					link_ratio.append(0)
			link_ratios.append(link_ratio)
		links=pd.DataFrame(link_ratios,index = df.index)
		links[links == inf] = 1
		return links

	#settlement patterns
	def Development_Pattern(self, df):
		df = pd.DataFrame(df)
		pattern = []
		for row in range(0,df.shape[0]):
			developments = []
			for column in range (0,df.shape[1]):
				if (column+row < df.shape[0]-1):
					developments.append(1/(df.iloc[row,column:(df.shape[0]-row-1)].product()))
				else:
					developments.append(1)
			pattern.append(developments)
		dev_pattern = pd.DataFrame(pattern,index = df.index)
		return dev_pattern.T

	#testing for entries with zero in link ratios derived
	def Boot_Cum(self, df_cum, df_link):
		boot_link = pd.DataFrame(df_link)
		df_cum = pd.DataFrame(df_cum)
		boot_link[boot_link != 0] = 1
		boot_link.insert(loc = int(boot_link.shape[1]), column = str(boot_link.shape[1]),value = 1)
		boot_link.columns = list(range(0,(boot_link.shape[1])))
		boot_cum = pd.DataFrame(boot_link.values*df_cum.values)
		return pd.DataFrame(boot_cum)

	#deriving development factors
	def Development_Factors(self, df_cum, df_link):
		df_cum = pd.DataFrame(df_cum)
		df_link = pd.DataFrame(df_link)
		dev_factors = []

		for column in range(0,(df_cum.shape[1]-1)):
			level = (df_cum.shape[0]-column-1)
			cumulatives = sum(df_cum.iloc[:level,(column)])
			if (cumulatives ==0):
				temp_dev = 1 #sum(df_cum.iloc[:level,(column)].values*df_link.iloc[:level,(column-1)].values)
			else:
				temp_dev = sum(df_cum.iloc[:level,(column)].values*df_link.iloc[:level,(column)].values)/cumulatives
			dev_factors.append(temp_dev)
		dev_factors = pd.DataFrame(dev_factors).T

		forecast_dev_factors = []	
		for row in range(0,df_cum.shape[0]):
			dev_rows = []
			diagonal_pos = int(df_cum.shape[0]-row-1)
			for col in range(0,df_cum.shape[1]):
				if (row+col+1 > df_cum.shape[0]):
					temp_df = dev_factors.iloc[:,diagonal_pos:(col)].product(axis = 1)
					dev_rows.append(float(temp_df))
				else:
					dev_rows.append(1)
			forecast_dev_factors.append(dev_rows)
		forecast_dev_factors = pd.DataFrame(forecast_dev_factors)
		return forecast_dev_factors

	def Forecasted_Triangle(self, cum_dev_factors, cum_triangle):
		cum_triangle = pd.DataFrame(cum_triangle)
		triangle = []
		for row in range(0,(cum_triangle.shape[0])):
			triangle_rows = []
			for column in range(0,(cum_triangle.shape[1])):
				if (column+row+1 > (cum_triangle.shape[0])):
					triangle_rows.append(
						cum_triangle.iloc[row,column]*float(cum_dev_factors.iloc[row,column])
						)
				else:
					triangle_rows.append(cum_triangle.iloc[row,column])
			triangle.append(triangle_rows)
		out_triangle = pd.DataFrame(triangle,index = cum_triangle.index)
		return out_triangle

BCLMethod = BCLMethod()