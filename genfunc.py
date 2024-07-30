# coding=utf-8

__author__ = 'Tatenda Muganyi'

__version__ = '0.1'

import sqlite3
import pandas as pd
import numpy as np
from datetime import datetime
from numpy import inf
import random

#General functions
class GNFunctions:
	def Cumulative_Sum(self, df):
		df = pd.DataFrame(df)
		return df.cumsum(axis=1)

	#Grouping function for the claims data
	def TriangleGenerator(self, df, index_var, col_var, values_var, lob):
		df = pd.DataFrame(df)
		triangle = df[df['Class'] == lob].groupby([index_var,col_var],as_index=False)[values_var].sum()
		triangle = pd.DataFrame(triangle)
		triangle_data = triangle.pivot(index = index_var,columns = col_var)[values_var]
		triangle_data.fillna(0,inplace=True)
		return triangle_data

	#Grouping function for premiums data
	def Premiums_Group(self, df, col_var, values_var, lob):
		df = pd.DataFrame(df)
		earned_pre = df[df['Class'] == lob].groupby([col_var],as_index=False)[values_var].sum()
		return earned_pre

	# Class of Business List
	def LOBList(self, data_for_list, level_of_list):
		class_list = list(dict.fromkeys(data_for_list[level_of_list])) #data_for_list[level_of_list].tolist()
		return class_list

	#Updating for Loss Ratio Assumptions
	def UpdateLRAssumpt(self,dfAssumpt,dfAdjUlt,dfBF):
		
		dfAssumpt = pd.DataFrame(dfAssumpt)
		dfAdjUlt = pd.DataFrame(dfAdjUlt)
		dfBF = pd.DataFrame(dfBF)

		selectedUlt = []

		try:
			for row in range(0,dfAssumpt.shape[0]):
				if dfAssumpt.iloc[row,1] =='BCL':
					selected = dfAdjUlt.iloc[row,0]*(1+dfAssumpt.iloc[row,3])
				elif dfAssumpt.iloc[row,1] == 'BF':
					selected = dfBF.iloc[row,0]*(1+dfAssumpt.iloc[row,3])
				selectedUlt.append(selected)
		except:
			for row in range(0,dfAssumpt.shape[0]):
				selected = dfAssumpt.iloc[row,0]
			selectedUlt.Append(selected)

		return pd.DataFrame(selectedUlt)


	#Seasonality Calculation
	def Seasonality(self, lr_df, earned_premiums):
		lr_df = pd.DataFrame(lr_df)
		earned_premiums = pd.DataFrame(earned_premiums)

		period_yr = []
		period_q_m = []
		lr = []
		for row in range(0,lr_df.shape[0]):
			period_str = str(lr_df.iloc[row,0])
			period_yr.append(period_str[:4])
			# extracting the monthly or quarterly periods, i.e. Q1...Q3, month 1...4
			try:
				q_m = int(float(period_str[4:]))
			except ValueError:
				q_m = period_str[4:]

			period_q_m.append(q_m) #appending the periods to be used as array index
			#calculating the seasonality factors with the first seasonality factor set equal to 1
			if (row==0):
				seasonality_factor = 1
			else:
				seasonality_factor = lr_df.iloc[row,1]/lr_df.iloc[(row-1),1]
			lr.append(seasonality_factor)

		period_y = pd.DataFrame(period_yr)
		period_q = pd.DataFrame(period_q_m)
		lr = pd.DataFrame(lr)
		seasonality_df = pd.concat([period_y,period_q, lr],axis = 1, ignore_index = True)
		seasonality_df.columns = ['Year','Period','Loss_Ratio']

		#creating pivot with seasonality factors showing the accident / underwriting years as columns and
		## monthly/quarterly periods as row indexes.
		seasonality_df = seasonality_df.pivot(index = 'Period',columns = 'Year')['Loss_Ratio']

		#deriving the weighted average seasonalities for the monthly/quarterly periods
		##Earned Premiums dataframe transforms
		ep = pd.concat([period_y,period_q, earned_premiums],axis = 1, ignore_index = True)
		ep.columns = ['Year','Period','Earned_Premiums']
		ep_pivot = ep.pivot(index = 'Period',columns = 'Year')['Earned_Premiums']

		ep_seasonality = []
		for row_z in range(0,(seasonality_df.shape[0])):
			ep_dot = []
			for col in range(0,(seasonality_df.shape[1]-1)):
				ep_seasonality_dot = ep_pivot.iloc[row_z,col]*seasonality_df.iloc[row_z,col]
				ep_dot.append(ep_seasonality_dot)
			ep_seas = sum(ep_dot)/sum(ep_pivot.iloc[row_z,:(ep_pivot.shape[1]-1)])
			ep_seasonality.append(ep_seas)
		ep_seasonality = pd.DataFrame(ep_seasonality,index = seasonality_df.index)
		ep_seasonality.columns = ['Weighted Average Seasonality Factors']
		
		seasonality_vF = pd.concat([seasonality_df.iloc[:,0:(seasonality_df.shape[1]-1)],ep_seasonality],axis = 1, ignore_index = False)

		return seasonality_vF

GNFunctions = GNFunctions()