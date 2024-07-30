# coding=utf-8
#//The code below generates prime numbers
#//To generate a set of Prime Numbers
#//-- call the function Prime_Number, specifying the 'Ubound'
#//-- (integer) as the upper bound

# !/usr/local/bin/python
__author__ = 'Tatenda Muganyi'

__version__ = '0.1'

import pandas as pd
import numpy as np


#/* checking if the value is an integer */
def is_int(val):
	if type(val)==int:
		return True
	else:
		if val.is_integer():
			return True
		else:
			return False

#/checking if the value is equal to 1
def isNotOne(val):
	if val!=1:
		return True
	else:
		return False

#generating a set of prime numbers
def Prime_Numbers(Ubound):
	prime_set = []
	count_prime = 1

	prime_set.append(1)

	while (count_prime <= Ubound):
		bool_checks = []
		
		for i in range(0,len(prime_set)):
			
			if is_int(count_prime/prime_set[i]) and isNotOne(prime_set[i]):#and count_prime != prime_set[i]:
				bool_c = 'Factor'
			else:
				bool_c = 'Not Factor'
			bool_checks.append(bool_c)

		if 'Factor'  not in bool_checks and count_prime != 1:
			prime_set.append(count_prime)
		
		count_prime = count_prime + 1

	return prime_set#list(dict.fromkeys(prime_set))



