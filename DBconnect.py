# coding=utf-8

__author__ = 'Tatenda Muganyi'

__version__ = '0.1'

import sqlite3
import pandas as pd
import numpy as np
from datetime import datetime
from numpy import inf
import random
#import os

#directory = os.getcwd()

table_name = "Claims_v2"
conn = sqlite3.connect(r"\\retiasstorage\devops\LossReserving\db\InsdB.db")
c = conn.cursor()

# Contents of all columns from Claims Data
c.execute('SELECT * FROM {tn}'.\
        format(tn=table_name))
pd.options.display.float_format = '{:,.0f}'.format
all_rows = pd.DataFrame (c.fetchall(), columns = [tuple[0] for tuple in c.description])
all_rows.fillna(value="", inplace=True)

# Contents of all columns from Premiums Data
c.execute('SELECT * FROM {tn}'.\
        format(tn="Earned_Premiums"))
pd.options.display.float_format = '{:,.0f}'.format
all_premiums = pd.DataFrame (c.fetchall(), columns = [tuple[0] for tuple in c.description])
all_premiums.fillna(value="", inplace=True)












