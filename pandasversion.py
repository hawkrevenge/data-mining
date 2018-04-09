# -*- coding: utf-8 -*-
"""
Created on Mon Apr  9 16:16:09 2018

@author: Roelv
"""

import pandas as pd

list1 = pd.read_csv("dataset_mood_smartphone.csv", sep=',', encoding='latin1', parse_dates=['time'], infer_datetime_format= True, dayfirst=True)
#print(list1)