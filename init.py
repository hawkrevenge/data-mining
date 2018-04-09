# -*- coding: utf-8 -*-
"""
Created on Mon Apr  9 10:26:19 2018

"""

import csv
import numpy as np

class User:
    def __init__(self, id):
        self.ID = id #number of the subject
        self.Days = [] #day

class Day:
    def __init__(self, tS, mood):
        self.timeStamp = tS #timestamp of the day
        self.mood = mood # mood of the user of that day
        self.activities = [] #list of activities of that person (dictionary)
        
class Activity:
    def __init__(self,variable, score):
        self.variable = []
def Read():
    csvfile = open("dataset_mood_smartphone.csv", 'r')
    reader = csv.reader(csvfile, delimiter=',')
    data = np.array([np.array(i) for i in list(reader)])
    print(data[1])
    print(data[1][2])
    userList = []
    activityList = []
    for d in data[1:]:
        if d[1] not in userList:        
            userList += [d[1]]
        if d[2] not in activityList and not d[2] == 'mood':        
            activityList += [d[2]]
    print(userList)
    print(activityList)
Read()