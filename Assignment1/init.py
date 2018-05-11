# -*- coding: utf-8 -*-
"""
Created on Mon Apr  9 10:26:19 2018

"""

import csv
import numpy as np
from datetime import datetime
from datetime import timedelta
from datetime import date

class User:
    def __init__(self, id):
        self.ID = id #number of the subject
        self.Days = {} #day

class Day:
    def __init__(self, tS, mood):
        self.timeStamp = tS #timestamp of the day
        self.mood = mood # mood of the user of that day
        self.activities = {} #dictionary of list of activities of that person 
        
class Activity:
    def __init__(self,variable, score):
        self.variable = variable
        self.score = score
        
def Read():
    csvfile = open("dataset_mood_smartphone.csv", 'r')
    reader = csv.reader(csvfile, delimiter=',')
    data = np.array([np.array(i) for i in list(reader)])
    earliestDate = date(2014,2,17)
    userList = [] #list with the right dictionary names
    activityList = [] #list with the right dictionary names
    Users = {}
    for d in data[1:]:
        temp = datetime.strptime(d[2],"%Y-%m-%d %H:%M:%S.%f")      
        tempDays = date(2014,temp.month,temp.day)-earliestDate
        tempDelta = timedelta(days = tempDays.days, hours = temp.hour, seconds = temp.second)
        if d[1] not in userList:        
            userList += [d[1]]
            Users[d[1]] = User(d[1])
        if tempDelta.days not in Users[d[1]].Days:
            Users[d[1]].Days[tempDelta.days] = Day(tempDelta,-1)
        if d[3] == 'mood':
            Users[d[1]].Days[tempDelta.days].mood = d[4]
        if d[3] not in activityList and not d[3] == 'mood':        
            activityList += [d[3]]
            if d[3] not in Users[d[1]].Days[tempDelta.days].activities:
                Users[d[1]].Days[tempDelta.days].activities[d[3]] = [Activity(d[3],d[4])]
            else:
                Users[d[1]].Days[tempDelta.days].activities[d[3]] += [Activity(d[3],d[4])]
Read()