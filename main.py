# -*- coding: utf-8 -*-
"""
Created on Fri Nov 15 13:10:28 2019
"""

import csv
import tkinter as tk
from tkinter import filedialog

class Time:
    def __init__(self):
        self.second = 0
        self.minute = 0
        self.hour = 0
        self.day = 0
        self.month = 0
        self.year = 0
    
    def __str__(self):
        return "{}/{}/{},{}:{}:{}".format(self.month, self.day, self.year, self.hour, self.minute, self.second)

class Earthquake:
    def __init__(self):
        self.date = Time()
        self.latitude = 0
        self.longitude = 0
        self.depth = 0
        self.mag = 0
        self.gap = 0
        self.dmin = 0
        self.rms = 0
        self.horizontalError = 0
        self.depthError = 0
        self.magError = 0
        self.magNst = 0
    
    def __str__(self):
        return "{}\n\tLat:{} Lon:{}\n\tDepth:{} Mag:{} Gap:{}\n\tDmin:{} Rms:{}\n\tHorizontalErr:{} DepthErr:{}\n\tMagErr:{} MagNst:{}".format(
                self.date, self.latitude, self.longitude, self.depth, self.mag, self.gap, self.dmin, self.rms, self.horizontalError, self.depthError, self.magError, self.magNst)

# use dialog box to select a file.
root = tk.Tk()
root.withdraw()

file_path = filedialog.askopenfilename()

# read a csv file
rows = []

with open(file_path, 'r') as csvfile:
    csvreader = csv.reader(csvfile)
    
    
    for row in csvreader:
        rows.append(row)

del rows[0]

earthquakes = []

highest_earthquake = Earthquake()

# Parse all the data
for row in rows:
    buffer_earthquake = Earthquake()
    
    # Get date
    buffer_earthquake.date.year = row[0][:4]
    buffer_earthquake.date.month = row[0][5:7]
    buffer_earthquake.date.day = row[0][8:10]
    buffer_earthquake.date.hour = row[0][11:13]
    buffer_earthquake.date.minute = row[0][14:16]
    buffer_earthquake.date.second = row[0][17:]
    
    # Get data points
    try:
        buffer_earthquake.latitude = float(row[1])
    except:
        print("Empty Latitude")
    try:
        buffer_earthquake.longitude = float(row[2])
    except:
        print("Empty Longitute")
    try:
        buffer_earthquake.depth = float(row[3])
    except:
        print("Empty depth")
    try:
        buffer_earthquake.mag = float(row[4])
    except:
        print("Empty mag")
    try:
        buffer_earthquake.gap = float(row[7])
    except:
        print("Empty gap")
    try:
        buffer_earthquake.dmin = float(row[8])
    except:
        print("Empty dmin")
    try:
        buffer_earthquake.rms = float(row[9])
    except:
        print("Empty rms")
    try:
        buffer_earthquake.horizontalError = float(row[15])
    except:
        print("Empty horizontalError")
    try:
        buffer_earthquake.depthError = float(row[16])
    except:
        print("Empty depthError")
    try:
        buffer_earthquake.magError = float(row[17])
    except:
        print("Empty magError")
    try:
        buffer_earthquake.magNst = float(row[18][:-1])
    except:
        print("Empty magNst")
    
    # Find highest values in each column
    if buffer_earthquake.depth > highest_earthquake.depth:
        highest_earthquake.depth = buffer_earthquake.depth
    if buffer_earthquake.mag > highest_earthquake.mag:
        highest_earthquake.mag = buffer_earthquake.mag
    if buffer_earthquake.gap > highest_earthquake.gap:
        highest_earthquake.gap = buffer_earthquake.gap
    if buffer_earthquake.dmin > highest_earthquake.dmin:
        highest_earthquake.dmin = buffer_earthquake.dmin
    if buffer_earthquake.rms > highest_earthquake.rms:
        highest_earthquake.rms = buffer_earthquake.rms
    if buffer_earthquake.horizontalError > highest_earthquake.horizontalError:
        highest_earthquake.horizontalError = buffer_earthquake.horizontalError
    if buffer_earthquake.depthError > highest_earthquake.depthError:
        highest_earthquake.depthError = buffer_earthquake.depthError
    if buffer_earthquake.magError > highest_earthquake.magError:
        highest_earthquake.magError = buffer_earthquake.magError
    if buffer_earthquake.magNst > highest_earthquake.magNst:
        highest_earthquake.magNst = buffer_earthquake.magNst
    
    earthquakes.append(buffer_earthquake)

print("\n\n\n")
print(str(highest_earthquake))

average_earthquake = Earthquake()
count = 0

# Normalize all data
for earthquake in earthquakes:
    earthquake.depth /= highest_earthquake.depth
    earthquake.mag /= highest_earthquake.mag
    earthquake.gap /= highest_earthquake.gap
    earthquake.dmin /= highest_earthquake.dmin
    earthquake.rms /= highest_earthquake.rms
    earthquake.horizontalError /= highest_earthquake.horizontalError
    earthquake.depthError /= highest_earthquake.horizontalError
    earthquake.magError /= highest_earthquake.magError
    earthquake.magNst /= highest_earthquake.magNst
    
    average_earthquake.depth += earthquake.depth
    average_earthquake.mag += earthquake.mag
    average_earthquake.gap += earthquake.gap
    average_earthquake.dmin += earthquake.dmin
    average_earthquake.rms += earthquake.rms
    average_earthquake.horizontalError += earthquake.horizontalError
    average_earthquake.depthError += earthquake.depthError
    average_earthquake.magError += earthquake.magError
    average_earthquake.magNst += earthquake.magNst
    
    count += 1

# fill in missing data
for x in earthquakes:
    if x.depth == 0:
        x.depth = average_earthquake.depth
    if x.mag == 0:
        x.mag = average_earthquake.mag
    if x.gap == 0:
        x.gap = average_earthquake.gap
    if x.dmin == 0:
        x.dmin = average_earthquake.dmin
    if x.rms == 0:
        x.rms = average_earthquake.rms
    if x.horizontalError == 0:
        x.horizontalError = average_earthquake.horizontalError
    if x.depthError == 0:
        x.depthError = average_earthquake.depthError
    if x.magError == 0:
        x.magError = average_earthquake.magError
    if x.magNst == 0:
        x.magNst = average_earthquake.magNst

with open('processed.csv', mode='w') as earthquake_file:
    earthquake_write = csv.writer(earthquake_file, lineterminator='\n')
    earthquake_write.writerow(['Date', 'Depth', 'Mag', 'Gap', 'Dmin', 'Rms', 'HorizontalErr', 'DepthErr', 'MagErr', 'MagNst'])
    
    for x in earthquakes:
        print("\n\n\nEarthquake" , x)
        earthquake_write.writerow([x.date, x.depth, x.mag, x.gap, x.dmin, x.rms, x.horizontalError, x.depthError, x.magError, x.magNst])