# -*- coding: utf-8 -*-

import requests
import json# call the OSMR API
import pandas as pd
import numpy as np
import time

#String of format {longitude},{latitude};{longitude},{latitude}[;{longitude},{latitude} ...] or polyline({polyline}).

request_str = "http://127.0.0.1:5000/table/v1/driving/"

coordinates_str = ""

df = pd.read_csv("Coordinates_Regensburg.csv", delimiter=",")

df["coordinates"] = df["Longitude"].astype(str)+","+df["Latitude"].astype(str)


coordinates_str = ";".join(df.coordinates.values)


print("Start request...")
r = requests.get(request_str+coordinates_str+"?annotations=distance,duration")
    
if r.status_code == 200:
    print(f"Request OK ({r.status_code})")
    table = json.loads(r.content)
    duration_matrix = np.array(table.get("durations"))
    distance_matrix = np.array(table.get("distances"))
    pd.DataFrame(duration_matrix).to_csv("durations_matrix.csv",header=None, index=None)
    pd.DataFrame(distance_matrix).to_csv("distance_matrix.csv",header=None, index=None)
    print("Saved matrix to csv...")
else:
    print("Error!")
    print(f"{r.status_code}")


source_coordinates = df["coordinates"].values[0]
destination_coordinates = df["coordinates"].values[500]

r_str = f"http://127.0.0.1:5000/route/v1/driving/{source_coordinates};{destination_coordinates}?overview=false&continue_straight=false"
r = requests.get(r_str)
request = json.loads(r.content)


