import tqdm
import pandas as pd
import numpy as np
import pymongo
from Stats.utils import Create_net
import tikzplotlib
import pycountry
import matplotlib.patches as mpatches
import matplotlib.font_manager as font_manager
import os
from matplotlib.lines import Line2D
from datetime import datetime
import datetime as dt 
import matplotlib.pyplot as plt
from collections import defaultdict
import plotly.express as px
from plotly.offline import plot

countries = {}
for country in pycountry.countries:
    countries[country.name] = country.alpha_2

client = pymongo.MongoClient("mongodb://localhost:27017")
mydb = client["pubmed"]
collection = mydb["pubmed_2015_cleaned"]

grants_info = defaultdict(dict)
grants_info_corona = defaultdict(dict)
docs = collection.find()

def unix2YM(unix):
    date = datetime.utcfromtimestamp(unix)
    month = str(date.month)
    year = str(date.year)
    if len(month) == 1:
        month = "0"+month
    return(year+month)

n_corona_pre_covid = 0
n_corona_post_covid = 0
n_pre_covid = 0
n_post_covid = 0
n_grants_corona_pre_covid = 0
n_grants_corona_post_covid = 0
n_grants_pre_covid = 0
n_grants_post_covid = 0

for doc in tqdm.tqdm(docs):
    date = int(unix2YM(doc["unix_received"]))
    if doc["is_coronavirus_lower"] == 1:
        if date > 202001:
            n_corona_post_covid += 1
            if doc["grants"] != None:
                n_grants_corona_post_covid += 1
        else:
            n_corona_pre_covid += 1
            if doc["grants"] != None:
                n_grants_corona_pre_covid += 1
    else:
        if date > 202001:
            n_post_covid += 1
            if doc["grants"] != None:
                n_grants_post_covid += 1
        else:
            n_pre_covid += 1
            if doc["grants"] != None:
                n_grants_pre_covid += 1

n_grants_corona_pre_covid/n_corona_pre_covid
n_grants_corona_post_covid/n_corona_post_covid
n_grants_pre_covid/n_pre_covid
n_grants_post_covid/n_post_covid

docs = collection.find()

time_period = []
for doc in tqdm.tqdm(docs):
    if doc["grants"] != None:
        if doc["unix_received"]:
            date = unix2YM(doc["unix_received"])
            time_period.append(date)
            for grant in doc["grants"]:
                if isinstance(grant,dict):
                    if "GrantID" in grant:
                        try:
                            if doc["is_coronavirus_lower"] == 1:                    
                                grants_info_corona[grant["GrantID"]][date] += 1
                            else:
                                grants_info[grant["GrantID"]][date] += 1
                        except:
                            if doc["is_coronavirus_lower"] == 1:                    
                                grants_info_corona[grant["GrantID"]][date] = 1
                            else:
                                grants_info[grant["GrantID"]][date] = 1
                  
grants_info = {i:grants_info[i] for i in grants_info if grants_info[i] != {}}
grants_info_corona = {i:grants_info_corona[i] for i in grants_info_corona if grants_info_corona[i] != {}}
time_period = list(set(time_period))
time_period.sort()
common_grants = list(set(list(grants_info_corona)).intersection(list(grants_info)))
new_grants =  list(set(list(grants_info_corona)).symmetric_difference(common_grants))


# Common grants

dict_of_insertion = {i:{"corona":0,"non_corona":0} for i in time_period}

for grant in tqdm.tqdm(common_grants):
    for period in grants_info[grant]:
        dict_of_insertion[period]["non_corona"] += grants_info[grant][period]
    for period in grants_info_corona[grant]:
        dict_of_insertion[period]["corona"] += grants_info_corona[grant][period]

list_of_insertion = []
for i in dict_of_insertion:
    list_of_insertion.append({"time_period":i,
     "corona":dict_of_insertion[i]["corona"],
     "non_corona":dict_of_insertion[i]["non_corona"]})

df = pd.DataFrame(list_of_insertion,columns=["time_period","corona","non_corona"])






