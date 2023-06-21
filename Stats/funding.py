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
import numpy as np

countries = {}
for country in pycountry.countries:
    countries[country.name] = country.alpha_2

client = pymongo.MongoClient("mongodb://localhost:27017")
mydb = client["pubmed"]
collection = mydb["pubmed_cleaned"]

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

df_common = pd.DataFrame(list_of_insertion,columns=["time_period","corona","non_corona"])


# New grants

dict_of_insertion = {i:{"corona":0,"non_corona":0} for i in time_period}

for grant in tqdm.tqdm(new_grants):
    for period in grants_info_corona[grant]:
        dict_of_insertion[period]["corona"] += grants_info_corona[grant][period]

list_of_insertion = []
for i in dict_of_insertion:
    list_of_insertion.append({"time_period":i,
     "corona":dict_of_insertion[i]["corona"],
     "non_corona":dict_of_insertion[i]["non_corona"]})

df_new = pd.DataFrame(list_of_insertion,columns=["time_period","corona","non_corona"])

#%% TOP 10 country most funding for coronavirus research pre-post. Top 10 country most funding for research pre-post


client = pymongo.MongoClient("mongodb://localhost:27017")
mydb = client["pubmed"]
collection = mydb["pubmed_cleaned"]

grants_country = defaultdict(lambda: defaultdict(list))
grants_country_corona = defaultdict(lambda: defaultdict(list))
docs = collection.find()


for doc in tqdm.tqdm(docs):
    date = int(unix2YM(doc["unix_received"]))
    if doc["grants"]:
        if type(doc["grants"]) == dict:
            grants = [doc["grants"]]
        for grant in grants:
            try:
                if doc["is_coronavirus_lower"] == 1:
                    grants_country_corona[date][grant["Country"]].append(grant["GrantID"])
                else:
                    grants_country[date][grant["Country"]].append(grant["GrantID"])
            except Exception as e:
                continue


countries = sorted(list(set(country for period in grants_country.values() for country in period.keys())))
periods = sorted(list(grants_country.keys()))

# Create an empty DataFrame
df = pd.DataFrame(columns=countries, index=periods)
df_corona = pd.DataFrame(columns=countries, index=periods)

# Populate the DataFrame with the distinct number of grant IDs
for period, data in grants_country.items():
    for country, grants in data.items():
        df.loc[period, country] = len(set(grants))

for period, data in grants_country_corona.items():
    for country, grants in data.items():
        df_corona.loc[period, country] = len(set(grants))

sum_before = df.loc[:'202001', :].sum()
sum_after = df.loc['202002':, :].sum()

top_10_countries_before = sum_before.nlargest(10).index.tolist()
top_10_countries_after = sum_after.nlargest(10).index.tolist()

print("Top 10 countries with the biggest sum before 202001:", top_10_countries_before)
print("Top 10 countries with the biggest sum after 202001:", top_10_countries_after)

sum_before = df_corona.loc[:'202001', :].sum()
sum_after = df_corona.loc['202002':, :].sum()

top_10_countries_before = sum_before.nlargest(10).index.tolist()
top_10_countries_after = sum_after.nlargest(10).index.tolist()

print("Top 10 countries with the biggest sum before 202001:", top_10_countries_before)
print("Top 10 countries with the biggest sum after 202001:", top_10_countries_after)


for doc in tqdm.tqdm(docs):
    date = int(unix2YM(doc["unix_received"]))
    if doc["grants"]:
        if type(doc["grants"]) == dict:
            grants = [doc["grants"]]
        for grant in grants:
            try:
                if doc["is_coronavirus_lower"] == 1:
                    grants_country_corona[date][grant["Country"]].append(grant["GrantID"])
                else:
                    grants_country[date][grant["Country"]].append(grant["GrantID"])
            except Exception as e:
                continue

# 33 country funding, 

#%% Number of international funding 

client = pymongo.MongoClient("mongodb://localhost:27017")
mydb = client["pubmed"]
collection = mydb["pubmed_cleaned"]



grants_international_agency = defaultdict(lambda: defaultdict(list))
grants_international_agency_corona = defaultdict(lambda: defaultdict(list))
n_grants = defaultdict(list)
n_grants_corona = defaultdict(list)
n_paper_inter = defaultdict(int)
n_paper_inter_corona = defaultdict(int)
n_paper = defaultdict(int)
n_paper_corona = defaultdict(int)
n_inter_collab_solo_funded = defaultdict(int)
n_inter_collab_solo_corona = defaultdict(int)

docs = collection.find()


for doc in tqdm.tqdm(docs):
    date = int(unix2YM(doc["unix_received"]))
    if doc["grants"]:
        if type(doc["grants"]) == dict:
            grants = [doc["grants"]]
        for grant in grants:
            if grant["Country"] == "International":
                try:
                    if doc["is_coronavirus_lower"] == 1:
                        grants_international_agency_corona[date][grant["Agency"]].append(grant["GrantID"])
                    else:
                        grants_international_agency[date][grant["Agency"]].append(grant["GrantID"])
                except Exception as e:
                    continue

agencies = sorted(list(set(country for period in grants_international_agency.values() for country in period.keys())))
periods = sorted(list(grants_international_agency.keys()))


# Create an empty DataFrame
df = pd.DataFrame(columns=agencies, index=periods)
df_corona = pd.DataFrame(columns=agencies, index=periods)

# Populate the DataFrame with the distinct number of grant IDs
for period, data in grants_international_agency.items():
    for country, grants in data.items():
        df.loc[period, country] = len(set(grants))

for period, data in grants_international_agency_corona.items():
    for country, grants in data.items():
        df_corona.loc[period, country] = len(set(grants))

df.sum().sum()
df_corona.sum().sum()        

# 137 international agencies, 19672 distinct grant for overall, 445 for corona

#%% Plot dfs

client = pymongo.MongoClient("mongodb://localhost:27017")
mydb = client["pubmed"]
collection = mydb["pubmed_cleaned"]



grants_international_agency = defaultdict(lambda: defaultdict(list))
grants_international_agency_corona = defaultdict(lambda: defaultdict(list))
n_paper_with_grant = defaultdict(int)
n_paper_with_grant_corona = defaultdict(int)
n_grants = defaultdict(list)
n_grants_corona = defaultdict(list)
n_paper_inter = defaultdict(int)
n_paper_inter_corona = defaultdict(int)
n_paper = defaultdict(int)
n_paper_corona = defaultdict(int)
n_inter_collab_solo = defaultdict(int)
n_inter_collab_solo_corona = defaultdict(int)

docs = collection.find()

for doc in tqdm.tqdm(docs):
    date = int(unix2YM(doc["unix_received"]))
    if doc["is_coronavirus_lower"] == 1:
        n_paper_corona[date] += 1
    else:
        n_paper[date] += 1       
    if doc["grants"]:
        if type(doc["grants"]) == dict:
            grants = [doc["grants"]]
        else:
            grants = doc["grants"]
        if doc["is_coronavirus_lower"] == 1:
            n_grants_corona[date].append(len(grants))
            n_paper_with_grant_corona[date] += 1
        else:
            n_grants[date].append(len(grants))
            n_paper_with_grant[date] += 1
        done_international = False
        for grant in grants:
            if grant["Country"] == "International":
                try:
                    if doc["is_coronavirus_lower"] == 1:
                        grants_international_agency_corona[date][grant["Agency"]].append(grant["GrantID"])
                        if done_international == False:
                            n_paper_inter_corona[date] += 1
                            done_international = True
                    else:
                        grants_international_agency[date][grant["Agency"]].append(grant["GrantID"])
                        if done_international == False:
                            n_paper_inter[date] += 1
                            done_international = True
                except Exception as e:
                    continue
        if done_international == False:
            if len(set(doc["country_list"].split(";"))) > 1:
                if doc["is_coronavirus_lower"] == 1:
                    n_inter_collab_solo_corona[date] += 1
                else:
                    n_inter_collab_solo[date] += 1
periods = [i for i in list(n_paper) if int(i)>201901 and int(i) < 202212]                
periods_dt = [dt.datetime.strptime(str(i), '%Y%m') for i in list(n_paper) if int(i)>201901 and int(i) < 202212]
periods.sort()
periods_dt.sort()

df = pd.DataFrame(index=periods)
df_corona = pd.DataFrame(index=periods)

n_grants = {i:np.mean(n_grants[i]) for i in n_grants}
n_grants_corona = {i:np.mean(n_grants[i]) for i in n_grants}

df["mean_grants"] = pd.Series(n_grants)   
df["n_papers"] = pd.Series(n_paper)
df["n_papers_inter_funding"] = pd.Series(n_paper_inter)        
df["n_national_funding_inter_collab"] = pd.Series(n_inter_collab_solo)
df["n_paper_with_grant"] = pd.Series(n_paper_with_grant)
df["share_inter_funding"] = df["n_papers_inter_funding"]/df["n_papers"]
df["share_national_funding_inter_collab"] = df["n_national_funding_inter_collab"]/df["n_papers"]
df.index = periods_dt
df["type"] = "non-corona"

df_corona["mean_grants"] = pd.Series(n_grants_corona)   
df_corona["n_papers"] = pd.Series(n_paper_corona)
df_corona["n_papers_inter_funding"] = pd.Series(n_paper_inter_corona)        
df_corona["n_national_funding_inter_collab"] = pd.Series(n_inter_collab_solo_corona)        
df_corona["n_paper_with_grant"] =  pd.Series(n_paper_with_grant_corona)
df_corona["share_inter_funding"] = df_corona["n_papers_inter_funding"]/df_corona["n_papers"]
df_corona["share_national_funding_inter_collab"] = df_corona["n_national_funding_inter_collab"]/df_corona["n_papers"] 
df_corona.index = periods_dt
df_corona["type"] = "corona"


df_final = pd.concat([df, df_corona], axis=0)
df_final["date"] = df_final.index
df_final.to_csv("Data/fig_funding.csv")
