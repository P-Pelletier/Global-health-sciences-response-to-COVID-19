import tqdm
import pickle
import pymongo
import pycountry
import numpy as np
import pandas as pd
import datetime as dt 
from datetime import datetime
from collections import Counter
from collections import defaultdict


countries = {}
for country in pycountry.countries:
    countries[country.name] = country.alpha_2

client = pymongo.MongoClient("mongodb://localhost:27017")
mydb = client["pubmed"]
collection = mydb["pubmed_cleaned"]

def unix2YM(unix):
    date = datetime.utcfromtimestamp(unix)
    month = str(date.month)
    year = str(date.year)
    if len(month) == 1:
        month = "0"+month
    return(year+month)



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



periods = sorted(list(grants_country.keys()))

# Create an empty DataFrame
df = pd.DataFrame(columns=countries, index=periods)
df_corona = pd.DataFrame(columns=countries, index=periods)

# Populate the DataFrame with the distinct number of grant IDs
for period, data in grants_country.items():
    for country, grants in data.items():
        if country and country != "International":
            df.loc[period, country] = len(set(grants))

for period, data in grants_country_corona.items():
    for country, grants in data.items():
        if country and country != "International":
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



# 137 international agencies, 19672 distinct grant for overall, 445 for corona

#%% Common and new grants definition

grants_info = defaultdict(dict)
grants_info_corona = defaultdict(dict)

docs = collection.find()

time_period = []
for doc in tqdm.tqdm(docs):
    if doc["grants"] != None:
        if type(doc["grants"]) == dict:
            grants = [doc["grants"]]
        else:
            grants = doc["grants"]
        if doc["unix_received"]:
            date = unix2YM(doc["unix_received"])
            time_period.append(date)
            for grant in grants:
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

#%% top_10 funder

docs = collection.find()

top_10_grants = defaultdict(int)

for doc in tqdm.tqdm(docs):
    if doc["is_coronavirus_lower"] == 1:      
        if doc["grants"]:
            if type(doc["grants"]) == dict:
                grants = [doc["grants"]]
            else:
                grants = doc["grants"]
            for grant in grants:
                if grant["Country"] != None and grant["Country"] != "International":
                    if grant["Country"] == "Korea":
                        top_10_grants["Republic of Korea"] += 1
                    else:
                        top_10_grants[grant["Country"]] += 1

top_10_grants = sorted(top_10_grants, key=top_10_grants.get, reverse=True)[:10]

#%% Sanki somehow 

grants_corona_pre_covid = []
pre_covid_period = [i for i in time_period if int(i)>=201901 and int(i)<= 201912]
post_covid_period = [i for i in time_period if int(i)>=202001 and int(i)<= 202212]
for grant in tqdm.tqdm(grants_info_corona):
    if set(pre_covid_period).intersection(set(list(grants_info_corona[grant]))):
        grants_corona_pre_covid.append(grant)


grant_only_global_pre_covid = []
for grant in tqdm.tqdm(grants_info):
    if set(pre_covid_period).intersection(set(list(grants_info[grant]))):
        if grant not in grants_corona_pre_covid:
           grant_only_global_pre_covid.append(grant)


df = pd.DataFrame(np.zeros((len(top_10_grants), 3)))
df.index = top_10_grants 
df.columns = ["global_n_pre","global_n_post","corona_n_post"]      

sanki_left = {i:[] for i in top_10_grants}

for period in tqdm.tqdm(pre_covid_period):
    for country in tqdm.tqdm(top_10_grants):
        grants = grants_country[int(period)][country]
        sanki_left[country] += [i for i in grants if i in grant_only_global_pre_covid]

for country in sanki_left:
    df.at[country,"global_n_pre"] += len(list(set(sanki_left[country])))
    

sanki_right_1 = {i:[] for i in top_10_grants}

for period in tqdm.tqdm(post_covid_period):
    for country in tqdm.tqdm(top_10_grants):
        grants = grants_country[int(period)][country]
        sanki_right_1[country] += [i for i in grants if i in grant_only_global_pre_covid]
        
sanki_right_2 = {i:[] for i in top_10_grants}

for period in tqdm.tqdm(post_covid_period):
    for country in tqdm.tqdm(top_10_grants):
        grants = grants_country_corona[int(period)][country]
        sanki_right_2[country] += [i for i in grants if i in grant_only_global_pre_covid]

sanki_final = {i:{"n_global":0,"n_corona":0} for i in top_10_grants}
for country in tqdm.tqdm(top_10_grants):
    frequency_count_global = Counter(sanki_right_1[country])
    frequency_count_corona = Counter(sanki_right_2[country])
    for grant in tqdm.tqdm(frequency_count_corona):
        if frequency_count_corona[grant] >= 1:
            sanki_final[country]["n_corona"] += 1 
        else:
            sanki_final[country]["n_global"] += 1 
    for key in frequency_count_global.keys():
        if key not in frequency_count_corona:
            sanki_final[country]["n_global"] += 1 
    
df_common.to_csv("Data/fig_funding_d.csv")

#%% Share of new grant for corona

grant_global = {i:[] for i in  time_period if int(i)>=201801 and int(i)<= 202212}

for grant in tqdm.tqdm(grants_info):
    for month in grants_info[grant]:
        if int(month) >= 201801 and int(month) <= 202212:
            grant_global[month].append(grant)


t = 0
for period in tqdm.tqdm(grant_global):
    if t == 0:
        grant_global[period] = list(set(grant_global[period]))
        period_temp = period
        t +=1 
        continue
    else:
        grant_global[period] = list(set(grant_global[period] + grant_global[period_temp]))
        period_temp = period


grant_corona = {i:[] for i in  time_period if int(i)>=201801 and int(i)<= 202212}

for grant in tqdm.tqdm(grants_info_corona):
    for month in grants_info_corona[grant]:
        if int(month) >= 201801 and int(month) <= 202212:
            grant_corona[month].append(grant)

t = 0
for period in tqdm.tqdm(grant_corona):
    if t == 0:
        grant_corona[period] = list(set(grant_corona[period]))
        period_temp = period
        t +=1 
        continue
    else:
        grant_corona[period] = list(set(grant_corona[period] + grant_corona[period_temp]))
        period_temp = period

share_dict = {}
new_grant_dict = {}
new_grant_corona_dict = {}

for period in tqdm.tqdm(grant_corona):
    common_grant = len(set(grant_corona[period]).intersection(set(list(grant_global[period]))))
    new_grant = len(grant_corona[period]) - common_grant
    share = common_grant/(common_grant+new_grant)
    share_dict[period] = share
    new_grant_dict[period] = len(grant_global[period])
    new_grant_corona_dict[period] = len(grant_corona[period])

periods = [i for i in time_period if int(i)>=201801 and int(i) <= 202212]                
periods_dt = [dt.datetime.strptime(str(i), '%Y%m') for i in time_period if int(i)>=201801 and int(i) <= 202212]
periods.sort()
periods_dt.sort()

df = pd.DataFrame(index=periods)
df["share_common_grants"] = df.index.map(share_dict)
df["new_grant_dict"] = df.index.map(new_grant_dict)
df["new_grant_corona_dict"] = df.index.map(new_grant_corona_dict)
df["date"] = df.index

df.to_csv("Data/fig_funding_d.csv",index=False)

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
n_paper_inter_funded = defaultdict(int)
n_paper_inter_funded_corona = defaultdict(int)
n_paper = defaultdict(int)
n_paper_corona = defaultdict(int)
n_inter_collab_solo_funded = defaultdict(int)
n_inter_collab_solo_funded_corona = defaultdict(int)

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
        countries_funding = []
        for grant in grants:
            countries_funding.append(grant["Country"])
            if grant["Country"] == "International":
                try:
                    if doc["is_coronavirus_lower"] == 1:
                        grants_international_agency_corona[date][grant["Agency"]].append(grant["GrantID"])
                        if done_international == False:
                            n_paper_inter_funded_corona[date] += 1
                            done_international = True
                    else:
                        grants_international_agency[date][grant["Agency"]].append(grant["GrantID"])
                        if done_international == False:
                            n_paper_inter_funded[date] += 1
                            done_international = True
                except Exception as e:
                    continue
        if len(set(countries_funding)) == 1 and countries_funding[0] != "International" :
            if len(set(doc["country_list"].split(";"))) > 1:
                if doc["is_coronavirus_lower"] == 1:
                    n_inter_collab_solo_funded_corona[date] += 1
                else:
                    n_inter_collab_solo_funded[date] += 1
                    
periods = [i for i in list(n_paper) if int(i)>=201901 and int(i) <= 202212]                
periods_dt = [dt.datetime.strptime(str(i), '%Y%m') for i in list(n_paper) if int(i)>=201901 and int(i) <= 202212]
periods.sort()
periods_dt.sort()

df = pd.DataFrame(index=periods)
df_corona = pd.DataFrame(index=periods)

n_grants = {i:np.mean(n_grants[i]) for i in n_grants}
n_grants_corona = {i:np.mean(n_grants_corona[i]) for i in n_grants}

df["mean_grants"] = pd.Series(n_grants)   
df["n_papers"] = pd.Series(n_paper)
df["n_papers_inter_funding"] = pd.Series(n_paper_inter_funded)        
df["n_national_funding_inter_collab"] = pd.Series(n_inter_collab_solo_funded)
df["n_paper_with_grant"] = pd.Series(n_paper_with_grant)
df["share_inter_funding"] = df["n_papers_inter_funding"]/df["n_papers"]
df["share_national_funding_inter_collab"] = df["n_national_funding_inter_collab"]/df["n_papers"]
df.index = periods
df["type"] = "non-corona"

df_corona["mean_grants"] = pd.Series(n_grants_corona)   
df_corona["n_papers"] = pd.Series(n_paper_corona)
df_corona["n_papers_inter_funding"] = pd.Series(n_paper_inter_funded_corona)        
df_corona["n_national_funding_inter_collab"] = pd.Series(n_inter_collab_solo_funded_corona)        
df_corona["n_paper_with_grant"] =  pd.Series(n_paper_with_grant_corona)
df_corona["share_inter_funding"] = df_corona["n_papers_inter_funding"]/df_corona["n_papers"]
df_corona["share_national_funding_inter_collab"] = df_corona["n_national_funding_inter_collab"]/df_corona["n_papers"] 
df_corona.index = periods
df_corona["type"] = "corona"




df_final = pd.concat([df, df_corona], axis=0)
df_final["date"] = df_final.index
df_final.to_csv("Data/fig_funding.csv")










