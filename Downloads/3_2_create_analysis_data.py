import tqdm
import pandas as pd
import pymongo
from Utils.Network_utils import Create_net
from datetime import datetime
import pycountry

countries = {}
for country in pycountry.countries:
    countries[country.name] = country.alpha_2


#%% df

client = pymongo.MongoClient('mongodb://Pierre:ilovebeta67@localhost:27017/')
mydb = client["pubmed"]
collection = mydb["pubmed_cleaned"]

start_covid = datetime(2020,1,1)
start_date = datetime(2019,1,1)
start_date_year = start_date.year
last_date = datetime(2022,12,31)
last_date_year = last_date.year

n_month_after_covid = (last_date.year - start_covid.year) * 12 + (last_date.month - start_covid.month) + (last_date.day - start_covid.day)/30
if len(str(last_date.month)) == 1:
    last_date = int(str(last_date.year)+"0"+ str(last_date.month))
else:
    last_date = int(str(last_date.year)+ str(last_date.month))
    
if len(str(start_date.month)) == 1:
    start_date = int(str(start_date.year)+"0"+ str(start_date.month))
else:
    start_date = int(str(start_date.year)+ str(start_date.month))

# Create corona network

net_instance_corona = Create_net(collection,{"$and":[{"is_coronavirus_lower":1}]},last_date = last_date, start_date = start_date )
net_instance_corona.create_list_city(scale = "country")
net_instance_corona.populate_network()
data_corona = net_instance_corona.network
dependance_corona = net_instance_corona.dependance_funding
grant_corona = net_instance_corona.grant
no_grant_corona = net_instance_corona.no_grant
time_period = net_instance_corona.time_period

# Create others network

net_instance = Create_net(collection,{"$and":[{"is_coronavirus_lower":0}]},last_date = last_date, start_date = start_date )
net_instance.create_list_city(scale = "country")
net_instance.populate_network()
dependance = net_instance.dependance_funding
grant = net_instance.grant
no_grant = net_instance.no_grant
data = net_instance.network


# edge_list

edge_list = pd.DataFrame()

for month in tqdm.tqdm(time_period):
    inp = data[month].stack()
    inp = inp[inp >= 1].rename_axis(('source', 'target')).reset_index(name='weight')
    inp.insert(inp.shape[1],"weight_corona",0)
    inp_corona = data_corona[month].stack()
    inp_corona = inp_corona[inp_corona >= 1].rename_axis(('source', 'target')).reset_index(name='weight')
    for row in inp_corona.iterrows():
        source = row[1]["source"]
        target = row[1]["target"]
        weight = row[1]["weight"]
        if inp.loc[(inp.source == source)&(inp.target == target), 'weight_corona'].empty:
            inp.loc[len(inp)+1] = [source,target,0,weight]
        else:
            inp.loc[(inp.source == source)&(inp.target == target), 'weight_corona'] = weight
    inp["dep_i"] = 0
    inp["dep_j"] = 0
    inp["paper_funded"] = 0
    for row in inp.iterrows():
        source = row[1]["source"]
        target = row[1]["target"]
        n_paper_with_grant_corona = grant_corona[month].at[source,target]
        n_paper_with_grant = grant[month].at[source,target]
        """
        dep_ratio_corona = (dependance_corona[month].at[source,target]/n_paper_with_grant_corona)*(dependance_corona[month].at[target,source]/n_paper_with_grant_corona)
        dep_ratio = (dependance[month].at[source,target]/n_paper_with_grant)*(dependance[month].at[target,source]/n_paper_with_grant)
        dep_i = (dependance[month].at[source,target]+dependance_corona[month].at[source,target])/(n_paper_with_grant+n_paper_with_grant_corona)
        dep_j = (dependance[month].at[target,source]+dependance_corona[month].at[target/source])/(n_paper_with_grant+n_paper_with_grant_corona)
        dep_total = dep_i*dep_j
        inp.loc[(inp.source == source)&(inp.target == target), 'dep_ratio'] = dep_ratio
        inp.loc[(inp.source == source)&(inp.target == target), 'dep_ratio_corona'] = dep_ratio_corona
        inp.loc[(inp.source == source)&(inp.target == target), 'dep_total'] = dep_total
        """
        inp.loc[(inp.source == source)&(inp.target == target), 'dep_i'] = dependance_corona[month].at[source,target] + dependance[month].at[source,target]
        inp.loc[(inp.source == source)&(inp.target == target), 'dep_j'] = dependance_corona[month].at[target,source] + dependance[month].at[target,source]
        inp.loc[(inp.source == source)&(inp.target == target), 'paper_funded'] = n_paper_with_grant+n_paper_with_grant_corona
               
    inp.insert(0, 'month', month)
    edge_list = pd.concat([edge_list,inp])

edge_list.to_csv("Data/Data_{}/edge_list.csv".format(str(last_date_year)))



