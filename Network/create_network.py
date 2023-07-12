import tqdm
import pandas as pd
import pymongo
from Network.utils import Create_net
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
time_period = net_instance_corona.time_period

# Create others network

net_instance = Create_net(collection,{"$and":[{"is_coronavirus_lower":0}]},last_date = last_date, start_date = start_date )
net_instance.create_list_city(scale = "country")
net_instance.populate_network()
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
    inp.insert(0, 'month', month)
    edge_list = pd.concat([edge_list,inp])

edge_list.to_csv("Data/Data_{}/edge_list.csv".format(str(last_date_year)))



