import json
import pymongo
import pandas as pd
import tqdm
from datetime import datetime
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
client = pymongo.MongoClient('mongodb://localhost:27017/')
db = client['pubmed']
collection = db['pubmed_2019_cleaned']

data = collection.find()
dist =[{'unix_received':i['unix_received'],
             'unix_accepted':i['unix_accepted'],
             'unix_medline':i['unix_medline'],
             'unix':i['unix'],
             'is_corona':i['is_coronavirus_lower']} for i in tqdm.tqdm(data) if i['country_list'] != '']

unix_df = pd.DataFrame(dist)
#unix_df = unix_df[unix_df.unix_received < 1609459200]
unix_df[unix_df['unix_received'] < 1577836800]
unix_df[unix_df['unix_received'] > 1577836800]


unix_df['date'] = list(map(lambda i: datetime.strptime(str(datetime.fromtimestamp(i)),  "%Y-%m-%d %H:%M:%S"),
                      unix_df.unix_received))
month_count = unix_df.groupby([unix_df.date.dt.to_period("M"),'is_corona']).date.agg('count')
month_count = month_count.unstack()
month_count.plot(legend = True)


unix_df = unix_df.astype({'date': 'str'})
unix_df['time_diff_received_medline']=(unix_df['unix_medline']-unix_df['unix_received'])/(3600*24)
unix_df['time_diff_received_pubmed']=(unix_df['unix']-unix_df['unix_received'])/(3600*24)
unix_df['time_diff_received_accepted']=(unix_df['unix_accepted']-unix_df['unix_received'])/(3600*24)

def show_distplots2(year,variable):
    unix_df_ = unix_df[unix_df['date'].str.contains(year)]
    fig = plt.figure(figsize=(10,6))
    unix_df_[unix_df_[variable] > 0].groupby('is_corona')[variable].apply(sns.distplot, hist=False, rug=True)
    fig.legend(labels=['Non Coronavirus','Coronavirus'])
    plt.title(variable+' '+year)
    plt.show()
    fig.savefig('Results/'+variable+year+'.png',
                dpi=fig.dpi)

def closest_to(dist,value):
    diff_ = abs(dist[0] - value)
    xloc = np.where(diff_ == np.min(diff_))
    x = dist[0][xloc]
    y = dist[1][xloc]
    return x, y
    
def show_distplots(year,variable):
    unix_df_ = unix_df[unix_df['date'].str.contains(year)]
    fig = plt.figure(figsize=(10,6))
    non_corona =  unix_df_[unix_df_[variable] > 0][unix_df_['is_corona']==0][variable]
    corona = unix_df_[unix_df_[variable] > 0][unix_df_['is_corona']==1][variable]
    dist_nc = sns.distplot(non_corona, hist=False, rug=True,color='blue')
    dist_c = sns.distplot(corona, hist=False, rug=True,color='orange')
    value_c = round(np.percentile(unix_df_[unix_df_[variable] > 0][unix_df_['is_corona']==1][variable], 90))
    x_c, y_c = closest_to(dist_c.get_lines()[0].get_data(),value_c)
    #plt.plot([x_c,x_c], [0, y_c],color='orange')
    #print(x_c,y_c)
    
    value_nc = round(np.percentile(unix_df_[unix_df_[variable] > 0][unix_df_['is_corona']==0][variable], 90))
    x_nc, y_nc = closest_to(dist_nc.get_lines()[0].get_data(),value_nc)
    #plt.plot([x_nc,x_nc], [0, y_nc],color='blue')
    print(value_c,value_nc)
    fig.legend(labels=['Non Coronavirus','Coronavirus',"90 percentiles non corona :{}".format(str(value_nc)),
                       "90 percentiles corona :{}".format(str(value_c))])
    plt.title(variable+' '+year)
    plt.show()
    fig.savefig('Results/'+variable+year+'.png',
                dpi=fig.dpi)


show_distplots('2019','time_diff_received_accepted')
show_distplots('2020','time_diff_received_accepted')
show_distplots('2021','time_diff_received_accepted')
show_distplots('2019','time_diff_received_medline')
show_distplots('2020','time_diff_received_medline')
show_distplots('2021','time_diff_received_medline')
show_distplots('2019','time_diff_received_pubmed')
show_distplots('2020','time_diff_received_pubmed')
show_distplots('2021','time_diff_received_pubmed')




sns.distplot(unix_df_2019.time_diff[unix_df_2019.time_diff > 0], hist = False, kde = True,
                 kde_kws = {'linewidth': 3})

unix_df_2020 = unix_df[unix_df['date'].str.contains('2020')]
sns.distplot(unix_df_2020.time_diff[unix_df_2020.time_diff > 0], hist = False, kde = True,
                 kde_kws = {'linewidth': 3})






#### Nb of country


client = pymongo.MongoClient('mongodb://localhost:27017/')
db = client['pubmed_cleaned2019']
collection = db['pubmed_cleaned']

docs = collection.find({})


df = []

for doc in tqdm.tqdm(docs):
    if doc['nb_country'] >1:
        if '2019' in doc['date_received']:
            if doc['is_coronavirus_lower'] == 0:
                df.append({'year':2019,'nb_country':doc['nb_country'],'is_corona':0})
            if doc['is_coronavirus_lower'] == 1:
                df.append({'year':2019,'nb_country':doc['nb_country'],'is_corona':1})
                
        elif '2020' in doc['date_received']:
            if doc['is_coronavirus_lower'] == 0:
                df.append({'year':2020,'nb_country':doc['nb_country'],'is_corona':0})
            if doc['is_coronavirus_lower'] == 1:
                df.append({'year':2020,'nb_country':doc['nb_country'],'is_corona':1})
                
        elif '2021' in doc['date_received']:
            if doc['is_coronavirus_lower'] == 0:
                df.append({'year':2021,'nb_country':doc['nb_country'],'is_corona':0})
            if doc['is_coronavirus_lower'] == 1:
                df.append({'year':2021,'nb_country':doc['nb_country'],'is_corona':1})
    
    
df = pd.DataFrame(df)
df.to_csv('nbcountry_year.csv')    