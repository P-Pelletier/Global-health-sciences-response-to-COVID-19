import json
import pymongo
import pandas as pd
import tqdm
from datetime import datetime
import seaborn as sns
import matplotlib.pyplot as plt

client = pymongo.MongoClient('mongodb://localhost:27017/')
db = client['pubmed_cleaned2019']
collection = db['pubmed_cleaned']

data = collection.find()
dist =[{'unix_received':i['unix_received'],
             'unix_accepted':i['unix_accepted'],
             'unix_medline':i['unix_medline'],
             'unix':i['unix'],
             'is_corona':i['is_coronavirus_lower']} for i in tqdm.tqdm(data) if i['country_list'] != '']

unix_df = pd.DataFrame(dist)
unix_df = unix_df[unix_df.unix_received < 1609459200]
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

def show_distplots(year,variable):
    unix_df_ = unix_df[unix_df['date'].str.contains(year)]
    fig = plt.figure(figsize=(10,6))
    unix_df_[unix_df_[variable] > 0].groupby('is_corona')[variable].apply(sns.distplot, hist=False, rug=True)
    fig.legend(labels=['Non Coronavirus','Coronavirus'])
    plt.title(variable+' '+year)
    plt.show()
    fig.savefig('C:/Users/pierre/Documents/GitHub/Global-health-sciences-response-to-COVID-19/Results/'+variable+year+'.png',
                dpi=fig.dpi)


show_distplots('2019','time_diff_received_accepted')
show_distplots('2020','time_diff_received_accepted')
show_distplots('2019','time_diff_received_medline')
show_distplots('2020','time_diff_received_medline')
show_distplots('2019','time_diff_received_pubmed')
show_distplots('2020','time_diff_received_pubmed')




sns.distplot(unix_df_2019.time_diff[unix_df_2019.time_diff > 0], hist = False, kde = True,
                 kde_kws = {'linewidth': 3})

unix_df_2020 = unix_df[unix_df['date'].str.contains('2020')]
sns.distplot(unix_df_2020.time_diff[unix_df_2020.time_diff > 0], hist = False, kde = True,
                 kde_kws = {'linewidth': 3})