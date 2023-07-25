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
import pickle
from matplotlib.lines import Line2D
from datetime import datetime
import matplotlib.pyplot as plt

countries = {}
for country in pycountry.countries:
    countries[country.name] = country.alpha_2

client = pymongo.MongoClient("mongodb://localhost:27017")
mydb = client["pubmed"]
collection = mydb["pubmed_cleaned"]

start_date = datetime(2019,1,1)
start_covid = datetime(2020,1,1)
last_date = datetime(2022,12,31)
last_date_year = last_date.year

if not os.path.exists("Data/Data_{}".format(str(last_date.year))):
    os.makedirs("Data/Data_{}".format(str(last_date.year)))


n_month_after_covid = (last_date.year - start_covid.year) * 12 + (last_date.month - start_covid.month) + (last_date.day - start_covid.day)/30
if len(str(last_date.month)) == 1:
    last_date = int(str(last_date.year)+"0"+ str(last_date.month))
else:
    last_date = int(str(last_date.year)+ str(last_date.month))


if len(str(start_date.month)) == 1:
    start_date = int(str(start_date.year)+"0"+ str(start_date.month))
else:
    start_date = int(str(start_date.year)+ str(start_date.month))

# Create corona pub dict

instance_corona = Create_net(collection,{"$and":[{"is_coronavirus_lower":1}]},last_date = last_date, start_date = start_date)
instance_corona.create_list_city(scale = "country")
instance_corona.populate_publication_dict()
pub_corona = instance_corona.n_publication
time_period = instance_corona.time_period

# Create others pub dict

instance_others = Create_net(collection,{"$and":[{"is_coronavirus_lower":0}]},last_date = last_date, start_date = start_date)
instance_others.create_list_city(scale = "country")
instance_others.populate_publication_dict()
pub_data = instance_others.n_publication


# additionnal info of publications for others scripts

add_corona =  instance_corona.n_publication_add
add_others =  instance_others.n_publication_add
add_info = pd.DataFrame()

for month in tqdm.tqdm(time_period):
    add_corona[month].columns = add_corona[month].columns + "Corona"
    test = pd.concat([add_others[month],add_corona[month]],axis=1)
    test.insert(0, 'month', month)
    add_info = add_info.append(test)

add_info['country'] = add_info.index
add_info.to_csv("Data/Data_{}/country_pub_info.csv".format(str(last_date_year)), index=False)

"""
n = 0
for year in range(201901,201913,1):
    n += pub_corona[str(year)]["n_pub"]["Somalia"]
"""

#%% n_pub per month covid vs non-covid


papers = collection.find()


publication = pd.DataFrame(np.zeros((len(time_period), 2)))
publication.index = time_period
publication.columns = ["n_pub","n_pub_corona"]


for paper in tqdm.tqdm(papers):
    date = instance_others.get_unix(paper)
    if int(date) <= instance_others.last_date and int(date) >= instance_others.start_date:
        if paper["is_coronavirus_lower"] == 0:
            publication.at[date, "n_pub"] += 1
        else:
            publication.at[date, "n_pub_corona"] += 1

#%% aggregate pre-post

# Aggregate pub

periods = ["others_pre","corona_pre","others_post","corona_post"]

df_pub = pd.DataFrame(np.zeros((len(instance_others.city_country_list), 1)))
df_pub.columns = ["n_pub"]
df_pub.index = instance_others.city_country_list



pub_data_aggr = {}
for period in periods:
    pub_data_aggr[period] = df_pub.copy()

for month in time_period:    
    if int(month) < 202001:
        pub_data_aggr["others_pre"] += pub_data[month]
        pub_data_aggr["corona_pre"] += pub_corona[month]
    else:
        pub_data_aggr["others_post"] += pub_data[month]
        pub_data_aggr["corona_post"] += pub_corona[month]

# pareto plot data

pareto = pd.concat([pub_data_aggr["corona_pre"], pub_data_aggr["corona_post"],pub_data_aggr["others_pre"],pub_data_aggr["others_post"]],axis=1)
pareto.columns = ["corona_pre","corona_post","others_pre","others_post"]
pareto.to_csv("Data/Data_{}/pareto.csv".format(str(last_date_year)), index=False)


share_corona_pre = pareto["corona_pre"]/pareto["corona_pre"].sum()
share_others_pre = pareto["others_pre"]/pareto["others_pre"].sum()
share_corona_post = pareto["corona_post"]/pareto["corona_post"].sum()
share_others_post = pareto["others_post"]/pareto["others_post"].sum()
pareto_share = pd.DataFrame({"share_corona_pre":share_corona_pre, 
                             "share_others_pre":share_others_pre,
                             "share_corona_post":share_corona_post,
                             "share_others_post":share_others_post})
pareto_share = pareto_share.fillna(value = 0)
pareto_share["countries"] = pareto_share.index
pareto_share.to_csv("Data/Data_{}/pareto_share.csv".format(str(last_date_year)), index=False)


#  Country publication ranking by period and type of research

for period in periods:
    pub_data_aggr[period] = pub_data_aggr[period].sort_values(by=["n_pub"],ascending = False)
    pub_data_aggr[period]["rank"] = pub_data_aggr[period].rank(method='max',ascending=False)["n_pub"] 

# Rank_diff

rank_diff_month = []
for month in time_period:
    pub_data[month] = pub_data[month].sort_values(by=["n_pub"],ascending = False)
    pub_data[month]["rank"] = pub_data[month].rank(method='max',ascending=False)["n_pub"]
    pub_corona[month] = pub_corona[month].sort_values(by=["n_pub"],ascending = False)
    pub_corona[month]["rank"] = pub_corona[month].rank(method='max',ascending=False)["n_pub"]
    test = np.absolute(pub_data[month]["rank"]-pub_corona[month]["rank"])
    q01= test.quantile(0.1)
    q09 = test.quantile(0.9)
    q25 = test.quantile(0.25)
    q75 = test.quantile(0.75)
    rank_diff_month.append([np.mean(test),np.median(test),np.std(test),q25,q75])



df_rank = pd.DataFrame(rank_diff_month)
df_rank.index = time_period

"""
# average corona pre-post

rank_average_pre_corona = pd.DataFrame()
rank_average_pre_corona["rank"] = np.zeros(len(pub_corona["201901"]))
rank_average_pre_corona.index = instance_others.city_country_list

rank_average_post_corona = rank_average_pre_corona.copy()

n_pre = 0
n_post = 0
for month in time_period:
    if int(month) < 202001:
        for row in pub_corona[month].iterrows():
            rank_average_pre_corona.at[row[0],"rank"] += row[1]["rank"]
        n_pre += 1
    else:
        for row in pub_corona[month].iterrows():
            rank_average_post_corona.at[row[0],"rank"] += row[1]["rank"]
        n_post += 1
rank_average_pre_corona = rank_average_pre_corona.div(n_pre)
rank_average_post_corona = rank_average_post_corona.div(n_post)

for month in time_period:
    if int(month) < 202001:
        pub_corona[month]["rank"] = rank_average_pre_corona.reindex(pub_corona[month].index)
    else:
        pub_corona[month]["rank"] = rank_average_post_corona.reindex(pub_corona[month].index)
"""
# rank_emond

df_rank_edmond = pd.DataFrame(np.zeros((len(instance_corona.city_country_list), len(instance_corona.city_country_list))))
df_rank_edmond.index = instance_corona.city_country_list
df_rank_edmond.columns = instance_corona.city_country_list
dict_edmond = {key: df_rank_edmond.copy() for key in time_period}   
dict_edmond_corona = {key: df_rank_edmond.copy() for key in time_period} 

for month in tqdm.tqdm(time_period):
    for country1 in pub_data[month].T:
        for country2 in pub_data[month].T:
            if pub_data[month].T[country1][0] >= pub_data[month].T[country2][0]:
                dict_edmond[month].at[country1, country2] = 1
            else:
                dict_edmond[month].at[country1, country2] = -1
            if pub_corona[month].T[country1][0] >= pub_corona[month].T[country2][0]:
                dict_edmond_corona[month].at[country1, country2] = 1
            else:
                dict_edmond_corona[month].at[country1, country2] = -1
    dict_edmond[month].values[[np.arange(dict_edmond[month].shape[0])]*2] = 0
    dict_edmond_corona[month].values[[np.arange(dict_edmond_corona[month].shape[0])]*2] = 0


rank_corr_month = []
for month in tqdm.tqdm(time_period):
    tau = np.matmul(dict_edmond[month].stack().T,dict_edmond_corona[month].stack())/(len(instance_corona.city_country_list)*(len(instance_corona.city_country_list)-1))
    rank_corr_month.append(tau)

df_rank_corr = pd.DataFrame(rank_corr_month)
df_rank_corr.index = time_period


"""
# Boostrap

df_rank_corr["mean_simu"] = None
df_rank_corr["var_simu"] = None

B = 500

for month in tqdm.tqdm(time_period):
    tau_simu = []
    for b in range(B):
        df_temp = dict_edmond[month].sample(frac=1,replace=True)
        df_temp = df_temp[df_temp.index]
        df_temp_corona = dict_edmond_corona[month][df_temp.index].T[df_temp.index]
        pseudo_tau = np.matmul( df_temp.stack(), df_temp_corona.stack())/(len(df_temp)*(len(df_temp)-1))
        tau_simu.append(pseudo_tau)
    df_rank_corr.at[month, "mean_simu"]  = np.mean(tau_simu)
    df_rank_corr.at[month, "var_simu"]  = np.var(tau_simu)
"""




#Leave one out

LCI_months = []
UCI_months = []

for month in tqdm.tqdm(time_period):
    n_simu = 0
    tau_list = []
    real_tau = df_rank_corr[0].T[month]
    N = len(instance_corona.city_country_list)
    for country in instance_corona.city_country_list:
        resample = dict_edmond[month].drop(country,axis = 0).drop(country,axis = 1)
        resample_corona = dict_edmond_corona[month].drop(country,axis = 0).drop(country,axis = 1)
        tau = np.matmul(resample.stack(),resample_corona.stack())/(len(resample)*(len(resample)-1))
        pseudo_tau = (N*real_tau) - ((N-1)*tau)
        tau_list.append(pseudo_tau)
    mean_pseudo_tau = (1/N)*np.sum(tau_list)
    var_pseudo_tau = np.sum([np.square(pseudo-mean_pseudo_tau) for pseudo in tau_list])/(N-1)
    LCI = mean_pseudo_tau-1.960*(np.sqrt((1/N)*var_pseudo_tau))
    UCI = mean_pseudo_tau+1.960*(np.sqrt((1/N)*var_pseudo_tau))
    LCI_months.append(LCI)
    UCI_months.append(UCI)

df_rank_corr["LCI"] = LCI_months
df_rank_corr["UCI"] = UCI_months

df_rank_corr.columns = ["Tau","LCI","UCI"]

#%% data for R plot

#figa

dates = list(range(201901,201913,1)) + list(range(202001,202013,1)) + list(range(202101,202113,1))+list(range(202201,202213,1))




dates = [str(date)[:4] + "-" + str(date)[4:] for date in dates]
publication.columns = ["non_Coronavirus","Coronavirus"]
publication.index = dates
publication['month'] = publication.index
publication.to_csv("Data/Data_{}/fig1a.csv".format(str(last_date_year)), index=False)

#figb

total_research = pub_data_aggr["corona_pre"]["n_pub"]+ pub_data_aggr["corona_post"]["n_pub"]
top = list(total_research.sort_values(ascending=False)[0:10].index)
pickle.dump( top, open( "Data/top_10_publisher.p", "wb" ) )


df1 = (pub_data_aggr["corona_pre"]["n_pub"].T[top].T)
df2 = (pub_data_aggr["corona_post"]["n_pub"].T[top].T)
df_barplot = pd.concat([df1,df2],axis=1)
df_barplot['country'] = df_barplot.index
df_barplot.columns = ["corona related pre covid", "corona related post covid","country"]
df_barplot.to_csv("Data/Data_{}/fig1b.csv".format(str(last_date_year)), index=False)

#figc

df_rank_corr.index = dates
df_rank_corr['month'] = df_rank_corr.index
df_rank_corr.to_csv("Data/Data_{}/fig1c.csv".format(str(last_date_year)), index=False)


#%% Create tikzplot of stats


# n_pub

dates = ["Jan 2019", "Feb 2019", "Mar 2019", "Apr 2019", "May 2019", "Jun 2019", "Jul 2019", "Aug 2019", "Sep 2019", "Oct 2019", "Nov 2019", "Dec 2019",
         "Jan 2020", "Feb 2020", "Mar 2020", "Apr 2020", "May 2020", "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020","Dec 2020",
         "Jan 2021", "Feb 2021", "Mar 2021", "Apr 2021", "May 2021", "Jun 2021", "Jul 2021", "Aug 2021", "Sep 2021", "Oct 2021", "Nov 2021","Dec 2021",
         "Jan 2022", "Feb 2022", "Mar 2022", "Apr 2022", "May 2022", "Jun 2022", "Jul 2022", "Aug 2022", "Sep 2022", "Oct 2022", "Nov 2022","Dec 2022",]
publication.index = dates
publication['month'] = publication.index
publication.to_csv("Data/Data_{}/fig1a.csv".format(str(last_date_year)), index=False)


fig, ax = plt.subplots(1, 1, figsize=(3, 2), dpi=300)
publication.plot(ax=ax,lw=2)#,title="Corona vs non corona\n log number of article per month")
plt.setp(ax.get_xticklabels(), rotation=30, horizontalalignment='right')
ax.axvline(11, color='k', linestyle='--')
ax.axvline(23, color='k', linestyle='--')
ax.set_ylabel("Log number of publications")
ax.set_yscale('log')
plt.savefig('Results/publication.png')
tikzplotlib.save("Results/publication.tex", axis_height='3.58cm', axis_width='6.12cm',dpi=300)

'''
# rank

fig, ax = plt.subplots(1, 1, figsize=(3, 2), dpi=300)

df_rank.index = dates
df_rank.columns = ["Mean","Median","Std","Q25","Q75"]
df_rank["Median"].plot(ax=ax,color='black',lw=2)
#ax.plot(df_rank.index,df_rank[0],color='blue')
#ax.plot(df_rank.index,df_rank[1],color='orange',lw=2)
ax.fill_between(df_rank.index, df_rank["Q25"], df_rank["Q75"], color='#e8eaf6', alpha=1)
plt.setp(ax.get_xticklabels(), rotation=30, horizontalalignment='right')
ax.axvline(11, color='k', linestyle='--')
#ax.title.set_text('Rank difference country \n between corona related and non corona related number of publication')
ax.set_ylabel("Rank difference")

colors = ['black',"#e8eaf6"]
lines = [Line2D([0], [0], color=c, linewidth=2, linestyle='-') for c in colors]
labels = ['median',"IQ range"]
ax.legend(lines, labels,loc=1)


tikzplotlib.save("./rank_differences.tex", axis_height='3.58cm', axis_width='6.12cm',dpi=300)
'''
# rank_corr

fig, ax = plt.subplots(1, 1, figsize=(3, 2), dpi=300)

df_rank_corr.index = dates
df_rank_corr['month'] = df_rank_corr.index
df_rank_corr.to_csv("Data/Data_{}/fig1c.csv".format(str(last_date_year)), index=False)

df_rank_corr["Tau"].plot(ax=ax,color='black',lw=2)
ax.fill_between(df_rank.index, df_rank_corr["LCI"], df_rank_corr["UCI"], color='#e8eaf6', alpha=1)
plt.setp(ax.get_xticklabels(), rotation=30, horizontalalignment='right')
ax.axvline(11, color='k', linestyle='--')
ax.set_ylabel("Rank correlation")

colors = ['black',"#e8eaf6"]
lines = [Line2D([0], [0], color=c, linewidth=2, linestyle='-') for c in colors]
labels = ['Tau',"Jackknife CI"]
ax.legend(lines, labels,loc=2)

plt.savefig('Results/rank_differences.png')
tikzplotlib.save("Results/rank_differences.tex", axis_height='3.58cm', axis_width='6.12cm',dpi=300)


# barplot of publication aggr of countries

font = font_manager.FontProperties(family='Comic Sans MS',
                                   weight='bold',
                                   style='normal', size=16)

total_research = pub_data_aggr["corona_pre"]["n_pub"]+ pub_data_aggr["corona_post"]["n_pub"]
top = list(total_research.sort_values(ascending=False)[0:10].index)
df1 = (pub_data_aggr["corona_pre"]["n_pub"].T[top].T)
df2 = (pub_data_aggr["corona_post"]["n_pub"].T[top].T)
#df2.sum()/pub_data_aggr["corona_post"]["n_pub"].sum()
df_barplot = pd.concat([df1,df2],axis=1)
df_barplot['country'] = df_barplot.index
df_barplot.to_csv("Data/Data_{}/fig1b.csv".format(str(last_date_year)), index=False)


df_barplot.columns = ["corona related pre covid", "corona related post covid","country"]
df_barplot = df_barplot#.apply(np.log)
labels = [countries.get(country, 'Unknown code') for country in top]

fig, ax = plt.subplots(1, 1, figsize=(3,2), dpi=300)

x = np.arange(len(df_barplot))
width = 0.2
rects1 = ax.bar(x - width/2, df_barplot["corona related pre covid"], width, label='pre',color="darkmagenta")
rects2 = ax.bar(x + width/2, df_barplot["corona related post covid"], width, label='post',color="dimgrey")
ax.set_xticks(x)
ax.set_xticklabels(labels)
ax.set_ylabel("Coronavirus papers")
ax.set_yscale('log')
plt.setp(ax.get_xticklabels(), rotation=90, horizontalalignment='center',fontsize=6,fontname="Arial")

pre = mpatches.Patch(color='darkmagenta', label='Pre COVID-19')
post = mpatches.Patch(color='dimgrey', label='Post COVID-19')

font = font_manager.FontProperties(family='sans-serif',
                                   style='normal', size=6)
plt.legend(handles=[pre, post], loc=1, prop=font)
plt.savefig('Results/country_publication_log.png')
# weird bug in tikz, need to change the starting point from 1 to 0.01, guessing that log scale not supported for barplot
tikzplotlib.save("Results/country_publication_log.tex", axis_height='3.58cm', axis_width='6.12cm',dpi=300)






