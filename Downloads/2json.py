# -*- coding: utf-8 -*-
"""
Created on Tue Jul  5 13:48:00 2022

@author: pierre
"""

import pymongo 
import tqdm
import json

client = pymongo.MongoClient('mongodb://localhost:27017')
db = client['pubmed']
col= db['pubmed_2015_cleaned']

#n = col.count_documents({"wos_cat":{"$exists":1}})

col.update_many({}, { "$unset" : { "wos_cat" : 1} })

reqs = [({'unix_received':{'$gt':1420070400,'$lt':1451606400}},2015),
({'unix_received':{'$gt':1451606400,'$lt':1483228800}},2016),
({'unix_received':{'$gt':1483228800,'$lt':1514764800}},2017),
({'unix_received':{'$gt':1514764800,'$lt':1546300800}},2018),
({'unix_received':{'$gt':1546300800,'$lt':1577836800}},2019),
({'unix_received':{'$gt':1577836800,'$lt':1609459200}},2020),
({'unix_received':{'$gt':1609459200,'$lt':1640995200}},2021),
({'unix_received':{'$gt':1640995200}},2022)]

for req in reqs:
    docs = col.find(req[0])
    data = []
    for doc in tqdm.tqdm(docs):
        try:
            data.append({"pmid":doc["pmid"],
                     "unix_received":doc["unix_received"],
                     "unix":doc["unix"],
                     "Location_cities_country":doc["Location_cities_country"],
                     "wos_cat":doc["wos_cat"]})
        except Exception as e:
            #print(str(e))
            data.append({"pmid":doc["pmid"],
                     "unix_received":doc["unix_received"],
                     "unix":doc["unix"],
                     "Location_cities_country":doc["Location_cities_country"]})            
    with open("D:/covid_pubmed/{}.json".format(str(req[1])), "w") as final:
       json.dump(data, final)


"""
with open("D:/covid_pubmed/2015.json", "r") as final:
       data = json.load(final)
       
"""
