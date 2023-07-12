#import argparse
from Downloads.utils import Clean_infos

#parser = argparse.ArgumentParser(description='up db for covidpaper')

#parser.add_argument('-from_')
#parser.add_argument('-to_')
#args = parser.parse_args()

data = Clean_infos('pubmed','all','pubmed_cleaned')
#data.clean_abstract()
#'pmid': 35291963
#data.update_db(int(args.from_),int(args.to_))
#data.update_db()


    
data.restrict_medline()
data.issn2categories()

"""
import pymongo
client = pymongo.MongoClient('mongodb://localhost:27017')
db = client["pubmed"]
collection = db["all"]


doc = collection.find_one({"pmid":36869612})
{pmid:31712091}
collection.delete_many({"meshwords":{"$exists":0}})
"""

import re
import tqdm
import pymongo
import datetime


# MongoDB connection settings
client = pymongo.MongoClient("mongodb://localhost")
db = client["pubmed"]
collection = db["pubmed_cleaned"]

pmids_1 = []
for paper in tqdm.tqdm(collection.find({"is_coronavirus_lower":1,"year_received":2019})):
    pmids_1.append(paper["pmid"])
    
collection = db["pubmed_2019_cleaned"]
query = {
    "$expr": {
        "$and": [
            {
                "$gte": [
                    {"$toLong": {"$toDate": "$unix_received"}},
                    1546300800
                ]
            },
            {
                "$lt": [
                    {"$toLong": {"$toDate": "$unix_received"}},
                    1577836799
                ]
            }
        ]
    },
    "is_coronavirus_lower":1
}


pmids_2 = []
for paper in tqdm.tqdm(collection.find(query)):
    pmids_2.append(paper["pmid"])    


not_matched_list2 = list(set(pmids_2) - set(pmids_1))

    
list_of_insertion = []
# Iterate over documents in the collection
for paper in tqdm.tqdm(collection.find({})):

    if paper['title']:
        text = str(paper['title']).lower() 
    else:
        text = ""
    if paper["abstract"]:
        text += str(paper['abstract']).lower()
    if not paper['meshwords']:
       paper['meshwords'] = [] 
    if not paper['meshsubwords']:
       paper['meshsubwords'] = [] 
    text +=  str(" ".join(paper['meshwords'])).lower() + str(" ".join(paper['meshsubwords'])).lower()
    is_in_text = any([any(re.search(w,text) for w in [i.lower() for i in ["2019-nCoV","2019nCoV","COVID-19","SARS-CoV-2","COVID19","COVID",
    "SARS-nCoV","Coronavirus","Corona virus","corona-virus","corona viruses",
    "coronaviruses","SARS-CoV","Orthocoronavirina","MERS-CoV",
    "Severe Acute Respiratory Syndrome","Middle East Respiratory Syndrome",
    "soluble ACE2"]]),
    all(re.search(w,text) for w in ["wuhan","coronavirus"]),
    all(re.search(w,text) for w in ["ace2","virus"]),
    all(re.search(w,text) for w in ["ardS","virus"]),
    all(re.search(w,text) for w in ["angiotensin-converting enzyme 2","virus"])])
    if is_in_text:
        covid_paper = 1
    else:
        covid_paper = 0    

    list_of_insertion.append(
        pymongo.UpdateOne(
            {"_id": paper["_id"]},
            {"$set": {"is_coronavirus_lower": covid_paper}}
        )
    )
    
    if len(list_of_insertion) == 10000:
        collection.bulk_write(list_of_insertion)
        list_of_insertion = []
        
collection.bulk_write(list_of_insertion)
list_of_insertion = []
print("Update completed.")