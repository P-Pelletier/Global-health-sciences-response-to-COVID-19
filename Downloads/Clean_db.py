#import argparse
from Downloads.utils import Clean_infos

#parser = argparse.ArgumentParser(description='up db for covidpaper')

#parser.add_argument('-from_')
#parser.add_argument('-to_')
#args = parser.parse_args()

data = Clean_infos('pubmed','all','pubmed_2015_cleaned')
#data.update_db(int(args.from_),int(args.to_))
data.update_db(int(30000000),int(35000000))
    

    
data.restrict_medline()
data.issn2categories()

"""
import pymongo
client = pymongo.MongoClient('mongodb://localhost:27017')
db = client["pubmed"]
collection = db["all"]


doc = collection.find_one({"pmid":36869612})

collection.delete_many({"meshwords":{"$exists":0}})
"""