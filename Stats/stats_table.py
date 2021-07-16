import pandas as pd
import pymongo
import tqdm

df = pd.read_csv("Data/Data_2020/country_pub_info.csv")
df.groupby("month").sum()
summed = df.groupby("month").sum()
summed["collabPubsCorona"]/(summed["solePubsCorona"]+summed["collabPubsCorona"])

client = pymongo.MongoClient('mongodb://localhost:27017/')
db = client["pubmed"]
collection = db["pubmed_2019_cleaned"]


docs = collection.find({})

n_inter = 0
n_solo = 0
for doc in tqdm.tqdm(docs):
    if doc["inter_collab"] == 0:
        n_solo +=1
    else:
        n_inter += 1


client = pymongo.MongoClient('mongodb://localhost:27017/')
db = client["test"]
collection = db["test"]

docs = collection.aggregate([
    {"$unwind": {
        "path": "$time",
        "includeArrayIndex": "position"
        }
    },
    {
    "$project":  {
             "unix":"$unix",
             "time_average":{"$avg":"$time"},
             "position":"$position"
     }
    }
])




print(next(docs))
docs = collection.find({"year":2019,"time.0":doc["time"][0]})
doc = next(docs)
doc = doc
doc["time"][0]
