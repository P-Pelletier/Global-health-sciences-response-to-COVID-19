import re
import tqdm
import json
import time
import pymongo
import requests


client = pymongo.MongoClient("mongodb://localhost:27017")
mydb = client["pubmed"]
collection = mydb["pubmed_2015_cleaned"]
collection_OA = mydb["OpenAlex"]
session = client.start_session()
docs = collection.find(no_cursor_timeout=True, session = session).skip(2082613+28974)



list_ids = []
for doc in tqdm.tqdm(docs):
    list_ids.append(doc["pmid"])
    if len(list_ids) == 25:
        all_ids = "|".join([str(i) for i in list_ids])
        #all_ids = re.sub(r'\|', r'|pmid:', all_ids)
        response = requests.get("https://api.openalex.org/works?filter=pmid:{}".format(all_ids))
        while response.status_code != 200:
            time.sleep(10)
            client.admin.command('refreshSessions', [session.session_id], session=session)
            response = requests.get("https://api.openalex.org/works?filter=pmid:{}".format(all_ids))

        to_insert = json.loads(response.content)["results"]    
        if to_insert == []:
            print("nothing")
            continue
        collection_OA.insert_many(to_insert)
        list_ids = []
        time.sleep(3)
        




to_insert
