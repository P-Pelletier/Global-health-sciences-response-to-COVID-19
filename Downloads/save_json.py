# -*- coding: utf-8 -*-
"""
Created on Wed Jun 16 07:57:30 2021

@author: kevin
"""

import pymongo
from bson.json_util import dumps
import tqdm

client = pymongo.MongoClient('mongodb://localhost:27017/')
mydb = client["pubmed"]
collection = mydb["pubmed_2019_cleaned"]
cursor = collection.find({})
with open('pubmed_2019_cleaned.json', 'w') as file:
    file.write('[')
    for document in tqdm.tqdm(cursor):
        file.write(dumps(document))
        file.write(',')
    file.write(']')