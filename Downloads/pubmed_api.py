import requests
import re
import tqdm
import pymongo
import time
import xmltodict
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.firefox.options import Options

from Downloads.utils import get_info_new

#add some argparser for username, password, start_at
# start_at = specific id, e.g starting at 30M pmid. If already some data or you want to start from 0 let start_at = None


# get some last id references from pubmed to know when to stop the loop
options = Options()
options.headless = True
driver = webdriver.Firefox(options=options)
driver.get("https://pubmed.ncbi.nlm.nih.gov/?term=Latest+Literature&sort=date")
element = WebDriverWait(driver, 10).until(EC.visibility_of_element_located((By.XPATH, "//span[@class='docsum-pmid']")))
end_pmid = element.text
print(end_pmid)
driver.close()

# Open mongodb collection

client = pymongo.MongoClient("mongodb://localhost:27017")
mydb = client["pubmed"]
collection = mydb["all"]

all_ids="25537518,1"
{'pmid': 25537518}

def inf_dl(collection,start_at = None):

    # init while loop
    working = True
    
    s = requests.Session()
    while working == True:
        time.sleep(1)
        # If you specified a start_at then no need to load a previous pmid for the first iteration
        if start_at:
            last_id = int(start_at)
            start_at = None
        # Else load last_id to continue from last_id
        else:
            try:
                last_id = collection.find().sort([("pmid",-1)]).limit(1)
                last_id = int([i for i in last_id][-1]["pmid"]) + 1 
            except Exception as e:
                print(str(e),"No pmid in collection so starting at pmid = 1")
                last_id = 1
        
        print(last_id)
        # Number of papers processed per request
        n_api = 200
        # append the 200 ids for the query
        all_ids = []
        for i in range(n_api):
            all_ids.append(last_id + i)  
        all_ids_temp = [id_ for id_ in all_ids]
        all_ids = ",".join([str(i) for i in all_ids])

        # api requests, read pubmed entrez documentation      

        try: 
            response = s.get("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id={}&showaiid=yes".format(all_ids))
        except:
            time.sleep(3)
            s = requests.Session()            
            response = s.get("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id={}&showaiid=yes".format(all_ids))
        dict_data = xmltodict.parse(response.content)
        try:
            if type(dict_data["PubmedArticleSet"]["PubmedArticle"]) == list:
                all_text = dict_data["PubmedArticleSet"]["PubmedArticle"]
            else:
                all_text =[dict_data["PubmedArticleSet"]["PubmedArticle"]]
        except:
            all_text = None
        # Split the response by papers
        #all_text = str(response.content).split("PubmedArticle")[1:]
        
        # if papers have been found store information
        if all_text != None:
            list_of_insertion = []
            for text in tqdm.tqdm(all_text):
                id_text = int(text['MedlineCitation']["PMID"]["#text"])
                try:                    
                    abstract,mesh_words,mesh_subwords,all_authors, title, grant,doi,ISSN,unix,unix_received,unix_accepted,\
                        unix_medline,year = get_info_new(text,id_text)
                    post = {"pmid": int(id_text),
                            "title":title,
                            'ISSN' : ISSN,
                            "abstract":abstract,
                            "meshwords":mesh_words,
                            "meshsubwords":mesh_subwords,
                            "authors":all_authors,
                            "grants":grant,
                            "doi":doi,
                            "unix":unix,
                            "unix_received" : unix_received,
                            "unix_accepted" : unix_accepted,
                            "unix_medline" : unix_medline,
                            "year":year}
                    list_of_insertion.append(post)
                except Exception as e:
                    print(str(e))
            if len(list_of_insertion) > 0:
                collection.insert_many(list_of_insertion)
            else:
                post = {"delete":1, "pmid": int(all_ids_temp[-1])}
                collection.insert_one(post)
        # stop if you are 200 iteration awa
        # If nothing found still update db with the 200 ids so last_id is updated next iteration, need to find a cleaner solution
        else:
            print("No text for the 200 ids")
            post = {"delete":1,
                    "pmid": int(all_ids_temp[-1])}
            collection.insert_one(post)
        # stop if you are 200 iteration away from end_pmid
        try:
            if int(id_text) > (int(end_pmid)-200):
                working = False
        except Exception as e:
            print(str(e))

if __name__ == "__main__":
    collection.create_index([ ("pmid",1) ])
    inf_dl(collection)



