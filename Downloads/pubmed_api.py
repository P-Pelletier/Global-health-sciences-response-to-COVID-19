import requests
import re
import tqdm
import pymongo

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.firefox.options import Options

from utils import get_info

#add some argparser for username, password, start_at
# start_at = specific id, e.g starting at 30M pmid. If already some data or you want to start from 0 let start_at = None


# get some last id references from pubmed to know when to stop the loop
options = Options()
options.headless = True
driver = webdriver.Firefox(options=options)
driver.get("https://pubmed.ncbi.nlm.nih.gov/?term=Latest+Literature&sort=date")
element = WebDriverWait(driver, 10).until(EC.visibility_of_element_located((By.XPATH, "//span[@class='docsum-pmid']")))
end_pmid = element.text
driver.close()

# Open mongodb collection
client = pymongo.MongoClient('mongodb://localhost:27017/')
mydb = client["pubmed"]
collection = mydb["pubmed_2019"]

def inf_dl(collection,start_at = None):

    # init while loop
    working = True
    

    while working == True:
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
        
        # Number of papers processed per request
        n_api = 200
        
        # append the 200 ids for the query
        all_ids = []
        for i in range(n_api):
            all_ids.append(last_id + i)  
        all_ids_temp = [id_ for id_ in all_ids]
        all_ids = ",".join([str(i) for i in all_ids])

        # api requests, read pubmed entrez documentation            
        response = requests.get("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id={}&showaiid=yes".format(all_ids))
        
        # Split the response by papers
        all_text = str(response.content).split("Pubmed-entry")[1:]
        
        # if papers have been found store information
        if len(all_text):
            for text in tqdm.tqdm(all_text):
                try:
                    id_text = re.findall("pmid [0-9]+",text)[0].split("pmid ")[-1]
                    abstract,mesh_words,mesh_subwords,all_authors,date,date_received,date_accepted,date_medline,title,\
                    source,grant,doi,ISSN,unix,unix_received,unix_accepted,unix_medline = get_info(text,id_text)
                    post = {"pmid": int(id_text),
                            "title":title,
                            'ISSN' : ISSN,
                            "abstract":abstract,
                            "meshwords":mesh_words,
                            "meshsubwords":mesh_subwords,
                            "authors":all_authors,
                            "source":source,
                            "grants":grant,
                            "date":date,
                            "date_received":date_received,
                            "date_accepted":date_accepted,
                            "date_medline":date_medline,
                            "doi":doi,
                            "unix":unix,
                            "unix_received" : unix_received,
                            "unix_accepted" : unix_accepted,
                            "unix_medline" : unix_medline}
                    collection.insert_one(post)
                except Exception as e:
                    print(str(e))
        # If nothing found still update db with the 200 ids so last_id is updated next iteration, need to find a cleaner solution
        else:
            print("No text for the 200 ids")
            for id_ in all_ids_temp:
                post = {"pmid": int(id_),
                        "title":None,
                        'ISSN' : None,
                        "abstract":None,
                        "meshwords":None,
                        "meshsubwords":None,
                        "authors":None,
                        "source":None,
                        "grants":None,
                        "date":None,
                        "date_received":None,
                        "date_accepted":None,
                        "date_medline":None,
                        "doi":None,
                        "unix":None,
                        "unix_received" : None,
                        "unix_accepted" : None,
                        "unix_medline" : None}
            collection.insert_one(post)
        # stop if you are 200 iteration away from end_pmid
        try:
            if int(id_text) > (int(end_pmid)-200):
                working = False
        except Exception as e:
            print(str(e))

if __name__ == "__main__":
    inf_dl(collection,start_at = None)
    collection.create_index([ ("pmid",1) ])

