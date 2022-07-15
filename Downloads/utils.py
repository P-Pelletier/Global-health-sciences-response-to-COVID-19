import os
import re
import time
import datetime
import pymongo
import tqdm
import csv
from crossref.restful import Works
import pandas as pd
import json




class Clean_infos:
    
    def __init__(self,db_name,coll_name,coll_name_clean):
        '''
        

        Parameters
        ----------
        db_name : str
            mongo db name.
        coll_name : str
            mongo collection name.
        year_window : list
            list of year to keep pnly documents from those years in the database

        Returns
        -------
        Init mongodb and cities csv file

        '''
        
        #self.year_window = year_window
        client = pymongo.MongoClient('mongodb://Pierre:ilovebeta67@localhost:27017')
        self.db = client[db_name]
        self.collection = self.db[coll_name]
        self.collection_clean = self.db[coll_name_clean]
        self.df = self.collection.find()
        csv_f = open(r'Data/cleaned_cities_loc.csv','r', encoding='utf-8')
        self.works = Works()
        self.cities = [i for i in  csv.reader(csv_f)]
        csv_f.close() 
    
    def get_name(self,author,aff_split):
        '''
        Parameters
        ----------
        author : str
            author variable provided by the pubmed metadata.
        aff_split : str
            pattern use to split the string.

        Returns
        -------
        name : list
        list of authors names
        '''
        name = author.split(aff_split)[0]
        return name
    
    
    def clean_country(self,author,aff_split):
        '''
        Parameters
        ----------
        author : str
            author variable provided by the pubmed metadata.
        aff_split : str
            pattern use to split the string.

        Returns
        -------
        query_geo : str
            Clean affiliation for a given author before match with country names. 
        '''
        
        if '; ' in author:
            author = author.split('; ')[0]
        if aff_split in author:
            author = author.split(aff_split)[1]
        if author is not None:
            first_aff = author.split(',')
            query_geo = ' '.join(first_aff)
            query_geo = query_geo.split(' ')
            for i in query_geo:
                if '@' in i:
                    query_geo.remove(i)
        query_geo = ' '.join(query_geo)
        query_geo = re.sub('[.]','',query_geo)
        query_geo = re.sub('  ',' ',query_geo)
        query_geo = re.sub(' Electronic address:','',query_geo)
        query_geo = re.sub(' E-mail:','',query_geo)
        query_geo = re.sub('FIN-','',query_geo)
        query_geo = re.sub(' USA',' United States',query_geo)
        query_geo = re.sub(' Korea',' South Korea',query_geo)
        query_geo = re.sub(' UK',' United Kingdom',query_geo)
        query_geo = re.sub('Liberal','',query_geo)
        query_geo = re.sub(' University','',query_geo)
        query_geo = self.clean_for_usa(query_geo)
        return query_geo
    
    def clean_for_usa(self,query_geo):
        """
        Parameters
        ----------
        query_geo : str
            Clean affiliation for USA before match.

        Returns
        -------
        query_geo : str
            USA states are replaced by 'United States' in each affiliations
        """
        
        query_geo = re.sub(' AL| AK| AZ| AR| CA| CO| CT| DE| DC| FL| GA| HI| ID',' United States',query_geo) 
        query_geo = re.sub(' IL| IN| IA| KS| KY| LA| ME| MD| MA| MI| MN| MS| MO',' United States',query_geo) 
        query_geo = re.sub(' MT| NE| NV| NH| NJ| NM| NY| NC| ND| OH| OK| OR| PA',' United States',query_geo) 
        query_geo = re.sub(' RI| SC| SD| TN| TX| UT| VT| VA| WA| WV| WI| WY',' United States',query_geo) 
        return query_geo
    
    def get_loc(self,query_geo):
        """
        Parameters
        ----------
        query : str
            author cleaned affiliation

        Returns
        -------
        loc_i : dict
            Match affiliation with the cities df and return the country for each author in a dict

        """
        try:
            try:
                output =[]
                for city in self.cities:
                    if str(city[1]) in str(query_geo):
                        output.append(city)
            except :
                output = None
        
            if output is not None and len(output) > 0:
                if output[0]==list(output[0]):
                    country = output[0][1]
                else:
                    country = output[1]
                loc_i = {"country":country}
                
                if loc_i is not None:
                    return loc_i
        except:
            print('NON')
        
    def get_query_by_doc(self,paper,aff_split,aut_split):
        """
        Update info for each authors in a paper

        Parameters
        ----------
        paper : dict
            a single document
        aff_split : str
            split pattern for affiliation .
        aut_split : str
            split pattern for authors.

        Returns
        -------
        authors_info_cities : dict
            a dict with all location info for each authors.
        n : int
            nb of authors.
        share : float
            share of authors in the paper with country location captured.
        inter_collab : Boolean
            binary, 1 if there is at least two different countries in the paper, 0 otherwise
        country : list
            list of countries present in affiliations.

        """
        authors = paper['authors'].split(aut_split)[1:]
        authors_info_cities = {}
        n=0
        j=0
        country = []
        
        for author in authors:
            
            n+=1
            query_country = self.clean_country(author,aff_split)
        
    
            try:
            
                loc_info_cities = self.get_loc(query_country)
                author_info_cities =  {'author'+str(n):{"name":self.get_name(author,aff_split),
                                                        'country':loc_info_cities['country']}}
                
                authors_info_cities.update(author_info_cities)
                country.append(loc_info_cities['country'])
                j+=1
            except :
                try:
                    query_country = self.clean_country(paper['author'][n]['affiliation'],'jesaiscestmoche')
                    loc_info_cities = self.get_loc(query_country)
                    author_info_cities =  {'author'+str(n):{"name":self.get_name(author,aff_split),
                                                        'country':loc_info_cities['country']}}
                    
                    authors_info_cities.update(author_info_cities)
                    country.append(loc_info_cities['country'])
                    
                    j+=1
                except:
                    author_info_cities =  {'author'+str(n):{"name":self.get_name(author,aff_split),
                                                            'country':None}}
                                                           
                    authors_info_cities.update(author_info_cities)
                    
        if len(set(country))>1:
            inter_collab = 1
        else:
            inter_collab = 0
        try: 
            share = j/n
        except:
            share = 0
        return authors_info_cities, n, share , inter_collab, country
    
    def is_covid(self,paper):
        """
        Is covid base on PMC europe query

        Parameters
        ----------
        paper : dict
            a single document.

        Returns
        -------
        covid_paper : Boolean
            binary, 1 if there is a match between lexic from PMC Europe and meshwords abstract or title, 0 otherwise.

        """
        text = str(paper['title']).lower() + str(paper['abstract']).lower() + str(paper['meshwords']).lower()
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
        return covid_paper
    
    def is_eu(self,countries):
        """
        Is in UE28 + Swiss
        

        Parameters
        ----------
        countries : list
            country list

        Returns
        -------
        is_eu : Boolean
             binary, 1 if any country in UE28, 0 otherwise..

        """
        countries = ' '.join(list(countries))
        eu_list = ['Germany','Belgium','France','Italy','Luxembourg',
                   'Netherlands','Denmark','Ireland','United Kingdom',
                   'Greece','Spain','Portugal','Austria','Finland','Sweden',
                   'Cyprus','Estonia','Hungary','Latvia','Lithuania','Malta',
                   'Poland','Czechia','Slovakia','Romania','Bulgaria','Croatia','Switzerland']
        is_eu = any(re.search(w,countries) for w in eu_list)
        if is_eu:
            is_eu = 1
        else:
            is_eu = 0
        return is_eu 
    
    
    
        
    def get_loc_list(self,paper):
        """
        Get loc list after all update

        Parameters
        ----------
        paper : dict
            a single document.

        Returns
        -------
        newvalues : dict
            new variables to a.

        """
        try:
            country = []
            for aut in paper['Location_cities_country']:
                if paper['Location_cities_country'][aut]['country'] is not None:
                    country.append(paper['Location_cities_country'][aut]['country'])
            country = list(set(country))
            newvalues = {
                "nb_country":len(country),
                "country_list": ';'.join(country),
                }
            return newvalues
        except:
            pass
           
           
        
    def is_to_update(self,paper):
        """
        Parameters
        ----------
        paper : dict
            A dict with the pubmed metadata for a given document

        Returns
        -------
        to_change : Boolean
            Check if we need to update the location with doi.org's information'

        """
        to_change = False
        if  len(paper['author'])==len(paper['Location_cities_country']):
            for aut in range(len(paper['Location_cities_country'])):
                if paper['Location_cities_country']['author'+str(aut+1)]['country'] is None and paper['author'][aut]['affiliation']!=[]:
                    to_change = True
                    paper['authors'] = re.sub(r'('+paper['Location_cities_country']['author'+str(aut+1)]['name']+'affil str)',
                           r'\1 '+str(paper['author'][aut]['affiliation'][0]['name']),
                           paper['authors'])
                   
        return to_change, paper
    
    def set_new_values(self,paper):
        """
        Set all new values

        Parameters
        ----------
        paper : dict
            A dict with the pubmed metadata for a given document

        Returns
        -------
        query : str
            id in mongodb.
        newvalues : dict
            new values to update in mongodb.

        """
        query = { "_id": paper['_id'] }
        covid_paper = self.is_covid(paper)
        loc_cities, team_size, share_captured, inter_collab, countries = self.get_query_by_doc(paper,
                     aff_split = 'affil str',
                     aut_split = 'name ml')
        eu_loc = self.is_eu(countries)
        newvalues = {
            "team_size":team_size,
            "Location_cities_country": loc_cities,
            "share_aff_captured": share_captured,
            "is_eu":eu_loc,
            "inter_collab":inter_collab,
            "is_coronavirus_lower": covid_paper
            }
        return query, newvalues
        
    
    def update_db(self,from_,to_):
        """
         Clean db and update 

        Parameters
        ----------
        from_ : int
            where to start in raw data.
        to_ : int 
            where to stop in raw data.

        Returns
        -------
        insert clean and updated data in a new db

        """
        
        df = self.collection.find({},no_cursor_timeout=True)
        pmid_clean = [i['pmid'] for i in self.collection_clean.find()]
        
        df = [paper for paper in tqdm.tqdm(df[from_:to_])]
        for paper in tqdm.tqdm(df):
            if paper['unix_received'] is not None and all([paper['pmid'] not in pmid_clean,paper['unix_received']> 1420070400]):
                try:
                    query, newvalues = self.set_new_values(paper)
                    if  paper['doi'] is not None: 
                        """
                        try:
                            infs = self.works.doi(paper['doi'])
                            infs = {str(key):infs[key] for key in ['is-referenced-by-count',
                                                                   'reference-count',
                                                                   'created',
                                                                   'author']}
                            newvalues.update(infs)
                            to_change, paper = self.is_to_update(paper)
                            if to_change:
                                query, newvalues2 = self.set_new_values(paper)
                                paper.update(newvalues2)
                        except:
                            print('No crossref info')
                        """
                        paper.update(newvalues)
                        paper.update(self.get_loc_list(paper))
                        self.collection_clean.insert_one(paper)
                        
                except Exception as e:
                    print("427",str(e))
        
            

    def restrict_medline(self):
        """
        Export a pmid list restricted on MEDLINE documents 
        """
        df = self.collection_clean.find()
        df_list = []
        for doc in tqdm.tqdm(df):
            try:
                if 'unix_received' in doc.keys() and doc['unix_received'] is not None and doc['unix_received']> 1420070400:
                    df_list.append(doc)
            except:
                pass
        df = pd.DataFrame(df_list)
        medline_issn = df[(pd.notnull(df['meshwords'])) & (df['meshwords'] != '')]['ISSN'].tolist()
        pmid_medline_list = df['pmid'][df['ISSN'].isin(medline_issn)].tolist()
        
        docs = self.collection_clean.find()
        for doc in tqdm.tqdm(docs):
            if doc["pmid"] not in pmid_medline_list:
                self.collection_clean.delete_one({'pmid': doc["pmid"]})
    
    def clean_doi(self):
        """
        remove duplicate 
        """

        docs = self.collection_clean.aggregate([
            { "$group": {
                "_id": "$pmid",
                "count": { "$sum": 1 }
            }},
            { "$match": {
                "count": { "$gt": 1 }
            }}
        ],allowDiskUse=True)
        
        
        for doc in docs:
            self.collection_clean.delete_one({'pmid': doc["_id"]})
    
    def remove_na(self):
        """
        Remove data without countries 
        """
        docs = self.collection_clean.find({"Location_cities_country":{"$exists":False}})
        for doc in docs:
            self.collection_clean.delete_one({'pmid': doc["pmid"]})
    

    def do_json(self):
        """
        Export clean data
        """
        df = self.collection_clean.find()
        df_list = []
        for doc in tqdm.tqdm(df):
            try:
                if 'unix_received' in doc.keys() and doc['unix_received'] is not None and doc['unix_received']> 1420070400:
                    doc.pop('_id')
                    df_list.append(doc)
            except:
                pass
        with open('../Data/data.json', 'w') as json_file:
            json.dump(df_list,json_file)
            
    def issn2categories(self, path_issn_files = "Data/ISSN"):
        # unique issn

        issn_list = []
        docs =  self.collection_clean.find()
        n = 0
        for doc in tqdm.tqdm(docs):
            issn_list.append(doc["ISSN"])
            n += 1
            if n % 100000 == 0:
                issn_list = list(set(issn_list))

        issn_list = list(set(issn_list))
        
        # assign directory
        directory = path_issn_files
         
        # iterate over files in
        # that directory
        df = pd.DataFrame()
        for filename in os.listdir(directory):
            f = os.path.join(directory, filename)
            # checking if it is a file
            if os.path.isfile(f):
               df = pd.concat([df,pd.read_csv(f)])
        issn2cat = {}
        for issn in issn_list:
            try:
                issn2cat[issn] = df[df["ISSN"] == issn]["Web of Science Categories"].values[0]
            except:
                pass

        docs = self.collection_clean.find({},no_cursor_timeout=True)
        
        for doc in tqdm.tqdm(docs):
            try:
                cat = issn2cat[doc["ISSN"]]
                self.collection_clean.update_one({'pmid':doc["pmid"]},{"$set":{'wos_cat':cat}},upsert=False)
            except:
                pass

####♥

def clean_prettify(txt):
    txt = re.sub(r'None'," ",txt)
    txt = re.sub(r'\n'," ",txt)
    txt = re.sub(r'\\n'," ",txt)
    txt = re.sub(r'\n      '," ",txt)
    txt = re.sub(r'\t'," ",txt)
    txt = re.sub(r'\t\t'," ",txt)
    txt = re.sub(r'\n\t\t'," ",txt)
    txt = re.sub(r'  +', ' ', txt)
    return(txt)

def date2unix(date):
    year = date.split(",")[0].split(" ")[-1]
    month = date.split(",")[1].split(" ")[-1]
    day = date.split(",")[2].split(" ")[-1]
    s = "/".join([day,month,year])
    
    unix = time.mktime(datetime.datetime.strptime(s, "%d/%m/%Y").timetuple())
    return unix

def get_next_braces(text,starting_pos = 0):
    text_split = text.split() 
    open_braces = 1
    closed_braces = 0
    i = starting_pos
    
    while open_braces != closed_braces:
        i += 1
        if re.search("{",text_split[i]):
            open_braces += 1
        elif re.search("}",text_split[i]):
            closed_braces += 1

    end_brace_pos = i
    return(end_brace_pos," ".join(text_split[starting_pos+1:end_brace_pos]))

def get_info(text,id_text):

    text = clean_prettify(text)
    try:
        abstract = re.search("abstract(.*)mesh",text).group(1)
    except:
        try:
            abstract = re.search("abstract(.*), pmid",text).group(1)
        except:
            abstract = None
    try:
        mesh = re.search("mesh(.*)",text).group(1)
        _,mesh = get_next_braces(mesh)
        mesh_words = re.sub('"',"","\n".join(re.findall("term ([^\},]+)",mesh))) #[^\},] match everythin except } and ,
        mesh_subwords = re.sub('"',"","\n".join(re.findall("subh ([^\},]+)",mesh)))
    except:
        mesh_words = None
        mesh_subwords = None

    try:
        source = re.search("from book(.*)",text).group(1)
        source = re.search("name([^\},]+)",source).group(1)
        source = "book:" + source
    except:
        try:
            source = re.search("from journal(.*)",text).group(1)
            source = re.search("name([^\},]+)",source).group(1)
            source = "journal:" + source
        except:
            source = None

    author_exist = True
    all_authors = []
    try:
        authors = re.search("names std(.*)",text).group(1)
        _, authors = get_next_braces(authors)
        last_pos, author = get_next_braces(authors)
        all_authors.append(author)
        while author_exist == True:
            try:
                last_pos, author = get_next_braces(authors,(last_pos+1))
                all_authors.append(author)
            except:
                author_exist = False
        all_authors = re.sub('"',"","\n".join(all_authors))
    except:
        try:
            authors = re.search("names ml(.*)",text).group(1)
            _, authors = get_next_braces(authors)
            all_authors = authors.split(",")
            all_authors = re.sub('"',"","\n".join(all_authors))
        except:
            all_authors = None
    
    try:
        date_received = re.search("pubstatus received(.*)",text).group(1)
        date_received = re.search("date std(.*)",date_received).group(1)
        _,date_received = get_next_braces(date_received)
    except:
       date_received = None
    
    try:
        unix_received = date2unix(date_received)
    except:
        unix_received = None
    
    try:
        date_accepted = re.search("pubstatus accepted(.*)",text).group(1)
        date_accepted = re.search("date std(.*)",date_accepted).group(1)
        _,date_accepted = get_next_braces(date_accepted)
    except:
        date_accepted = None  
    try:
        unix_accepted = date2unix(date_accepted)
    except:    
        unix_accepted = None
       
    try:
        date_medline = re.search("pubstatus medline(.*)",text).group(1)
        date_medline = re.search("date std(.*)",date_medline).group(1)
        _,date_medline = get_next_braces(date_medline) 
    except:
        date_medline = None
    try:
        unix_medline = date2unix(date_medline)
    except:   
        unix_medline = None  
    try:
        date = re.search("em std(.*)",text).group(1)
        _,date = get_next_braces(date)
        unix = date2unix(date)
    except Exception as e:
        print(str(e),id_text)
        try:
            date = re.search("date std(.*)",text).group(1)
            _,date = get_next_braces(date)
            unix = date2unix(date)
        except:
            date = None
            unix = None
    try:
        title = re.search("title(.*)",text).group(1)
        _,title = get_next_braces(title)
        title = re.sub("name ","",title)
        title = re.sub('"',"",title)
    except:
        title = None
    try:
        grant = re.search("idnum([^\}]+)",text).group(1)
        grant = re.sub("{","",grant)
    except:
        grant = None
        
    try:
        doi = re.search("doi(.*)",text).group(1)
        doi = doi.split('"')[1]
    except:
        doi = None
    try:
        ISSN = re.search("issn(.*)",text).group(1)
        ISSN = ISSN.split('"')[1]
    except:
        ISSN = None
    
    return(abstract,mesh_words,mesh_subwords,all_authors,date,date_received,\
           date_accepted,date_medline,title,source,grant,doi,ISSN,unix,unix_received,unix_accepted,unix_medline)