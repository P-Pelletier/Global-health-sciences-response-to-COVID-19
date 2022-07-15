import re
import tqdm
import numpy as np
import pandas as pd
from datetime import datetime
from collections import defaultdict
import collections
from dateutil.relativedelta import relativedelta
import pickle
from os.path import exists

class Create_net:
    
    def __init__(self,collection, query, last_date, start_date, lag = 0, unix_type = "unix_received"):
        
        '''

        Parameters
        ----------
        collection : str
            mongo collection name.
        query : str
            The filter for your collection. Used to differentiate corona and non corona papers
        last_date : int or string
            The end point of the analysis Format YYYYMM (202005 in our case) 
        lag: int
            Number of month to lag the unix, simulate the starting of the collaboration, we dont use lag
        unix_type: str
            The name of the key in the collection for the unix you will use (received = submission time) 
        Returns
        -------
        Dicts of publication per month

        '''

        self.collection = collection
        self.query = query
        self.start_date = int(start_date)
        self.last_date = int(last_date)
        self.lag = lag
        self.unix_type = unix_type
    
    def start_gen(self):
        '''
        Parameters
        ----------

        Returns
        -------
        generator of docs from the collection with the query filter
        '''
        
        self.data = self.collection.find(self.query)
        
    def clean_loc(self,loc):
        '''
        Parameters
        ----------
        loc : str
            The name of location (cities,countries)
        Returns
        -------
        str cleaned
        '''
        
        loc = re.sub("'","",loc)
        loc = re.sub("-","",loc)
        return loc

    
    def get_unix(self, paper):
        '''
        Parameters
        ----------
        paper : dict
            a single document
        Returns
        -------
        lagged unix
        '''     
        
        unix = paper[self.unix_type]
        date = self.date_lag(unix)
        return date
    
    def date_lag(self,unix):
        '''
        Parameters
        ----------
        unix : int
            Unix field of a doc
        Returns
        -------
        Transform the unix of the paper to format YYYYMM and adding lag
        '''
        
        date = datetime.utcfromtimestamp(unix)
        date = date + relativedelta(months =+ self.lag)
        month = str(date.month)
        year = str(date.year)
        if len(month) == 1:
            month = "0"+month
        return(year+month)
        
    
    def create_list_city(self,scale = "city"):
        '''
        Parameters
        ----------
        scale : str
            either "city" or "country" => level of analysis (country = international, cities = Inter+intra national)
        Returns
        -------
        list of cities found in self.collection
        '''
        
        file1_exists = exists("Data/city_country_list.p")
        file2_exists = exists("Data/time_period.p")
        self.scale = scale

        if file1_exists and file2_exists:
            self.city_country_list = pickle.load( open( "Data/city_country_list.p", "rb" ) )
            self.time_period = pickle.load( open( "Data/time_period.p", "rb" ) )
        else:
            data = self.collection.find()
            city_country_list = []
            time_period = []
            
            for paper in tqdm.tqdm(data):
                date = self.get_unix(paper)
                if int(date) <= self.last_date and int(date) >= self.start_date:
                    if date not in time_period:
                        time_period.append(date)
                    try:
                        if self.scale == "city":
                            for author in paper["Location_cities"]:
                                city = paper["Location_cities"][author]["city"]
                                country = paper["Location_cities"][author]["country"]
                                if country in ["country","Eswatini","Kosovo","Micronesia"]:
                                    continue      
                                if city and country != None:                  
                                    loc = self.clean_loc(city + "_" + country)
                                    if loc not in city_country_list:
                                        city_country_list.append(loc)
                        else:
                            for author in paper["Location_cities_country"]:
                                country = paper["Location_cities_country"][author]["country"]
                                if country in ["country","Eswatini","Kosovo","Micronesia"]:
                                    continue      
                                if country != None:
                                    loc = self.clean_loc(country)
                                    if loc not in city_country_list:
                                        city_country_list.append(loc)
                    except Exception as e:
                        print(str(e))
            
            time_period.sort()
            self.city_country_list = city_country_list
            self.time_period = time_period
            pickle.dump( self.time_period, open( "Data/time_period.p", "wb" ) )
            pickle.dump( self.city_country_list, open( "Data/city_country_list.p", "wb" ) )

    def populate_publication_dict(self):
        '''
        Parameters
        ----------

        Returns
        -------
        Different of publication by country for each month and type of research (type of research specified in query)
        '''
        
        self.start_gen()
        df = pd.DataFrame(np.zeros((len(self.city_country_list), 1)))
        df.index = self.city_country_list
        df.columns = ["n_pub"]
        self.n_publication = {key: df.copy() for key in self.time_period}

        df_add = pd.DataFrame(np.zeros((len(self.city_country_list), 2)))
        df_add.index = self.city_country_list
        df_add.columns = ["solePubs","collabPubs"]
        self.n_publication_add = {key: df_add.copy() for key in self.time_period}        

        for paper in tqdm.tqdm(self.data):
            date = self.get_unix(paper)
            if int(date) <= self.last_date and int(date) >= self.start_date:
                # get list of country for each author 
                temp_list_country = []
                if self.scale == "city":
                    for author in paper["Location_cities"]:
                        city = paper["Location_cities"][author]["city"]
                        country = paper["Location_cities"][author]["country"]
                        if country in ["country","Eswatini","Kosovo","Micronesia"]:
                            continue
                        if city and country != None:
                            loc = self.clean_loc(city + "_" + country)
                            temp_list_country.append(loc)
                else:
                    for author in paper["Location_cities_country"]:
                        country = paper["Location_cities_country"][author]["country"]
                        if country in ["country","Eswatini","Kosovo","Micronesia"]:
                            continue                        
                        if country != None:
                            loc = self.clean_loc(country)
                            temp_list_country.append(loc)
                
                for loc in set(temp_list_country):
                    self.n_publication[date].at[loc, "n_pub"] += 1       

                for loc in set(temp_list_country):
                    if len(set(temp_list_country))>1:
                        self.n_publication_add[date].at[loc, "collabPubs"] += 1  
                    else:
                        self.n_publication_add[date].at[loc, "solePubs"] += 1 
        
    def populate_publication_dict_full_count(self):
        '''
        Parameters
        ----------

        Returns
        -------
        Different of publication by country for each month and type of research (type of research specified in query)
        '''
        
        self.start_gen()
        self.n_publication_full_count = pd.DataFrame(np.zeros((len(self.city_country_list), 4)))
        self.n_publication_full_count.index = self.city_country_list
        self.n_publication_full_count.columns = ["pmid","country","n_author","year"]
        
        for paper in tqdm.tqdm(self.data):
            date = self.get_unix(paper)
            if int(date) <= self.last_date and int(date) >= self.start_date:
                # get list of country for each author 
                temp_list_country = []
                if self.scale == "city":
                    for author in paper["Location_cities"]:
                        city = paper["Location_cities"][author]["city"]
                        country = paper["Location_cities"][author]["country"]
                        if country in ["country","Eswatini","Kosovo","Micronesia"]:
                            continue
                        if city and country != None:
                            loc = self.clean_loc(city + "_" + country)
                            temp_list_country.append(loc)
                else:
                    for author in paper["Location_cities_country"]:
                        country = paper["Location_cities_country"][author]["country"]
                        if country in ["country","Eswatini","Kosovo","Micronesia"]:
                            continue                        
                        if country != None:
                            loc = self.clean_loc(country)
                            temp_list_country.append(loc)
                
                counter=collections.Counter(temp_list_country)
                for country in counter:
                    name = country
                    freq = counter[country]    
                    self.n_publication_full_count.append([paper["pmid"],name,freq,date])

