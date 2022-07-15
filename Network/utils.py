import itertools
import tqdm
import pandas as pd
import numpy as np
import re
import networkx as nx
from datetime import datetime
from dateutil.relativedelta import relativedelta





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
        Dicts of adjacency matrix per month and per type of research

        '''
        self.collection = collection
        self.query = query
        self.last_date = int(last_date)
        self.start_date = int(start_date)        
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
                
        data = self.collection.find()
        self.scale = scale
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


    def init_network(self):
        '''
        Parameters
        ----------

        Returns
        -------
        dict with empty pd.DataFrame rows = col = name countries for each month
        '''
        df = pd.DataFrame(np.zeros((len(self.city_country_list), len(self.city_country_list))))
        df.columns = self.city_country_list
        df.index = self.city_country_list

        
        self.network = {key: df.copy() for key in self.time_period}
    
    def populate_network(self):
        '''
        Parameters
        ----------

        Returns
        -------
        the dict with adjacency matrix for each month
        '''
        self.start_gen()
        self.init_network()
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
                
                # get the coocurence and the unique
                cooc = set(list(itertools.permutations(temp_list_country, 2)))
                
                #Add a collab in the network at the right date between countries
                for i in cooc:
                    self.network[date].at[i[0], i[1]] += 1

