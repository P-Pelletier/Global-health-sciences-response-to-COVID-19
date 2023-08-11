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

    def pub_info(self, paper, date):
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
            if len(temp_list_country) == 1:
                self.n_publication_add[date].at[loc, "solo_author"] += 1 
            if len(set(temp_list_country))>1:
                self.n_publication_add[date].at[loc, "collabPubs"] += 1  
            else:
                self.n_publication_add[date].at[loc, "solePubs"] += 1 
            

        for loc in temp_list_country:
            if len(set(temp_list_country))>1:
                self.n_publication_add[date].at[loc, "collabPubs_full_count"] += len([i for i in temp_list_country if i != loc])
            else:
                self.n_publication_add[date].at[loc, "solePubs_full_count"] += len(temp_list_country)
        
        return(temp_list_country)
    
    def grant_info_depecrated(self, paper, date, temp_list_country):
        if paper["grants"]:
            if type(paper["grants"]) == dict:
                grants = [paper["grants"]]
            else:
                grants = paper["grants"]    
            for country_funded in list(set(temp_list_country)):
                self.n_publication_add[date].at[country_funded, "N_paper_with_grant"] += 1
                
            countries_funding = []
            for grant in grants:
                countries_funding.append(grant["Country"])
            
            countries_funding = [country for country in countries_funding if country != None]
            
            if countries_funding:
            # Only one country funding                      
                if len(set(countries_funding)) == 1:
                    country_funder = list(set(countries_funding))[0]
                    if len(set(temp_list_country)) == 1:
                        country_funded = list(set(temp_list_country))[0]
                        if country_funder == country_funded:
                            #print("N_funding_solo",paper["pmid"])
                            self.n_publication_add[date].at[country_funded, "N_funding_solo"] += 1
                        elif country_funder == "International":
                            self.n_publication_add[date].at[country_funded, "IAG_funding_solo"] += 1
                            #print("IAG_funding_solo",paper["pmid"])
                        else:
                            self.n_publication_add[date].at[country_funded, "I_funding_solo"] += 1
                            #print("I_funding_solo",paper["pmid"])
                    else:
                        for country_funded in list(set(temp_list_country)):
                            if country_funder == country_funded:
                                #print("N_funding_collab",paper["pmid"])
                                self.n_publication_add[date].at[country_funded, "N_funding_collab"] += 1
                            elif country_funder == "International":
                                #print("IAG_funding_collab",paper["pmid"])
                                self.n_publication_add[date].at[country_funded, "IAG_funding_collab"] += 1
                            else:
                                #print("I_funding_collab",paper["pmid"])
                                self.n_publication_add[date].at[country_funded, "I_funding_collab"] += 1                               
                # Multiple country funding
                else:
                    countries_funder = list(set(countries_funding))
                    if len(set(temp_list_country)) == 1:
                        country_funded = list(set(temp_list_country))[0]
                        if country_funded in countries_funder:
                            if "International" in countries_funder and len(countries_funder) > 2:
                                #print("N_I_IAG_funding_solo",paper["pmid"])
                                self.n_publication_add[date].at[country_funded, "N_I_IAG_funding_solo"] += 1
                            elif "International" in countries_funder:
                                #print("N_IAG_funding_solo",paper["pmid"])
                                self.n_publication_add[date].at[country_funded, "N_IAG_funding_solo"] += 1
                            else:
                                #print("N_I_funding_solo",paper["pmid"])
                                self.n_publication_add[date].at[country_funded, "N_I_funding_solo"] += 1 
                        else:
                            if "International" in countries_funder and len(countries_funder) >= 2:
                                #print("I_IAG_funding_solo",paper["pmid"])
                                self.n_publication_add[date].at[country_funded, "I_IAG_funding_solo"] += 1
                            elif "International" in countries_funder:
                                #print("IAG_funding_solo",paper["pmid"])
                                self.n_publication_add[date].at[country_funded, "IAG_funding_solo"] += 1                    
                            else:
                                #print("I_funding_solo",paper["pmid"])
                                self.n_publication_add[date].at[country_funded, "I_funding_solo"] += 1 
                    else:
                        for country_funded in list(set(temp_list_country)):
                            if country_funded in countries_funder:
                                if "International" in countries_funder and len(countries_funder) > 2:
                                    #print("N_I_IAG_funding_collab",paper["pmid"])
                                    self.n_publication_add[date].at[country_funded, "N_I_IAG_funding_collab"] += 1
                                elif "International" in countries_funder:
                                    #print("N_IAG_funding_collab",paper["pmid"])
                                    self.n_publication_add[date].at[country_funded, "N_IAG_funding_collab"] += 1
                                else:
                                    #print("N_I_funding_collab",paper["pmid"])
                                    self.n_publication_add[date].at[country_funded, "N_I_funding_collab"] += 1 
                            else:
                                if "International" in countries_funder and len(countries_funder) >= 2:
                                    #print("I_IAG_funding_collab",paper["pmid"])
                                    self.n_publication_add[date].at[country_funded, "I_IAG_funding_collab"] += 1
                                elif "International" in countries_funder:
                                    #print("IAG_funding_collab",paper["pmid"])
                                    self.n_publication_add[date].at[country_funded, "IAG_funding_collab"] += 1                    
                                else:
                                    #print("I_funding_collab",paper["pmid"])
                                    self.n_publication_add[date].at[country_funded, "I_funding_collab"] += 1         
            else:
                for country_funded in list(set(temp_list_country)):
                    self.n_publication_add[date].at[country_funded, "N_paper_with_no_grant"] += 1
                            
                
    def grant_info_funder(self, paper, date, temp_list_country):
        if paper["grants"]:
            if type(paper["grants"]) == dict:
                grants = [paper["grants"]]
            else:
                grants = paper["grants"]    
                
            countries_funding = []
            for grant in grants:
                countries_funding.append(grant["Country"])
            
            countries_funding = [country for country in countries_funding if country != None]
            
            if countries_funding:
            # Only one country funding                      
                if len(set(countries_funding)) == 1:
                    country_funder = list(set(countries_funding))[0]
                    if len(set(temp_list_country)) == 1:
                        country_funded = list(set(temp_list_country))[0]
                        if country_funder == country_funded:
                            #print("N_funding_solo",paper["pmid"])
                            self.n_publication_add[date].at[country_funder, "N_funding_solo"] += 1
                            self.n_publication_add[date].at[country_funder, "Total_funding"] += 1
                    else:
                        for country_funded in list(set(temp_list_country)):
                            if country_funder == country_funded:
                                #print("N_funding_collab",paper["pmid"])
                                self.n_publication_add[date].at[country_funder, "N_funding_collab"] += 1
                                self.n_publication_add[date].at[country_funder, "Total_funding"] += 1
                # Multiple country funding
                else:
                    countries_funder = list(set(countries_funding))
                    for country_funder in countries_funder:
                        if len(set(temp_list_country)) == 1:
                            country_funded = list(set(temp_list_country))[0]
                            if country_funded == country_funder:
                                if "International" in countries_funder and len(countries_funder) > 2:
                                    #print("N_I_IAG_funding_solo",paper["pmid"])
                                    self.n_publication_add[date].at[country_funder, "N_I_IAG_funding_solo"] += 1
                                    self.n_publication_add[date].at[country_funder, "Total_funding"] += 1
                                elif "International" in countries_funder:
                                    #print("N_IAG_funding_solo",paper["pmid"])
                                    self.n_publication_add[date].at[country_funder, "N_IAG_funding_solo"] += 1
                                    self.n_publication_add[date].at[country_funder, "Total_funding"] += 1
                                else:
                                    #print("N_I_funding_solo",paper["pmid"])
                                    self.n_publication_add[date].at[country_funder, "N_I_funding_solo"] += 1 
                                    self.n_publication_add[date].at[country_funder, "Total_funding"] += 1
                        else:
                            countries_funded = list(set(temp_list_country))
                            if country_funder in countries_funded:
                                if "International" in countries_funder and len(countries_funder) > 2:
                                    #print("N_I_IAG_funding_collab",paper["pmid"])
                                    self.n_publication_add[date].at[country_funder, "N_I_IAG_funding_collab"] += 1
                                    self.n_publication_add[date].at[country_funder, "Total_funding"] += 1
                                elif "International" in countries_funder:
                                    #print("N_IAG_funding_collab",paper["pmid"])
                                    self.n_publication_add[date].at[country_funder, "N_IAG_funding_collab"] += 1
                                    self.n_publication_add[date].at[country_funder, "Total_funding"] += 1
                                else:
                                    #print("N_I_funding_collab",paper["pmid"])
                                    self.n_publication_add[date].at[country_funder, "N_I_funding_collab"] += 1
                                    self.n_publication_add[date].at[country_funder, "Total_funding"] += 1
                                          
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

        df_add = pd.DataFrame(np.zeros((len(self.city_country_list), 21)))
        df_add.index = self.city_country_list
    
        df_add.columns = ["solePubs", "solePubs_full_count","solo_author","collabPubs","collabPubs_full_count",
                          "N_funding_solo", "IAG_funding_solo", "I_funding_solo",
                          "N_funding_collab", "IAG_funding_collab", "I_funding_collab",
                          "N_I_IAG_funding_solo", "N_IAG_funding_solo","N_I_funding_solo",
                          "I_IAG_funding_solo", "N_I_IAG_funding_collab", "N_IAG_funding_collab","N_I_funding_collab",
                          "I_IAG_funding_collab","N_paper_with_no_grant","N_paper_with_grant"]
    
        #df_add.columns = ["solePubs", "solePubs_full_count","solo_author","collabPubs","collabPubs_full_count",
        #                  "N_funding_solo", "N_funding_collab", "N_I_IAG_funding_solo", "N_IAG_funding_solo",
        #                  "N_I_funding_solo", "N_I_IAG_funding_collab", "N_IAG_funding_collab","N_I_funding_collab",
        #                  "Total_funding"]       
        self.n_publication_add = {key: df_add.copy() for key in self.time_period}        

        for paper in tqdm.tqdm(self.data):
            date = self.get_unix(paper)
            if int(date) <= self.last_date and int(date) >= self.start_date:
                # get list of country for each author 
                temp_list_country = self.pub_info(paper,date)
                self.grant_info_depecrated(paper,date,temp_list_country)
