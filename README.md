# Global health sciences response to COVID-19

This folder hosts the code, data and results for the paper "Global health science leverages established collaboration network to fight COVID-19" (Müller et al., 2021).

## Data

pubmed__2019_cleaned.rar contains the cleaned data used for the analysis. You need to insert the json file in a MongoDB to run the scripts. The DB name used is "pubmed" and the collection name "pubmed_2019_cleaned". The session is currently set to localhost on port 27017. Please change parameters if needed.

The json contains the strict minimum to run our analysis, but the Pubmed API gives you more information. If you wish to download the data, you can use the script in the folder "Downloads". First run "Downloads/pubmed_api.py" followed by "Downloads/Clean_db.py".  
Pubmed_api.py will download every paper starting at the PMID 30M. If you want to download the whole database change line 127 to "inf_dl(collection,start_at = None)". 
Clean_db.py will currently clean the 5M papers change the line 12 if you did not start at 30M.

## Run the analysis

First run: Stats/get_stats.py and create_network.py which will give the data for the rest of the scripts.

Second run: Stats/netwadaptation.r 
This produce the Fig1 D using "country_pub_info.csv" and "edge_list.csv" to compute the kernel regression.

Third run: Stats/Fig1.R
This will give the Fig1 of the paper

You can finish by running Stats/Tab1_RegressionCoronaPapers.R and Network/Fig2_networks.R