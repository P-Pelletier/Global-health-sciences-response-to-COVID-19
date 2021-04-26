# Global health sciences response to COVID-19

This folder hosts the code, data and results for the paper "Global health science leverages established collaboration network to fight COVID-19" (Müller et al., 2021).

## Data

pubmed_data.rar contains the cleaned data used for the analysis. You need to insert the json file in a MongoDB to run the scripts. The DB name used is "pubmed" and the collection name "cleaned". The session is currently set to localhost on port 27017. Please change parameters if needed.

The json contains the strict minimum to run our analysis, but the Pubmed API gives you more information. If you wish to download the data, you can use the script in the folder "Downloads". First run "pubmed_api.py" followed by "Clean_db.py". 

## Run the analysis

First run: Stats/get_stats.py and Network/create_network.py
This will give you the files "country_pub_info.csv" and "edge_list.csv" in the folder Data, as well as the tex file used for Fig1 A-C

Second run: Stats/3d plot/netwadaptation.r 
This produce the Fig1 D using "country_pub_info.csv" and "edge_list.csv" to compute the kernel regression.
