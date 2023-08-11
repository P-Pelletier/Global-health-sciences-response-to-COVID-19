# Global health sciences response to COVID-19

This folder hosts the code, data and results for the paper "Global health science leverages established collaboration network to fight COVID-19" (Müller et al., 2021).

## Data

For simplicity of use you can find data_covid_paper.rar at https://zenodo.org/record/5011448#.Ykb2PDU68mA which contains the cleaned data used for the analysis. However we do not own the data and The following 6 data sources have been used.


1. Publication data

source: PubMed API  (download 26.06.2023). You can view the process in the Download folder

Output: country_pub_info.csv and edge_list.csv

You can view the process in the Download folder
1_Pubmed_api.py will download every paper starting at the PMID 30M. If you want to download the whole database change line 127 to "inf_dl(collection,start_at = None)". 
2_Clean_db.py will currently clean the 5M papers change the line 12 if you did not start at 30M.
The rest of the scripts are used to create the csv for figures/stats and regressions.


2. Covid cases (download 26.06.2023)

file: owid-covid-data.csv

website: https://github.com/owid/covid-19-data/tree/master/public/data

original source (for some variables): COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University


3. Covid policy restrictions (last download 21.06.2021)

website: https://ourworldindata.org/policy-responses-covid

data file: OxCGRT_latest.csv

Oxford Covid-19 Government Response Tracker 


cit: Thomas Hale, Noam Angrist, Rafael Goldszmidt, Beatriz Kira, Anna Petherick, Toby Phillips, Samuel Webster, Emily Cameron-Blake, Laura Hallas, Saptarshi Majumdar, and Helen Tatlow. (2021). “A global panel database of pandemic policies (Oxford COVID-19 Government Response Tracker).” Nature Human Behaviour. https://doi.org/10.1038/s41562-021-01079-8



4. Economic wealth

Penn World Table version 10.0

cit: Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015), "The Next Generation of the Penn World Table" American Economic Review, 105(10), 3150-3182, available for download at www.ggdc.net/pwt


5. Economic/social development 

Human Development Index (HDI) from http://hdr.undp.org/en/content/download-data


## Run the analysis

Each folder has its own role. If you are interested in the Figures/Regression or Statistics you can just refer to the folder with the name.