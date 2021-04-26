from utils import Clean_infos

data = Clean_infos('pubmed','pubmed_2019','pubmed_2019_cleaned')

data.collection_clean.create_index([ ("pmid",1) ])
data.restrict_medline()
data.clean_doi()
data.remove_na()
