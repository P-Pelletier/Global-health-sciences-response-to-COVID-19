#import argparse
from utils import Clean_infos

#parser = argparse.ArgumentParser(description='up db for covidpaper')

#parser.add_argument('-from_')
#parser.add_argument('-to_')
#args = parser.parse_args()

data = Clean_infos('pubmed_cleaned2019','pubmed','pubmed_cleaned')
#data.update_db(int(args.from_),int(args.to_))
data.update_db(int(0),int(6000000))
    

    

