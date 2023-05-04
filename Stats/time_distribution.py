import tqdm
import pandas as pd
import numpy as np
import pymongo
from Stats.utils import Create_net
import tikzplotlib
import pycountry
import matplotlib.patches as mpatches
import matplotlib.font_manager as font_manager
import os
from matplotlib.lines import Line2D
from datetime import datetime
import datetime as dt 
import matplotlib.pyplot as plt
from collections import defaultdict
import plotly.express as px
from plotly.offline import plot

countries = {}
for country in pycountry.countries:
    countries[country.name] = country.alpha_2

client = pymongo.MongoClient("mongodb://localhost:27017")
mydb = client["pubmed"]
collection = mydb["pubmed_2015_cleaned"]
