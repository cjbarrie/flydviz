import pandas as pd
from pytrends.request import TrendReq 
import datetime as dt
import math
import numpy as np

pd.options.display.max_rows = 999

with open("data/output/translations.txt") as trans_file:
    translations = trans_file.read().splitlines()

with open("data/output/country_codes.txt") as cnts_file:
    countries = cnts_file.read().splitlines()
    
with open("data/output/languages.txt") as langs_file:
    languages = langs_file.read().splitlines()

trans_code_df = pd.DataFrame()
trans_code_df["trans"] = translations
trans_code_df["country"] = countries
trans_code_df["lang"] = languages
trans_code_df.head()

pytrends = TrendReq()

dates='2020-01-01 2020-08-18'
for idx, trans, country, lang in trans_code_df.itertuples():
    try:
        pytrends.build_payload(kw_list=[trans], timeframe=dates, geo=country)
        interest_over_time_df = pytrends.interest_over_time()
        interest_over_time_df["lang"] = lang
        interest_over_time_df.to_csv(f"data/output/exports/{country}{idx}_racism.csv")
        print(f"{country}{idx} was succesfully pulled from Google Trends")
    except Exception as e:
        print(f"{country}{idx} was not successfully pulled because of the following error:" + str(e))
        continue  