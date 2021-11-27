import requests
import json

from urllib.request import Request

import pandas as pd
from collections import Counter


url = 'http://stats.oecd.org/sdmx-json/data/QNA/AUS+AUT.GDP+B1_GE.CUR+VOBARSA.Q/all?startTime=2009-Q2&endTime=2011-Q4&dimensionAtObservation=allDimensions'

x = requests.Request(url)

x = requests.get(url, verify=False)
# just don't verify to not get SSL errors: https://stackoverflow.com/questions/10667960/python-requests-throwing-sslerror
res = json.loads(x.text)



"http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/<dataset identifier>"

# AUS+AUT.GDP+B1_GE.CUR+VOBARSA.Q

url = 'http://stats.oecd.org/sdmx-json/data/STANI4_2020/AUT+AUS.PROD.D90T92/all?startTime=1985&endTime=2020&dimensionAtObservation=allDimensions'

url = 'http://stats.oecd.org/sdmx-json/data/STANI4_2020/all.PROD.D90T92/all?startTime=1985&endTime=2020&dimensionAtObservation=allDimensions'
# doesn't seem possible to get all countries

x = requests.get(url, verify=False)
res = json.loads(x.text)


res_proc = [{'idx': idx, 'value': value[0]} for idx, value in res['dataSets'][0]['observations'].items()]

pd.DataFrame(res_proc)


# maybe should try to get entire DB, and then filter further, also atm not clear for which of the overall indicators there is data on creative industries

url_all = 'http://stats.oecd.org/sdmx-json/data/STANI4_2020/all/all?startTime=1985&endTime=1985&dimensionAtObservation=allDimensions'
# try only one year for now

x = requests.get(url_all, verify=False)
res = json.loads(x.text)

# oh fuck what about currencies? are included fortunately in attributes

res['structure'].keys()

res['structure']['links'] # irrelevant
res['structure']['name']  # irrelevant
res['structure']['description'] # irrelevant
res['structure']['dimensions']['observation'][0] # country keys, but no link to number, just three-letter country code
res['structure']['dimensions']['observation'][1] # table name, e.g. PROD
res['structure']['dimensions']['observation'][2] # industry?
res['structure']['dimensions']['observation'][3] # time period

# converter dict for id needs to be in string format

c = 0
converter_dict_id = {}
for i in res['structure']['dimensions']['observation']:
    converter_dict_id[c] = {}
    c2 = 0
    for k in i['values']:
        converter_dict_id[c][str(c2)] = k['id'] 
        c2+=1
    c+=1


res['structure']['attributes'] # some weird stuff with all kinds of information, e.g. origin of data, also something about currencies? 
# the actual values lists have a bunch of other values (5 actually) -> are they the attributes? 
res['dataSets'][0]['observations']['20:10:10:0']

len(res['structure']['attributes']['observation']) # 5 attributes, seems to fit to the number of attributes that value lists have

res['structure']['attributes']['observation'][0] # time format, always annual 
res['structure']['attributes']['observation'][1] # observation status
res['structure']['attributes']['observation'][2] # currency 
res['structure']['attributes']['observation'][2]['values'][3] # euro
res['structure']['attributes']['observation'][3] # multiplier, either millions or thousands, presumably value * 10^x 
res['structure']['attributes']['observation'][4] # reference period, seems to be always 2015? 

# the different possible values that each attribute can have are specified as lists -> order information there
# should be possible to convert all attribute information in one go
# dict seems best format?


c = 0
converter_dict = {}
for i in res['structure']['attributes']['observation']:
    converter_dict[c] = {}
    c2 = 0
    for k in i['values']:
        converter_dict[c][c2] = k['id']
        c2 +=1

    converter_dict[c][None] = None

    c+=1
    
    # should have option to manually specify which attribute columns I want converted,
    # think that's doable in my processing list comprehension tho
    # especially for the annotations, but actually the codes for them are just single letters, just not clear what their meaning is
    # but can look that up later when debugging

    
res_proc = [{
    # 'idx': idx, 
    'country': converter_dict_id[0][idx.split(":")[0]],
    'variable_table': converter_dict_id[1][idx.split(":")[1]],
    'industry': converter_dict_id[2][idx.split(":")[2]],
             'time_period': converter_dict_id[3][idx.split(":")[3]],
    'value': value[0],
    'time_format': converter_dict[0][value[1]],
    'observation_status': converter_dict[1][value[2]],
    'currency': converter_dict[2][value[3]],
    'multiplier': converter_dict[3][value[4]],
    'reference_period': converter_dict[4][value[5]]
}
            for idx, value in res['dataSets'][0]['observations'].items()]


df = pd.DataFrame(res_proc)
    

df_ccs = df[df['industry'] == 'D90T92']
Counter(df_ccs['country'])










