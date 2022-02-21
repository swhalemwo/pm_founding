# * xml reading 


from bs4 import BeautifulSoup
import pandas as pd
from collections import Counter

import xmltodict
import pandas as pd
import json
import re

DEGRUYTER_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/data/degruyter/"


xml_file_name = DEGRUYTER_DIR + "mow/MOW2020_Output.xml"

with open(xml_file_name, 'r') as f:
    data = f.read()

bs_data = BeautifulSoup(data, 'xml')

bs_records = bs_data.find_all('directory_record')

# "directory_record" is always only key 
dict_records = [xmltodict.parse(str(i), dict_constructor = dict)['directory_record'] for i in bs_records]

rec_dict = dict_records[0]

# * extracting main info

def extract_info(rec_dict):
    idx = rec_dict['@id']
    inst_dict = rec_dict['institution']
    
    namex = inst_dict['inst_name_info']['inst_name']
    name_eng = ""
    if "inst_name_English" in inst_dict['inst_name_info'].keys():
        name_eng = inst_dict['inst_name_info']["inst_name_english"]
        
    ## if there are multiple addresses

    nbr_branches = 1

    city = country = ""
    nbr_branches = 1
    if "dir_address" in inst_dict.keys():
        if type(inst_dict['dir_address']) == type([]):
            ## find the main one
            addr_dict = [i for i in inst_dict['dir_address'] if i['@type']=="main"][0]
            nbr_branches = len(inst_dict['dir_address'])

        elif type(inst_dict['dir_address']) == type({}):
            addr_dict = inst_dict['dir_address']
        
            if "street_addr" in addr_dict.keys():
                if "place" in addr_dict["street_addr"].keys():
                    city = addr_dict['street_addr']['place']
        
        country = addr_dict['country_short']


    typex = []
    
    founding_date1 = "NA"
    founding_date2 = "NA"
    founding_date3 = "NA"
    founding_date4 = "NA"
        
    founding_date_str = "NA"
    
    if "additional_inst_info" in inst_dict.keys():
        if "type" in inst_dict["additional_inst_info"].keys():
            typex = inst_dict["additional_inst_info"]["type"]
            if type(typex) != type([]):
                typex = [typex]
                
            # print("type: ", typex)
        if "founding_date" in inst_dict["additional_inst_info"].keys():
            founding_date_str = inst_dict["additional_inst_info"]["founding_date"]
            founding_dates = re.findall(r'\d+', founding_date_str)

            if len(founding_dates) > 0:
                founding_date1 = int(founding_dates[0])

            if len(founding_dates) > 1:
                founding_date2 = int(founding_dates[1])

            if len(founding_dates) > 2:
                founding_date3 = int(founding_dates[2])
                
            if len(founding_dates) > 3:
                founding_date4 = int(founding_dates[3])
        
    subj_clsfcn= []
    if 'subject_classification' in rec_dict.keys():
        subj_clsfcn = rec_dict['subject_classification']["keyword"]
        ## use lists everywhere to allow easier processing
        if type(subj_clsfcn) != type([]):
            subj_clsfcn = [subj_clsfcn]

    nbr_staff = 0
    if "inst_staff" in inst_dict.keys():
        if type(inst_dict["inst_staff"]["person"]) == type([]):
            nbr_staff = len(inst_dict["inst_staff"]["person"])
        elif type(inst_dict["inst_staff"]["person"]) == type({}):
            nbr_staff = 1


    proc_dict = {"idx": idx,
                 "name": namex,
                 "name_eng": name_eng,
                 "type" : typex,
                 "clsfcn": subj_clsfcn,
                 "founding_date1" : founding_date1,
                 "founding_date2" : founding_date2,
                 "founding_date3" : founding_date3,
                 "founding_date4" : founding_date4,
                 "founding_date_str": founding_date_str,
                 "city": city,
                 "country": country,
                 "nbr_branches": nbr_branches,
                 "nbr_staff": nbr_staff
                 
                 }
    
    return proc_dict


proc_records = []
for i in dict_records:
    proc_record = extract_info(i)
    proc_records.append(proc_record)
df_proc = pd.DataFrame(proc_records)
df_proc

# * melting list columns

# ** classification
df_melt = df_proc[["idx", "clsfcn"]]
df_clsfcn = df_melt.clsfcn.apply(pd.Series) \
                         .merge(df_melt, right_index = True, left_index = True) \
                         .drop(["clsfcn"], axis = 1) \
                         .melt(id_vars = ['idx'], value_name = "clsfcn") \
                         .drop("variable", axis = 1) \
                         .dropna()

df_clsfcn.to_csv(DEGRUYTER_DIR + "mow/classification.csv", index = False)


# ** type

df_melt = df_proc[["idx", "type"]]
df_type = df_melt.type.apply(pd.Series) \
                          .merge(df_melt, right_index = True, left_index = True) \
                          .drop(["type"], axis = 1) \
                          .melt(id_vars = ['idx'], value_name = "type") \
                          .drop("variable", axis = 1) \
                          .dropna()

df_type.to_csv(DEGRUYTER_DIR + "mow/type.csv", index = False)


# * using csv for main table
df_main = df_proc[[i for i in list(df_proc.columns) if i not in ["type", "clsfcn"]]]
df_main.to_csv(DEGRUYTER_DIR + "mow/mow.csv", index = False)

# * using json 

df_proc[df_proc['founding_date1'] > 2]

df_proc_json = df_proc.to_json()

with open(DEGRUYTER_DIR + "mow/mow.json", 'w', encoding = "utf8") as fo:
    fo.write(df_proc_json)

# * preliminary explorations

# ** classification

# def flatten_list(t):
#     flat_list = [item for sublist in t for item in sublist]
#     return flat_list


# type_lists = []
# for i in list(df_proc['clsfcn']):
#     if type(i) == type([]):
#         type_lists.append(i)
#     else:
#         type_lists.append([i])

# Counter(flatten_list(type_lists))

# ** founding year

# ctr = Counter(df_proc['founding_date'])
# plt_years = list(range(1900, 2020))
# plt_cnts = []
# for i in plt_years:
#     plt_cnts.append(ctr[str(i)])
# import matplotlib.pyplot as plt
# plt.plot(plt_years, plt_cnts)
# plt.show()

# * scrap 

# from flatten_dict import flatten

# def flatten_record(record):
#     """transform the xml record to flat dict"""
#     record_dict = xmltodict.parse(str(record), dict_constructor = dict)
#     record_flat = flatten(record_dict['directory_record'], reducer = 'dot', enumerate_types=(list,))
    
#     return record_flat

# flatten_record(bs_records[10])

# flat_entries = [flatten_record(i) for i in bs_records[0:5000]]


# len(flat_entries)

# df = pd.DataFrame(flat_entries)
# df.columns
# df.columns.tolist()


# len(dict_records)
# len(bs_records)
# bs_record = bs_records[0]

# x_record = bs_records[0]



# dict1 = xmltodict.parse(str(x_record), dict_constructor = dict)



# dict1['directory_record']['subject_classification']['keyword']

# dict1.keys()

# d1_flat = flatten(dict1, reducer = 'underscore', enumerate_types=(list,))
# d1_flat.keys()
# d1_flat
# # this should work: can see which keys are everywhere

# flatten({'a': [1, 2, 3], 'b': 'c'}, enumerate_types=(list,), reducer = 'underscore')
# d1_flat = flatten(dict1, reducer = 'underscore', enumerate_types=(list,))








