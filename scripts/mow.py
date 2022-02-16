# * xml reading 


from bs4 import BeautifulSoup
import pandas as pd
from collections import Counter



with open(xml_file_name, 'r') as f:
    data = f.read()

bs_data = BeautifulSoup(data, 'xml')

bs_records = bs_data.find_all('directory_record')
len(bs_records)
bs_record = bs_records[0]

x_record = bs_records[0]

import xmltodict

dict1 = xmltodict.parse(str(x_record), dict_constructor = dict)

from flatten_dict import flatten

dict1['directory_record']['subject_classification']['keyword']

dict1.keys()

def underscore_reducer(k1, k2):
    if k1 is None:
        return k2
    else:
        return k1 + "_" + k2

d1_flat = flatten(dict1, reducer = 'underscore', enumerate_types=(list,))
d1_flat.keys()
d1_flat
# this should work: can see which keys are everywhere

flatten({'a': [1, 2, 3], 'b': 'c'}, enumerate_types=(list,), reducer = 'underscore')
d1_flat = flatten(dict1, reducer = 'underscore', enumerate_types=(list,))


def flatten_record(record):
    record_dict = xmltodict.parse(str(record), dict_constructor = dict)
    record_flat = flatten(record_dict, reducer = 'underscore', enumerate_types=(list,))
    return record_flat

flatten_record(bs_records[10])

flat_entries = [flatten_record(i) for i in bs_records]
len(flat_entries)

df = pd.DataFrame(flat_entries)

df.columns
df.columns.tolist()

columns:
- directory_record_institution_additional_inst_info_founding_date




