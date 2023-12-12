
from bs4 import BeautifulSoup
import os
import csv
import re
import pandas as pd
import pdb

ARTNEWS_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/data/artnews/selenium/"

# OUTPUT_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/data/artnews/"
OUTPUT_DIR = "/home/johannes/Dropbox/phd/pmdata/data_sources/artnews/"



#main-wrapper > main > div.lrv-a-wrapper.lrv-a-grid.lrv-a-cols4\@tablet.lrv-u-padding-t-2.js-ProfileFilter > div > ul > li:nth-child(1)

def get_year_ranking(year):
    with open(ARTNEWS_DIR + 'ranking_' + str(year) +'.html', 'r') as fi:
        ranking_html = fi.read()

    ranking_soup = BeautifulSoup(ranking_html, 'html.parser')
    
    return(ranking_soup)


def extract_cltr_info(nbr, ranking_soup):

    # pdb.set_trace()
    
    prfl_xpath = '#main-wrapper > main > div.lrv-a-wrapper.lrv-a-grid.lrv-a-cols4\@tablet.lrv-u-padding-t-2.js-ProfileFilter > div > ul > li:nth-child({nbr})'.format(nbr=nbr)


    prfl = ranking_soup.select(selector = prfl_xpath)
    # prfl_text = prfl[0].contents[1]

    # 'u-color-red lrv-u-padding-tb-050">'

    
    # clctr_name = re.findall('<img alt="(.*?)"', str(prfl))
    regex_dict = {
        'clctr_name' :"\t\t\t\t\t(.*?)\t\t\n\t",
        'location' :'u-color-red lrv-u-padding-tb-050">(.*?)</p>',
        'collection_focus' : 'u-background-color-red:before a-icon-float-left">(.*?)</p>',
        'industry' : 'lrv-u-font-family-primary lrv-u-display-block">(.*?)</p>'}

    
    # clctr_name = re.findall("\t\t\t\t\t(.*?)\t\t\n\t", str(prfl))
    # location = re.findall('u-color-red lrv-u-padding-tb-050">(.*?)</p>', str(prfl))
    # collection_focus = re.findall('u-background-color-red:before a-icon-float-left">(.*?)</p>', str(prfl), re.S)
    # industry = re.findall('lrv-u-font-family-primary lrv-u-display-block">(.*?)</p>', str(prfl))
    data_dict = {k:extract_info(prfl, v) for k,v in regex_dict.items()}

    # pdb.set_trace()

    ## replace non-breaking spaces
    data_dict['clctr_name'] = data_dict['clctr_name'].replace(u'\xa0', u' ')

    return(data_dict)


def get_nbr_of_clctrs(ranking_soup):
    """get number of collectors (some years only have 199)"""

    collector_list = ranking_soup.select("#main-wrapper > main > div.lrv-a-wrapper.lrv-a-grid.lrv-a-cols4\@tablet.lrv-u-padding-t-2.js-ProfileFilter > div > ul")

    cl2 = collector_list.pop()
    nbr_clctrs = len(cl2.findAll('li'))

    return(nbr_clctrs)




# main-wrapper > main > div.lrv-a-wrapper.lrv-a-grid.lrv-a-cols4\@tablet.lrv-u-padding-t-2.js-ProfileFilter > div > ul


# main-wrapper > main > div.lrv-a-wrapper.lrv-a-grid.lrv-a-cols4\@tablet.lrv-u-padding-t-2.js-ProfileFilter > div > ul


def extract_info(prfl, regex):

    return_obj = ""

    res = re.findall(regex, str(prfl), re.S)
    if res != []:
        return_obj = res[0]
        return_obj = re.sub(r"[\n\t]*", "", return_obj)

    return(return_obj)



# extract_info(prfl, "\t\t\t\t\t(.*?)\t\t\n\t")

    
# {k:extract_info(prfl, v) for k,v in regex_dict.items()}


def proc_year(year):
    print(year)
    ranking_soup = get_year_ranking(year)
    
    ranking_infos = [extract_cltr_info(i, ranking_soup) for i in range(1,202)]
    empty_dict = ranking_infos[-1]
    ranking_infos2 = [i for i in ranking_infos if i != empty_dict]
    ranking_pd = pd.DataFrame(ranking_infos2)
    ranking_pd['year'] = year
    
    return(ranking_pd)

# year = 1990
    


# for i in range(1990, 2022):
if __name__ == "__main__":

    ## get all the data, combine it
    dfs_ranking = [proc_year(i) for i in range(1990, 2022)]
    df_ranking_cbd = pd.concat(dfs_ranking)

    ## generate ID for collector
    names = df_ranking_cbd['clctr_name'].unique().tolist()
    ids = ["ACE" + str(i) for i in range(1, len(names)+1)]
    maps = {k:v for k,v in zip(names, ids)}
    df_ranking_cbd['id'] = df_ranking_cbd['clctr_name'].map(maps)

    
    df_ranking_cbd.to_csv(OUTPUT_DIR + "an_ranking_time.csv")
    

# for i in range(200):
#     print(extract_cltr_info(i))

    
