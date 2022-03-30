
from bs4 import BeautifulSoup
import os
import csv
import re
import pandas as pd

ARTNEWS_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/data/artnews/selenium/"

OUTPUT_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/data/artnews/"


#main-wrapper > main > div.lrv-a-wrapper.lrv-a-grid.lrv-a-cols4\@tablet.lrv-u-padding-t-2.js-ProfileFilter > div > ul > li:nth-child(1)

def get_year_ranking(year):
    with open(ARTNEWS_DIR + 'ranking_' + str(year) +'.html', 'r') as fi:
        ranking_html = fi.read()

    ranking_soup = BeautifulSoup(ranking_html, 'html.parser')
    
    return(ranking_soup)


def extract_cltr_info(nbr, ranking_soup):
    
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

    return(data_dict)


def extract_info(prfl, regex):

    return_obj = []

    res = re.findall(regex, str(prfl), re.S)
    if res != []:
        return_obj = res[0]
        return_obj = re.sub(r"[\n\t]*", "", return_obj)

    return(return_obj)



# extract_info(prfl, "\t\t\t\t\t(.*?)\t\t\n\t")

    
# {k:extract_info(prfl, v) for k,v in regex_dict.items()}


def proc_year(year):
    ranking_soup = get_year_ranking(year)
    
    ranking_infos = [extract_cltr_info(i, ranking_soup) for i in range(1,201)]
    ranking_pd = pd.DataFrame(ranking_infos)
    ranking_pd['year'] = year
    
    return(ranking_pd)
    

# for i in range(1990, 2022):
    
dfs_ranking = [proc_year(i) for i in range(1990, 2022)]

df_rankcing_cbd = pd.concat(dfs_ranking)
df_rankcing_cbd.to_csv(OUTPUT_DIR + "ranking.csv")
    

# for i in range(200):
#     print(extract_cltr_info(i))

    
