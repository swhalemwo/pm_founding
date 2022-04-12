from bs4 import BeautifulSoup
import os
import csv
import re
import pandas as pd
import pdb
from itertools import product

ARTNEWS_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/data/artnews/selenium2/"

OUTPUT_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/data/artnews/"


#main-wrapper > main > div.lrv-a-wrapper.lrv-a-grid.lrv-a-cols4\@tablet.lrv-u-padding-t-2.js-ProfileFilter > div > ul > li:nth-child(1)

def get_year_ranking(year, genre, page_nbr):
    """return the soup"""
    with open(ARTNEWS_DIR + 'ranking_' + genre + str(year) + "_page" + str(page_nbr) +'.html', 'r') as fi:
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
    """generic function for extracting information from profile using regex"""

    return_obj = []

    res = re.findall(regex, str(prfl), re.S)
    if res != []:
        return_obj = res[0]
        return_obj = re.sub(r"[\n\t]*", "", return_obj)

    return(return_obj)



# extract_info(prfl, "\t\t\t\t\t(.*?)\t\t\n\t")

    
# {k:extract_info(prfl, v) for k,v in regex_dict.items()}


def proc_year(year, genre):
    """processes a year lol"""
    # pdb.set_trace()
    print("year: ", year)
    print("genre: ", genre)
    
    ## inefficiently filtering the files
    files = [i for i in os.listdir(ARTNEWS_DIR) if str(year) in i and genre in i]
    
    genre_year_dfs = []

    # loop over the individual files
    for page_nbr in list(range(len(files))):
        print("page_nbr: ", page_nbr)
        ranking_soup = get_year_ranking(year, genre, page_nbr)
    
        ranking_infos = [extract_cltr_info(i, ranking_soup) for i in range(1,202)]
        empty_dict = ranking_infos[-1]
        ranking_infos2 = [i for i in ranking_infos if i != empty_dict]
        
        print("ranking_infos2: ", len(ranking_infos2))

        ranking_df = pd.DataFrame(ranking_infos2)
        ranking_df['year'] = year
        ranking_df['genre'] = genre
        
        genre_year_dfs.append(ranking_df)

    genre_year_df = pd.concat(genre_year_dfs)
    ranking_pd = genre_year_df.drop_duplicates()
    
    return(ranking_pd)
    
areas = ["asian-art", "old-masters", "impressionism-and-post-impressionism", "postwar-art", "modern-art", "contemporary-art"]


# proc_year(2020, "asian-art")
# for i in range(1990, 2022):


keys = ['genre', 'year']
cbns = [dict(zip(keys, combo)) for combo in product(areas, list(range(1990, 2022)))]


dfs_ranking = [proc_year(i['year'], i['genre']) for i in cbns]

df_rankcing_cbd = pd.concat(dfs_ranking)
df_rankcing_cbd.to_csv(OUTPUT_DIR + "ranking_genre.csv")
    

# for i in range(200):
#     print(extract_cltr_info(i))

    
