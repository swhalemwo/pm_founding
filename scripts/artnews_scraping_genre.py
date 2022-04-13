import pandas as pd
from selenium.webdriver import Firefox
import sys
from selenium import webdriver
from time import time, sleep
from selenium.webdriver.chrome.options import Options





# base_str = "https://www.artnews.com/art-collectors/top-200-profiles/?filter_top200year="
ARTNEWS_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/data/artnews/selenium2/"

base_str="https://www.artnews.com/art-collectors/top-200-profiles/?filter_top200year={year}&filter_collectingarea={area}&page=1"

areas = ["asian-art", "old-masters", "impressionism-and-post-impressionism", "postwar-art", "modern-art", "contemporary-art"]


def scroll_down_page(driver):

    xpath = "/html/body/div[3]/main/div[2]/div/div/nav/button/span[1]"

    more_button = driver.find_element_by_xpath(xpath)
    more_button.click()


def scrape_ranking(year, area):    
    driver.get(base_str.format(area=area, year=year))
    sleep(4)

    maxi=0
    for i in range(6):

        try:
            # save before scrolling down; saving should always work            
            with open(ARTNEWS_DIR + 'ranking_' + area + str(year) + "_page" + str(i) + '.html', 'w') as fo:
                fo.write(driver.page_source)

            scroll_down_page(driver)
        except:
            maxi=i
            break

        sleep(3)

        
    with open(ARTNEWS_DIR + 'ranking_' + area + str(year) + "_page" + str(maxi+1) + '.html', 'w') as fo:
        fo.write(driver.page_source)


    
driver = webdriver.Chrome('/home/johannes/tec/chromedriver2/chromedriver')

driver.get(base_str.format(year=1995, area="asian-art"))
sleep(3)
reject_button = driver.find_element_by_id("onetrust-reject-all-handler")
reject_button.click()

for year in range(1990, 2022):
    print(year)
    for area in areas:
        print(area)
        scrape_ranking(year=year, area=area)
