import pandas as pd
from selenium.webdriver import Firefox
import sys
from selenium import webdriver
from time import time, sleep
from selenium.webdriver.chrome.options import Options





base_str = "https://www.artnews.com/art-collectors/top-200-profiles/?filter_top200year="
ARTNEWS_DIR = "/home/johannes/Dropbox/phd/papers/org_pop/data/artnews/selenium/"


def scroll_down_page(driver):

    xpath = "/html/body/div[3]/main/div[2]/div/div/nav/button/span[1]"

    more_button = driver.find_element_by_xpath(xpath)
    more_button.click()


def scrape_ranking(year):    
    driver.get(base_str+str(year))
    sleep(4)

    for i in range(6):
        try:
            scroll_down_page(driver)
        except:
            pass
        
        sleep(3)
        
    with open(ARTNEWS_DIR + 'ranking_' + str(year) + '.html', 'w') as fo:
        fo.write(driver.page_source)


    
driver = webdriver.Chrome('/home/johannes/tec/chromedriver2/chromedriver')

driver.get(base_str+'1995')
sleep(3)
reject_button = driver.find_element_by_id("onetrust-reject-all-handler")
reject_button.click()

for year in range(1990, 2022):
    print(year)
    scrape_ranking(year)
