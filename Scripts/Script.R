###############################################
#            Problem set 1                    #
#                                             #
# Colaboradores:                              #
# Daniel Raquira 201914059                    #
# Santiago Becerra 201911587                  #
#                                             #
###############################################

## 1 ##
#a.i.
rm(list=ls())
require(pacman)
p_load(tidyverse, rvest)
url1 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
html1 <- read_html(url1)
tabla1 <- html1 %>% html_elements(xpath = "//table") %>% html_table()

url2 <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html"
html2 <- read_html(url2)