rm(ls())
require(rvest)
require(dplyr)

url_home <- 'https://catalogue.data.gov.bc.ca'
url_sub <- '/dataset?sort=metadata_modified+desc&q='
search_text = 'grade+12'

#read HTML code from the data bc website
webpage <- read_html(paste(url_home, url_sub, search_text, sep = ""))

#get page urls from first page
pages <- html_nodes(webpage,'.pagination-centered a') %>% html_text()
min = min(as.numeric(pages), na.rm=TRUE)
max = max(as.numeric(pages), na.rm=TRUE)
x = seq(min, max, by = 1)
page_urls <- paste(url_home, url_sub, search_text, "&page=", x, sep = "")

#function scrapes a url (x) and returns names of datasets on each page
get_ds_titles <- function(x){
  read_html(x) %>%
    html_nodes('.dataset-heading a') %>%
    html_text()
}

#get dataset titles/names
max_p <- 1
l <- lapply(page_urls[1:max_p], get_ds_titles)
titles <- unlist(l)

#search for title
search_text <- '"BC 2012-13 Second Quarterly Report - Forecast Update, Table 1.1"'

#read HTML code from the data bc website
webpage <- read_html(paste(url_home, url_sub, search_text, sep = ""))





