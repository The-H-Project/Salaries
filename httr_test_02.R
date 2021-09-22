library(rvest)
library(dplyr)

# working directories
outputdirectory <- 'd:/data/'


# SeeThroughNY link
STNY_Link <- 'https://www.seethroughny.net/payrolls/201205824'
STNY_Robots <- 'https://www.seethroughny.net/robots.txt'

STNY_Page <- read_html(STNY_Link)

STNY_Robot_Page <- read_html(STNY_Robots)

write_html(STNY_Robot_Page, paste0(outputdirectory, 'test2.html'))

STNY_Class_List <- STNY_Page %>% html_nodes('*') %>% html_attr('class') %>% unique()

nodeset <- STNY_Page %>% html_nodes('*')

# Stack Overflow
# https://stackoverflow.com/questions/55327525/how-to-scrape-a-website-that-has-a-difficult-table-to-read-pandas-beautiful-s

STNY_Page %>% html_nodes('#selected-value') %>% html_text()

STNY_Page %>% html_nodes('#panel-title') %>% html_text()
