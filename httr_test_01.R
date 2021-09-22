library(httr)
library(rvest)
library(dplyr)
library(purrr)

# Get the HTML document from Wikipedia using httr
wikipedia_response <- GET('https://en.wikipedia.org/wiki/Varigotti')
# Parse the response into an HTML doc
wikipedia_page <- content(wikipedia_response, type = 'text/html')
# Check the status code of the response
status_code(wikipedia_response)

# Extract the elevation with XPATH
wikipedia_page %>% 
  html_nodes('table tr:nth-child(9) > td') %>% 
  html_text()


TestPage <- GET("https://en.wikipedia.org/w/index.php?title=Mount_Everest&oldid=958643874")
TestPageContent <- content(TestPage)

TestPageContent %>% html_node('h1') %>% html_text()

TestPageContent %>% html_nodes('h1') %>% html_text()

TestPageContent %>% html_nodes('#firstHeading') %>% html_text()

coords <- TestPageContent %>% 
  html_node('#coordinates .geo-dms') %>% html_text()

TestPageContent %>% html_nodes(xpath = '//h1') %>% html_text()

TestPageContent %>% html_nodes(xpath = '//*[@id = "firstHeading"]') %>% html_text()

peak <- TestPageContent %>% 
  html_nodes(xpath = '//*[@id = "firstHeading"]') %>% html_text()

coords <- TestPageContent %>% 
  html_node(xpath = '//*[@id = "coordinates"]//*[@class = "geo-dms"]') %>% html_text()

