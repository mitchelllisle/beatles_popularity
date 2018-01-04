library(jsonlite)
library(dplyr)
library(leaflet)
library(httr)
library(exploratory)

beatlesInfluence <- read.csv("data/beatlesInfluences.csv")

# links <- getBeatlesPages()
# linksWithId <- fetchAllWikiDataIds(links)

#### FUNCTIONS
getBeatlesPages <- function(){
  allLinks <- NULL
  
  pages <- c("https://en.wikipedia.org/w/index.php?title=Special%3AWhatLinksHere&limit=5000&target=The+Beatles&namespace=0",
             "https://en.wikipedia.org/w/index.php?title=Special:WhatLinksHere/The_Beatles&namespace=0&limit=5000&from=4819043&back=0", 
             "https://en.wikipedia.org/w/index.php?title=Special:WhatLinksHere/The_Beatles&namespace=0&limit=5000&from=39646592&back=4819043")
  
  for(i in 1:length(pages)){
    cat("Scraping Page ", i, "\n")
    linkData <- scrapeBeatlesLinkData(pages[i]) %>% tidyr::gather() %>% select(value) %>% filter(value %nin% c('edit', 'links'))
    allLinks <- rbind(allLinks, linkData) 
  }
  return(allLinks)
}

scrapeBeatlesLinkData <- function(page){
  args <- toJSON(list(args = list(url = unbox(page),
                                  selectors = unbox("ul#mw-whatlinkshere-list a"))))
  
  request <- httr::POST("http://localhost:3000/r/termatico/web_scraper", body = args)
  
  data <- httr::content(request)
  
  return(data.frame(data$data$data))
}

fetchAllWikiDataIds <- function(links){
  linksWithId <- NULL
  for(i in 1:nrow(links)){
    cat(links$value[i], paste0(round(i/nrow(links)*100, 2), "%"), "\n")
    currentLink <- data.frame(link = links$value[i], stringsAsFactors = FALSE)
    
    tryCatch({
      currentLink$wikiDataID <- fetchWikipediaIDs(currentLink$link)
      linksWithId <- rbind(linksWithId, currentLink)
    }, error = function(e){
      currentLink$wikiDataID <- NA
      linksWithId <- rbind(linksWithId, currentLink)
    })
    rm(currentLink)
  }
  return(linksWithId)
}

fetchWikipediaIDs <- function(title){
  url <- paste0("https://en.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&redirects=1&titles=", URLencode(title), "&format=json")
  
  request <- httr::GET(url)
  
  data <- httr::content(request)
    
  return(data$query$pages[[1]]$pageprops$wikibase_item)
}
