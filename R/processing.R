library(jsonlite)
library(dplyr)
library(leaflet)
library(httr)
library(exploratory)

# beatlesInfluence <- read.csv("data/beatlesInfluences.csv", stringsAsFactors = FALSE)
# 
# beatlesInfluenceEntityCodes <- fetchAllArticleEntities(beatlesInfluence)
# beatlesInfluenceEntityValues <- fetchAllEntityValues(linksWithInstanceOf)
# secondaryCategories <- fetchAllEntityValues(categories)
# performerValues <- fetchAllEntityValues(performers)

#' Fetch All Articles Entites (that I care about)
#' 
#' @description This function will iterate over a dataframe, calling fetchArticleEntities() function.
#' It returns only a subset of the data that is there. Additional work is needed to get the actual
#' values of the entity in some cases. For dates you will get the actual value, for classes you will
#' get an ID that needs to be used to look up the actual value.
fetchAllArticleEntities <- function(beatlesInfluence){
  allArticleEntities <- NULL
  for(i in 1:nrow(beatlesInfluence)){
    cat(beatlesInfluence$articleTitle[i], paste0(round(i/nrow(beatlesInfluence)*100, 2), "%"), "\n")
    currentId <- data.frame(id = beatlesInfluence$articleId[i], stringsAsFactors = FALSE)
    
    tryCatch({
      articleEntities <- fetchArticleEntities(currentId$id)
      allArticleEntities <- rbind(allArticleEntities, articleEntities)
    }, error = function(e){
      currentId$InstanctOf <- NA
      linksWithInstanceOf <- rbind(allArticleEntities, articleEntities)
    })
    rm(currentId)
  }
  return(allArticleEntities)
}

#' Fetch Article Entities
#' 
#' @description This function is used to fetch one articles entities as seen when looking at WikiData. Example here:
#' https://www.wikidata.org/wiki/Q1299.
fetchArticleEntities <- function(articleId){
  url <- paste0("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",articleId,"&format=json&languages=en")
  request <- httr::GET(url)
  data <- httr::content(request)
  statements <- data$entities[[1]]$claims
  
  tryCatch({
    instanceOfId <- checkvalueExists(statements$P31[[1]]$mainsnak$datavalue$value$id)
    subclassId <- checkvalueExists(statements$P279[[1]]$mainsnak$datavalue$value$id)
    inceptionTime <- checkvalueExists(statements$P571[[1]]$mainsnak$datavalue$value$time)
    dateOfBirth <- checkvalueExists(statements$P569[[1]]$mainsnak$datavalue$value$time)
    performer <- checkvalueExists(statements$P175[[1]]$mainsnak$datavalue$value$id)
    publicationDate <- checkvalueExists(statements$P577[[1]]$mainsnak$datavalue$value$time)
  
    entities <- data.frame(cbind(articleId, instanceOfId, subclassId, inceptionTime, dateOfBirth, performer, publicationDate))
    
  }, error = function(e){
    instanceOfId <- NA
    subclassId <- NA
    inceptionTime <- NA
    dateOfBirth <- NA
    performer <- NA
    publicationDate <- NA
    entities <- data.frame(cbind(articleId, instanceOfId, subclassId, inceptionTime, dateOfBirth, performer, publicationDate))
  })
  
  return(entities)
}


fetchAllEntityValues <- function(categories){
  categoriesWithValues <- NULL
  for(i in 1:nrow(categories)){
    cat(categories$id[i], paste0(round(i/nrow(categories)*100, 2), "%"), "\n")
    currentId <- data.frame(id = categories$id[i], stringsAsFactors = FALSE)
    
    tryCatch({
      currentId$categoryLookup <- fetchEntityValue(currentId$id)
      categoriesWithValues <- rbind(categoriesWithValues, currentId)
    }, error = function(e){
      currentId$categoryLookup <- NA
      categoriesWithValues <- rbind(categoriesWithValues, currentId)
    })
    rm(currentId)
  }
  return(categoriesWithValues)
}

fetchEntityValue <- function(wikiDataId){
  url <- paste0("https://www.wikidata.org/w/api.php?action=wbgetentities&ids=",wikiDataId,"&format=json&props=labels&languages=en")
  
  request <- httr::GET(url)
  
  data <- httr::content(request)
  
  tryCatch({
    
    instanceValue <- data$entities[[1]]$labels$en$value
    
    if(is.null(instanceValue)){
      instanceValue <- NA
    }
  }, error = function(e){
    instanceValue <- NA
  })
  
  return(instanceValue)
}

checkvalueExists <- function(value){
  if(is.null(value)){
    value <- NA
  } else {
    value <- value
  }
  return(value)
}

