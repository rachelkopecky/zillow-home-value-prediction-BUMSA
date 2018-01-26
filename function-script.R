#---------------------------
  # Zillow Home Value Prediction
  # Function Building Script
  #
  # Due 11/01/2017 @ 11:55 pm
  #
  # Rachel Kopecky
  # 
#--------------------------- 

  # Load required libraries

library(Hmisc)
library(ZillowR)
library(XML)
library(tidyverse)
library(plyr)
library(dplyr)
library(MASS)
library(car)
library(knitr)

#---- 

  # Set ID
  
set_zillow_web_service_id("X1-ZWz1g32bxlzsp7_5xiff") # rkopecky@murraystate.edu

set_zillow_web_service_id("X1-ZWz1g29a78apsb_5886x") # rkopecky@bellarmine.edu

#---- 

  # comp_finder()

comp_finder <- function(address, zip1) {
    
    zip <- as.character(zip1)  
    
      # Retrieve info for property of interest
  
    init <- GetSearchResults(address, zip)
      
      # Extract the zpid
    
    zpidx <- unlist(init[3]$response[[1]][[1]][[1]][[1]])
    zpidx <- as.numeric(zpidx[[2]])
  
    myHome <- GetDeepComps(zpid = zpidx, 
                          count = 25)  
      
     # If there are no comps for property, return error message.
    
    if (myHome$message$text == "Error: comps not available for the specified property identifier" |
        myHome$message$text == "Error: no comps for the specified property were found") {
      
      print(paste0("Error: ",address, " ", zip, " does not have any comparable properties. Please enter an address with comparable properties."))
      
      break
      
      } else {
      
      propertiesx <- xmlToList(myHome$response[["properties"]])
      
        # z_extract()
      
      z_extract <<- function(properties) {
            
            zpid <- NULL
            taxAssess <- NULL
            year <- NULL
            lot <- NULL
            finish <- NULL
            bath <- NULL
            bed <- NULL
            total <- NULL
            zindexx <- NULL
            lastsold <- NULL
            
              # Loop through properties list to pull out relevant variables. Substitute NULL values with NA to hold the place.
            
            for (i in 1:25) {
             
              if (!is.null(properties[[2]][i]$comp$zpid)) {
                zpid[i] <- properties[[2]][i]$comp$zpid
              } else {
                zpid[i] <- "NA"
              }
              if (!is.null(properties[[2]][i]$comp$taxAssessment)) {
                taxAssess[i] <- properties[[2]][i]$comp$taxAssessment
              } else {
                taxAssess[i] <- "NA"
              }
              if (!is.null(properties[[2]][i]$comp$yearBuilt)) {
                year[i] <- properties[[2]][i]$comp$yearBuilt
              } else {
                year[i] <- "NA"
              }
              if (!is.null(properties[[2]][i]$comp$lotSizeSqFt)) {
                lot[i] <- properties[[2]][i]$comp$lotSizeSqFt
              } else {
                lot[i] <- "NA"
              }
              if (!is.null(properties[[2]][i]$comp$finishedSqFt)) {
                finish[i] <- properties[[2]][i]$comp$finishedSqFt
              } else {
                finish[i] <- "NA"
              }
              if (!is.null(properties[[2]][i]$comp$bathrooms)) {
                bath[i] <- properties[[2]][i]$comp$bathrooms
              } else {
                bath[i] <- "NA"
              }
              if (!is.null(properties[[2]][i]$comp$bedrooms)) {
                bed[i] <- properties[[2]][i]$comp$bedrooms
              } else {
                bed[i] <- "NA"
              }
              if (!is.null(properties[[2]][i]$comp$totalRooms)) {
                total[i] <- properties[[2]][i]$comp$totalRooms
              } else {
                total[i] <- "NA"
              }
              if (!is.null(properties[[2]][i]$comp$lastSoldPrice$text)) {
                lastsold[i] <-  properties[[2]][i]$comp$lastSoldPrice$text
              } else {
                lastsold[i] <- "NA"
              }
              
            }
  
            zestimate <- map(properties$comparables, c("zestimate", "amount","text"))
            zestimate[sapply(zestimate, is.null)] <- NA 
            zestimate <- data.frame(matrix(unlist(zestimate), byrow=T),stringsAsFactors=FALSE)
            if (nrow(zestimate) < 25) zestimate[(nrow(zestimate)+1):25,] <- "NA"
            if (nrow(zestimate) > 25) zestimate <- zestimate[1:25,]
            zestimate <- data.frame(zestimate)
            zindex <- map(properties$comparables, c("localRealEstate"))
            for (i in 1:25) {
              if (!is.null(zindex[i]$comp[[1]])) {
                   zindexx[i] <- zindex[i]$comp[[1]]
                 } else {
                   zindexx[i] <- "NA"
                 }
            }
            
            
            myData <- data.frame(zpid,
                                 taxAssess,
                                 year,
                                 lot,
                                 finish,
                                 bath,
                                 bed,
                                 total,
                                 zestimate,
                                 zindexx,
                                 lastsold)
            
            myData[,'zindexx'] <- gsub(",","", myData[,'zindexx'])
            
            colnames(myData) <- c("zpids", 
                                  "taxAssessment", 
                                  "yearBuilt", 
                                  "lotSizeSqFt", 
                                  "finishedSqFt", 
                                  "bathrooms", 
                                  "bedrooms", 
                                  "totalRooms",
                                  "zestimate",
                                  "zindexValue",
                                  "lastSoldPrice")
            
              # Convert all columns to numeric
            
            myData[, c(1:11)] <- sapply(myData[, c(1:11)], as.character)
            myData[, c(1:11)] <- sapply(myData[, c(1:11)], as.numeric)
    
              # Remove row if zpid = NA
            
            myData <- myData[!is.na(myData[,"zpids"]),]
      }       
    
      myData <- z_extract(propertiesx)
    
      # Initialize empty dataframe
    
      data <- data.frame()
    
      # Loop through 25 comps to obtain more comp info
    
      for (i in 1:nrow(myData)) {
          
        myHome1 <- GetDeepComps(zpid = myData[i,1], 
                                count = 25)
        
        if (myHome1$message$text == "Error: comps not available for the specified property identifier" |
            myHome1$message$text == "Error: no comps for the specified property were found") {
         
           next 
        
          } else {
        
          properties1 <- xmlToList(myHome1$response[["properties"]])
          
          x <- z_extract(properties1)
          
          data <- rbind(data, x)
        }
      }
      
      # Only keep rows with unique zpids
      
      myData2 <- data[!duplicated(data[,"zpids"]),]
      
      # Initialize second data frame
      
      data1 <- data.frame()
      
      # Loop through unique comps to obtain more comp info
      
      for (i in 1:nrow(myData2)) {
          
              myHome1 <- GetDeepComps(zpid = myData2[i,1], 
                                      count = 25)
              
              if (myHome1$message$text == "Error: comps not available for the specified property identifier" |
                  myHome1$message$text == "Error: no comps for the specified property were found") {
               
                 next
              
                } else {
              
                properties1 <- xmlToList(myHome1$response[["properties"]])
                
                x <- z_extract(properties1)
                
                data1 <- rbind(data1, x)
        }   
      }
      
      # Bind all dataframes for complete listing of comps
      
      myData4 <- rbind(myData, data, data1)
      
      # Only keep rows with unique zpids
      
      dfComp <<- myData4[!duplicated(myData4[,"zpids"]),]
      
      return(dfComp)
      
  }

}

comps <- comp_finder("1906 Brook Stone Ct", 40014)

write.csv(dfComp, "dfComp.csv")