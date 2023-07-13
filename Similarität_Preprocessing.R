calculateSimilarity <- function(dataframe, column, dist = "lv", threshold = 0.35, min_match = 1, export = F, file = "") {
  # Import package
  library(stringdist)
  
  # Main Function
  similarity <- function(string1, string2, min_match) {
    string1 <- tolower(string1)
    string2 <- tolower(string2)
    
    len1 <- nchar(string1) #length
    len2 <- nchar(string2)
    
    # identical? 
    if(string1 == string2) {
      return(1)
    }
    # empty?
    else if(len1 == 0 | len2 == 0) {
      return(0)
    }
    # How similar are they?
    else {
      # get Levenshtein Distance
      similarity <- stringdist::stringdistmatrix(string1, string2, method = dist)
      
      # Return similarity normalized by string length
      if (len1 >= len2) {
        return(1 - similarity / len1)
      } else {
        return(1 - similarity / len2)
      }
    }
  }
  
  # New Column
  dataframe$Similarity <- NA
  
  for(i in 1:(nrow(dataframe) - 1)) {
    dataframe$Similarity[i] <- similarity(dataframe[[column]][i], dataframe[[column]][i+1])
  }
  dataframe$Similarity[nrow(dataframe)] <- 0
  
  dataframe$Threshold <- NA
  
  for(i in 1:(nrow(dataframe))){
    if(dataframe$Similarity[i] >= threshold){
      dataframe$Threshold[i] <- 1
    }
    else{
      dataframe$Threshold[i] <- 0
    }
    
  }

  # mark Context above and below the rows
  
  dataframe$Context_Above <- NA
  
  for(i in 2:(nrow(dataframe))) {
    if(dataframe$Similarity[i] >= threshold){
      dataframe$Context_Above[i-1] <- 1
    }
    else{
      dataframe$Context_Above[i-1] <- 0
    }
    dataframe$Context_Below[nrow(dataframe)] <- 1
    dataframe$Context_Above[1] <- 1
  }
  
  dataframe$Context_Below <- NA
  
  for(i in 1:(nrow(dataframe)-1)) {
    if(dataframe$Similarity[i] >= threshold){
      dataframe$Context_Below[i+1] <- 1
    }
    else{
      dataframe$Context_Below[i+1] <- 0
    }
    dataframe$Context_Below[nrow(dataframe)] <- 1
    dataframe$Context_Below[1] <- 1
  }
  
  dataframe$SimSum <- NA
  
  for(i in 1:(nrow(dataframe))) {
    dataframe$SimSum[i] <- sum(dataframe$Context_Above[i], dataframe$Context_Below[i], dataframe$Threshold[i])
  }
  
  # get rid of "helper columns"
  dataframe$Context_Above <- NULL
  dataframe$Context_Below <- NULL
  dataframe$Threshold <- NULL
  # saves df as csv optionally, file = path
  if(export == T){
    write.csv2(dataframe, file)
  }
    
  # Return dataframe
  return(dataframe)
}

