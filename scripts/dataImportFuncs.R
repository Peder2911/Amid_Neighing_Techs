# Various functions to handle data gathering, etc.

# Util function to fix data type of dates
# Add other data type fixes here if needed
# Called on both file import and on API response
fixDates<-function(gedSet){
  gedSet$date_start<-as.Date(gedSet$date_start)
  gedSet$date_end<-as.Date(gedSet$date_end)
  gedSet
}

# Returns a df from an API request
# If there is no data, the request returns NULL. This is handled @ downloadGed
# see also gedScraper.py
apiRequest<-function(name,startDate='1950-01-01',endDate=format(Sys.time(),'%Y-%m-%d')){
  response = NULL
  
  ret<-system2('./gedScraper.py',name,startDate,endDate)
  
  if (ret == 0){
    response <- read.csv('./data/reqOut.csv')
  } else {
    response <- NULL
  }
  response
}

# Returns a list of responses from the API, based on 
downloadGed<-function(names,startDate,endDate){
  dfs<-list()
  i <- 1
  
  for(name in names){
    response <- apiRequest(name,startDate,endDate)
    
    if(length(response)!=0){
    dfs[[i]]<-apiRequest(name,startDate,endDate)%>%
      fixDates()
    i<-i+1
    } else {
      print(paste('Zero events for',name))
    }
    
  }
  
  print(paste('Number of countries pulled:',length(dfs)))
  dfs
}

# Either reads ged from file, or downloads ged if file is not present.
# Should also implement caching (storage) of downloaded subsets to avoid redownloading data?

getGed<-function(names,startDate,endDate){
  
  print(startDate)
  
  if('ged181.csv' %in% list.files('./data')){
    print('Using file')
    ged<-read.csv('./data/ged181.csv',stringsAsFactors = FALSE)%>%
      fixDates()%>%
      subsetByVector(names,startDate,endDate)
    
  } else {
    # Only getting plots from middle request?
    # Do a simpler script to test requests.
    # Do not implement directly. (DNID)
    print('Using API')
    ged<-downloadGed(names,startDate,endDate)
  }
  
  # Return
  ged
}