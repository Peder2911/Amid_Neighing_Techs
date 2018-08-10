# outputs a plot of deaths
# functions found in funcs.r
require(tidyverse)
require(lubridate)

source('scripts/dataMungingFuncs.r')
source('scripts/dataImportFuncs.r')

###########################
# TEMP for testing

names<-c('Colombia')
scales <- c('years','months','weeks','days')

startDate<-'1989-01-01'

endDate<- Sys.time()%>%
  as.character()%>%
  str_split(' ')%>%
  unlist()%>%
  .[1]
  

# TEMP for testing
###########################
# Get subset criteria
source('textInteract.R')

# Get the data, either from file (if it is present as './data/ged171.csv'), or from API (might be time consuming)
#TODO implement caching
ged<-getGed(names,startDate,endDate)

# Make list of timelines by list of subsets
timelines<-lapply(ged,getTimeline,startDate,endDate)

# Make list of lists of plots.
# Number of plots is determined by the scale vector (eg. what is requested)
plots<-lapply(timelines,plotsByScaleVector,scale=scales)%>%
  unlist(recursive = FALSE)

# Unnest plots from subsets
plots<-unlist(plots,recursive = FALSE)

# Save plots with filenames generated from plot subtitle
savePlots(plots)
