midYear <- function(date){
  # Transforms date into mid-year date for better aggregation
  year <- year(date)
  midYear <- paste(as.character(year),'-07-02',sep='')%>%
    as.Date()
  midYear
}
