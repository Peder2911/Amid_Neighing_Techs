library(tidyverse)
library(lubridate)
library(magrittr)

# Preprocessing ---------------------------------------------------------------

# Generates a random vector with N length, and a sum of M
# Very useful for allocating events!
# Snipped from github.
rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}

# Takes a data set with date-start and date-end, expands rows with duration (eg. different start and end) into separate rows
# allocates events randomly throughout constitutive dates.
# Called in getTimeline
dateExpand<-function(df){
  
  df$rowname <- seq_len(nrow(df)) # make rownames for later
  df$diff <- df$date_end-df$date_start # make diff variable
  
  df <- transform(df[rep(1:nrow(df),df$diff+1),]) # expand data set where there is diff, diff no. of times
  # It repeats diff no. of times per row #
  
  df$consec<-sequence(rle(df[,'rowname'])$lengths)-1 # Counts number of consecutive occurences of same row name (repeated)
  
  df$date<-df$date_start+df$consec # Date is start date plus repetition (consec)
  
  df<-transform(df,totDeaths = deaths_a+deaths_b+deaths_civilians+deaths_unknown)
  
  
  # This generates a random distribution vector for each "zero-day" row with length = duration of event.
  # The distribution has the total number of deaths during the event period as its sum.
  # TODO add death substats (eg. deaths_a_distrib) etc.
  df$deathsDistrib<-apply(df[which(df$consec == 0),],1,function(x){
                                                 n <- as.numeric(gsub('[^0-9]','',x['diff']))+1
                                                 sum <- as.numeric(x['totDeaths'])
                                                 vec<-rand_vect(n,sum)
                                                 })%>%
    unlist() # Remember to do this.
  
  df
  
}

# Subsets the ged dataframe by ged$country with strings from a vector
# subsets<-subsetByVector(ged,c('Colombia','Norway','France'))
subsetByVector<-function(df,vector,startDate,endDate){
  
  if (startDate == ''){
    startDate<-'1950-01-01'
  }
  
  if(endDate == ''){
    endDate<-'2020-12-31'
  }
  
  startDate<-as.Date(startDate,format = '%Y-%m-%d')
  endDate<-as.Date(endDate,format = '%Y-%m-%d')
  
  dfs <- list()
  index<-1
  for(name in vector){
    subset<-df%>%
      filter(country == name)
    if(nrow(subset) > 0){
      dfs[[index]]<-subset
      index<-index+1
      
    } else {
      print(paste('Subset has zero rows: ',name))
    }
  }
  dfs<-lapply(dfs,function(df){
    df<-df%>%
      filter(date_start >= startDate & date_end <= endDate)}) # The lapply is new
}

# Transforms an event dataset into an event timeline, using dateExpand to convert events with duration into constitutive dates
# tls<-lapply(subsets,getTimeline)
getTimeline<-function(df,start,end){
  
  start = as.Date(start,'%Y-%m-%d')
  end = as.Date(end,'%Y-%m-%d')
  
  dfExp<-dateExpand(df)
  
  dfSum<-dfExp%>%
    group_by(date)%>%
    summarise(deaths = sum(deathsDistrib),
              country = max(as.character(country)), # Get the country name
              dyad = max(as.character(dyad_name)),
              dyad_id = max(dyad_new_id)) # Get the dyad name
  
  timeRange<-data.frame(date=seq(start,
                                 end,
                                 by='days')) # scale='days'?
  

  dfOut<-merge(dfSum,timeRange,by='date',all.y=TRUE)%>%
    arrange(date)
  
  dfOut<-separate(dfOut,'date',into=c('year','month','day'),remove = FALSE)
  
  dfOut$week<-week(dfOut$date)
  
  # Missing after merge = 0
  dfOut$deaths<-ifelse(is.na(dfOut$deaths),
         0,
         dfOut$deaths)
  dfOut$country<-ifelse(is.na(dfOut$country),
                        max(dfOut$country,na.rm = T),
                        dfOut$country)
  dfOut
  
  
}

# Explodes a timeline summary into individual event where each data point accounts for a single occurrence of the event count.
# Useful for vizualising with geom_freqpoly and other stat_bin based geoms
explodeTimeline <- function(tl,countVar,dropCountVar = TRUE){
  
  types <- sapply(tl,class)
  
  explodeRow <- function(row,countVar){
    row <- as.list(row)
    nCount <- row[[countVar]]%>%
      as.numeric()
      
    nCount <- nCount -1
    
    product <- as.tibble(row)
    if(nCount > 0){
      for(n in 1:nCount){
        product <- rbind(product,row)
      }
      product
    } else {
      product
    }
  }
  
  tl_out <- apply(tl,1,explodeRow,countVar)%>%
    bind_rows()%>%
    copyTypes(tl)%>%
    filter(deaths > 0)
  if(dropCountVar){
    tl_out <- tl_out[, !names(tl_out) == countVar]
  }
}

# Function to modify the data type of columns in a data frame.
# Data type to apply is specified with a character vector (e.g c('character','character','numeric'))
setTypes <- function(df,typeVec){
  expressions <- sapply(typeVec,function(x){
    paste('as.',x,sep='')%>%
      parse(text = .)
  })
  
  i = 1
  for(e in expressions){
    df[[i]] <- eval(e)(df[[i]])
    i <- i + 1 
  }
  df
}

# Copies data types from one data frame to another.
copyTypes <- function(to,from){
  types <- sapply(from,class)
  to <- setTypes(to,types)
  to
}

# Plotting ----------------------------------------------------------------

# Aggregates deaths by scale in timeline
# timeAggregate(tls[[1]],'years')
# Called by plotsByScaleVector()
timeAggregate<-function(timeline,scale = 'days'){
  
  if(scale == 'days'){
    
  } else if (scale == 'weeks'){
    timeline<-timeline%>%
      group_by(year,month,week)%>%
      summarise(deaths = sum(deaths),
                date = mean(date))
  } else if (scale == 'months'){
    timeline<-timeline%>%
      group_by(year,month)%>%
      summarise(deaths = sum(deaths),
                date = mean(date))
  } else if (scale == 'years'){
    timeline<-timeline%>%
      group_by(year)%>%
      summarise(deaths = sum(deaths),
                date = mean(date))
  }
  timeline
  
}

calculateXbreaks<-function(startdate='1992-11-29',enddate='2018-05-18'){
  startdate<-as.Date(startdate,format='%Y-%m-%d')
  enddate<-as.Date(enddate,format='%Y-%m-%d')
  diff <- enddate-startdate
  breaks<-seq.Date(startdate,enddate,diff/10)
  breaks
}

# Plots the timeline
# Remember to add scale and countryname args
# plotTimeline(timeAggregate(tls[[1]],'years'),'years','Colombia')
# Called by plotsByScaleVector()
plotTimeline<-function(timeline,scale='days',countryName='Noname'){
  # Plots a timeline, the scale parameter ONLY changes annotation
  # TODO change scale breaks on Y-scale
  # TODO where is the country name variable?
  
  if(scale == 'days'){
    scaleStr<-'Daily'
  } else if (scale == 'weeks'){
    scaleStr <- 'Weekly'
  } else if (scale == 'months'){
    scaleStr <- 'Monthly'
  } else if (scale == 'years'){
    scaleStr <- 'Yearly'
  }
  
  xBreaks<-calculateXbreaks(min(timeline$date),max(timeline$date))
  
  plotOut<-ggplot(timeline,aes(date,deaths))+
    scale_x_date(breaks = xBreaks)+
    geom_line(size = 1)+
    labs(title = 'Deaths from GED events',
         subtitle = paste(countryName,'-',scaleStr),
         x='',
         y=paste('Total deaths',scaleStr))+
    theme(axis.text.x = element_text(angle = 45,hjust = 1))
  
  plotOut
}
#TODOS!

# Plots timelines by a scale vector (multiple scales!)
# plotsByScaleVector(tls[[1]],c('weeks','months'),name)
# Returns a list of plots
plotsByScaleVector<-function(timeline,scaleVec,name){
  plots <- list()
  index <- 1
  
  for(scale in scaleVec){
    plots[[index]] <- plotTimeline(timeAggregate(timeline,scale),scale,max(timeline$country))
    index <- index + 1
  }
  plots
}

# Saves plots with filename based on subtitle
# savePlots(plots)
savePlots<-function(plots){
  i <- 1
  for(plot in plots){
    splitSubt<-strsplit(plot$labels$subtitle,split = ' - ')%>%
      unlist()
    countryNameStr = splitSubt[1]
    scaleStr <- splitSubt[2]
    path <- './plots/'
    ggsave(filename = paste(path,countryNameStr,'_',scaleStr,'.pdf',sep = ''),plot = plot,device = 'pdf')
  }
}
