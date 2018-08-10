# source me

library(stringr)
library(forcats)

# Architecture tests ################
if(!str_sub(getwd(),start = str_length(getwd())-3,end = str_length(getwd())) == '/ged' ){
  stop('run from the root directory!')
}
if(!any(str_detect(list.files('data'),'ged[0-9]+\\.csv'))){
  stop('no data file found in ./data/!')
}

source('scripts/dataMungingFuncs.r')
source('scripts/dataImportFuncs.r')

# Testdefaults ######################
startDate<-'1989-01-01'

endDate<- Sys.time()%>%
  as.character()%>%
  str_split(' ')%>%
  unlist()%>%
  .[1]

names <- 'Colombia'

# TestFlow ##########################
gedDat <- getGed(names,startDate,endDate)[[1]]
tl <- getTimeline(gedDat,startDate,endDate)
expl <- explodeTimeline(tl,countVar = 'deaths')

# Plots #############################
s <- expl%>%
  group_by(dyad)%>%
  summarise(obs = n())%>%
  arrange(-obs)
expl$dyad <- factor(expl$dyad,levels = s$dyad)

plot <- ggplot(expl,aes(date,color = dyad))+
  geom_freqpoly(size=1,position = 'stack',binwidth = 365)+
  scale_x_date(date_breaks = '2 years')

plot2 <- expl%>%
  filter(dyad != 'Government of Colombia - FARC')%>%
  ggplot(aes(date,fill = dyad))+
    geom_density(aes(color = dyad),stat='bin',binwidth = 365,position='stack',alpha = 0.5)+
    scale_x_date(date_breaks='2 years')