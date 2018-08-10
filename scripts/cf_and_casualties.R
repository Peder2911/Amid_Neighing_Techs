
library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)
library(readxl)
library(lubridate)

setwd('~/code/PRIOcode/ged')

source('scripts/dataImportFuncs.R')
source('scripts/dataMungingFuncs.R')

# Variables #########################

names <- c('Colombia')

start <- '1989-01-01'
end <- Sys.time()%>%
  as.character()%>%
  str_split(' ')%>%
  unlist()%>%
  .[1]

# Imports ###########################

ged <- getGed(names,start,end)%>%
  .[[1]]
cf <- read_xlsx('data/Colombia.xlsx')%>%
  .[-1,]

# Transformation ####################

tl <- getTimeline(ged,start,end)

tl_cf <- tibble(date = seq(as.Date(start),as.Date(end),by='days'))%>%
  mutate(ceasefire = ifelse(date %in% cfdates,1,0))%>%
  filter(ceasefire == 1)

tl_sum <- merge(tl,tl_cf,all.x = TRUE)%>%
  mutate(ceasefire = ifelse(is.na(ceasefire),0,1))

plot_a <- tl%>%
  group_by(midYear(date))%>%
  summarise(deaths = sum(deaths))%>%
  rename(date = `midYear(date)`)%>%
  ggplot(aes(date,deaths))+
    geom_line()

plot_b <- tl_sum%>%
  group_by(midYear(date))%>%
  summarise(deaths = sum(deaths),
            ceasefires = sum(ceasefire))%>%
  rename(date = `midYear(date)`)%>%
  ggplot(aes(x = date, y = deaths))+
    geom_line()+
    geom_point(aes(size = ceasefires),color = 'red')

plot_d <- tl_sum%>%
  group_by(midYear(date))%>%
  summarise(deaths = sum(deaths),
            ceasefires = sum(ceasefire))%>%
  rename(date = `midYear(date)`)%>%
  rowwise()%>%
  mutate(id = sample(c('a','b','c'),size = 1))%>%
  ggplot(aes(x = date, y = deaths))+
  geom_polygon(aes(color = id))+
  geom_point(aes(size = ceasefires),color = 'red')

# Plots #############################


