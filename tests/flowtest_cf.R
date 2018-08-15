library(readxl)
# Append. to flowtest.R

source('tests/flowtest.R')

# TestFlow ##########################
cf_data <- read_xlsx('data/Dataset_ceasefire_complete.xlsx')%>%
  filter(Location == 'Colombia')

cf_data$end_date <- ifelse(cf_data$End_yr > 0 & cf_data$End_month > 0 & cf_data$End_day > 0,
                           paste(as.numeric(cf_data$End_yr),
                                 as.numeric(cf_data$End_month),
                                 as.numeric(cf_data$End_day),
                                 sep = '-'),
                           NA)%>%
  as.Date()
cf_data$start_date <- paste(as.numeric(cf_data$CF_effect_yr),
                            as.numeric(cf_data$CF_effect_month),
                            as.numeric(cf_data$CF_effect_day),
                            sep = '-')%>%
  as.Date()

cf_data <- cf_data%>%
  select(dyad = UCDP_Dyad_1,
         start_date,
         end_date)%>%
  mutate(dyad = ifelse(dyad > 0,
                       dyad%>%
                         as.numeric()%>%
                         as.character(),
                       NA))

# Dyad data for naming ##############

dyads <- read.csv('data/ucdp-dyadic-181.csv',stringsAsFactors = FALSE)%>%
  mutate(dyadName = paste(side_a,'-',side_b))%>%
  select(dyad_id,dyadName)

dyadvec <- dyads$dyadName
names(dyadvec) <- dyads$dyad_id

cf_data$dyadName <- dyadvec[cf_data$dyad]

# Pull FARC data ####################

expl_farc <- expl[str_detect(expl$dyad,'FARC'),]
cf_farc <- cf_data[str_detect(cf_data$dyadName,'FARC'),]%>%
  .[complete.cases(.),]%>%
  unique()%>%
  arrange(start_date)
cf_farc$id <- as.numeric(rownames(cf_farc))

expl_others <- expl[!str_detect(expl$dyad,'FARC'),]
cf_others <- cf_data[!str_detect(cf_data$dyadName,'FARC'),]%>%
  .[complete.cases(.),]%>%
  unique()%>%
  arrange(start_date)
cf_others$id <- as.numeric(rownames(cf_others))

# Plots - farc ######################

plot_farc <- expl_farc%>%
  filter(dyad == 'Government of Colombia - FARC')%>%
  ggplot(aes(x = date))+
  geom_freqpoly(binwidth = 365)+
  scale_x_date(date_breaks = '2 years')


plot_farc+
  geom_rect(data = cf_farc,
    aes(
      x = NULL,
      xmin = start_date,
      xmax = end_date+1,
      ymin = 0,
      ymax = 3000,
      fill = factor(id %% 2)
    ),alpha = 0.5)+
  geom_text(data = cf_farc,
    aes(
      x = start_date+(end_date-start_date)/2,
      y = (as.numeric(cf_farc$id) * 100)+1000,
      label = as.character(cf_farc$start_date)
    ))+
  scale_size_continuous(range = c(4,6))

zoomPlot <- expl_farc%>%
  filter(date > date('2012-01-01'))%>%
  ggplot(aes(x = date))+
    geom_freqpoly(binwidth = 12)+
    scale_x_date(date_breaks = 'years')
zoomPlot+
  geom_rect(data = cf_farc%>%
              filter(start_date > date('2012-01-01')),
            aes(
              x = NULL,
              xmin = start_date,
              xmax = end_date+1,
              ymin = 0,
              ymax = 100,
              fill = factor(id %% 2)
            ),alpha = 0.5)

# Plots - others ######################

plot_eln <- expl_others%>%
  filter(dyad == 'Government of Colombia - ELN')%>%
  ggplot(aes(x = date))+
    geom_freqpoly(binwidth = 183)
plot_eln+
  geom_rect(data = cf_others,
            aes(
              x = NULL,
              xmin = start_date,
              xmax = end_date+1,
              ymin = 0,
              ymax = 500,
              fill = factor(id %% 2)
            ),alpha = 0.5)+
  geom_rect(data = cf_others,
            aes(
              x = NULL,
              xmin = start_date-50,
              xmax = end_date+50,
              ymin = 0,
              ymax = 500,
              fill = factor(id %% 2)
            ),alpha = 0.2)
