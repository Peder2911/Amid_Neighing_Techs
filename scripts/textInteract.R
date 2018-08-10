# Interactive prompt for generating plots with main.r

names<-unlist(strsplit(readline('Enter countries (separated by comma if several): '),','))

scales <-unlist(strsplit(readline('Enter timescales (separated by comma if several): '),','))

startDate<-readline('Enter start date in format YYYY-MM-DD: ')
endDate<-readline('Enter end date in format YYYY-MM-DD: ')
