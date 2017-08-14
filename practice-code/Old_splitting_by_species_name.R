
library(dplyr)
library(tidyverse)
install.packages("dplyr")


families <- read.csv("cleaned.data.300517.csv")
names(families)

#Getting rid of the extra X1 column 
familiesclean <- select(families, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Year, Date) 
levels(familiesclean$Name.Current.Sci)

dim(familiesclean)

#Having a look at how many of each species 
select(familiesclean, Name.Current.Sci)
species <- count(familiesclean, Name.Current.Sci)

View(species)

speciesyear <- select(families, Year, Name.Current.Sci)
splitspeciesyear<-data.frame(speciesyear,stringsAsFactors = TRUE)
splitspeciecsyear


#Making these factors (not sure I actually need to do this...)
speciecsyear$Name.Current.Sci<-as.factor(speciesyear$Name.Current.Sci)
splitspeciesyear$Year<-as.factor(splitspeciesyear$Year)

#Splitting by year and species 
splitspeciesyear %>% group_by(Year)%>%group_by(Name.Current.Sci)
totals <- splitspeciesyear %>%
  group_by(Name.Current.Sci, Year)

filter (totals, Name.Current.Sci == "ziphius cavirostris" %>% "phocoena phocoena")
  

  



