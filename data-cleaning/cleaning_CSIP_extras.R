
#Working with the CSIP dataset only as it records bycatch, pathologist and date certainty
#which the NHM fails to do in detail 

#Code for cleaning the dates in the CSIP dataset 
#Code for renaming columns so that both datsets have the same variables 
#Code for binding the two datasets together 

#Load libraries/packages
library(dplyr)
library(tidyr)
library(ggplot2)


#Just working with CSIP data here 
#I've already cleaned this data before but need to do so again as I want to look at some 
#additional columns e.g. cause of death 
#read.csv("EDITCSIPdata.csv")
csip <- read.csv("EDITCSIPdata.csv", header = TRUE)
names(csip)

#Keep these the same as the NHM categories - for consistency 
csip <- rename(csip, County = Local.Authority)
csip <- rename(csip, Name.Common = Name.common)
csip <- rename(csip, Year = year)
csip <- rename(csip, Date = Date.Found)

#Changing date format in the NHM dataset
install.packages("lubridate")
library(lubridate)
library(dplyr)

#The columns I want to select to look at 
selectnhm <- select(nhm, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Date, Year)
selectcsip <- select(csip, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Date, Year)

#Changing CSIP date to YYYYMMMDD
csip$Date
csip <- mutate(csip, Date = dmy(Date))

#Tidying names - as before 
#Names are tidy in the CSIP dataset (would use code as before if not)

#Columns that I want to work with 
csipfinal <- select(csip, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Date, Year)
csipfinal


#merging the two datasets
#Checking that both have the same classes

sapply(nhmfinal, class)
sapply(csipfinal, class)

#Merging the two datasets 
nhmcsip <- bind_rows(nhmfinal, csipfinal)
View(nhmcsip)

nhmfinal$Latitude


#Saving the new dataset 

write.csv(nhmcsip, file = "cleandates.csv") 


#Notes to think about 
#105 fail to parse - was 130, am still missing some - need to work out how to change 'June 1929" etc
View(selectnhm$Date)
selectnhm
labels(selectnhm$Date)
selectnhm$Date
selectnhm

View(selectnhm)

nhm$Date


csip$Name.Current.Sci
levels(csip$Name.Current.Sci)
