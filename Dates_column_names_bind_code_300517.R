

#Code for cleaning up messy dates and latitudes in the NHM dataset 
#Code for cleaning the dates in the CSIP dataset 
#Code for renaming columns so that both datsets have the same variables 
#Code for binding the two datasets together 


rm(list = ls())
#Load libraries/packages
library(dplyr)
library(tidyr)
library(ggplot2)


#read.csv("EDITNHMdata.csv")
nhm <- read.csv("EDITNHMdata.csv", header = TRUE)
#read.csv("EDITCSIPdata.csv")
csip <- read.csv("EDITCSIPdata.csv", header = TRUE)
names(csip)
names(nhm)

str(nhm)#returns the structure of the dataset
str(csip)

#Cleaning column names to keep:
#Name Common
#Name Current Sci
#Date (NHM) Date found (CSIP)
#Location
#Grid ref
#County (NHM) Local authority (CSIP); change to county?

#Names of counties
#Need to make them all the same as with species names

names(csip)
names(nhm)
csip <- rename(csip, County = Local.Authority)
csip <- rename(csip, Name.Common = Name.common)
nhm <- rename(nhm, Grid.Ref = Grid.ref)
nhm <-rename(nhm, Year = year)
csip <- rename(csip, Year = year)
csip <- rename(csip, Date = Date.Found)



names(csip)


#Changing dates and names in the selectnhm

csip$Date #all dates appear in the same format...
nhm$Date #some dates appear as August 1929 or Summer 1929, code below to clean
levels(nhm$Date)
View(nhm)


#attempt using mutate to change "beg" "mid" "end" and "Summer" "Winter" etc
#Beg and early = 1
#Mid = 14
#End = 27
#wk 1 = 1
#wk 2 = 8
#wk 3 = 15
#wk 4 = 22
#Spring = 20 Mar
#Summer = 21 Jun
#Autumn = 22 Sep
#Winter = 21 Dec

#16-17 range of dates to take earliest date
#what shall I do with 4-5 etc, ?, 'c' and mm yyyy? How? Also Jun 1929 etc



#embedding commands
nhm$Date <- gsub("Summer", "1 Jun",
            gsub("Winter", "1 Dec",
            gsub("Spring", "1 Mar",
            gsub("Autumn", "1 Sept",
            gsub("beg", "1",
            gsub("Beg", "1",
            gsub("mid", "14",
            gsub("Mid", "14",
            gsub("end", "27",
            gsub("End", "27",
            gsub("Early", "1",
            gsub("Ely", "1",
            gsub("Early", "1",
            gsub("wk1", "1",
            gsub("wk2", "8",
            gsub("wk3", "15",
            gsub("wk4", "22",
            gsub("Wk4", "22",
                 nhm$Date))))))))))))))))))


#more specific changes - might have to do each at a time?


View(nhm)

#checking if the above has worked
nhm$Date
head(nhm$Date)
tail(nhm$Date)
View(nhm)

nhm$Date


#Select specific coloumns
selectnhm <- select(nhm, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Date, Year)
selectcsip <- select(csip, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Date, Year)

#Checking that the above has worked 
View(selectnhm)
selectnhm$Date


#Changing date format in the NHM dataset
install.packages("lubridate")
library(lubridate)
library(dplyr)
#Why is this adding 100 years???
selectnhm <- mutate(selectnhm, Date = dmy(Date))

#Trying this instead - works by changing all 2000s to 1900s
nhmdates <- select(nhm, Date)
selectnhm <- mutate(nhmdates, Date = format(as.Date(Date, "%d-%b-%y"), "19%y-%m-%d"))
selectnhm

#Changing selectnhm to a 'Date' rather than a 'character'
selectnhm <- mutate(selectnhm, Date = as.Date(Date)) 

sapply(selectnhm, class)

#Double checking
View(selectnhm)
sapply(selectnhm, class)


#Adding the mutated date column 
#Changing the Latitudes to 'numeric' 
nhmnew <- select(nhm, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Year)
nhmnew <- mutate(nhmnew, Latitude = as.numeric(Latitude))
nhmfinal <- bind_cols(nhmnew, selectnhm, .id = NULL)
nhmfinal$Date

nhmfinal


#Changing CSIP date to YYYYMMMDD
csip$Date
csip <- mutate(csip, Date = dmy(Date))


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
