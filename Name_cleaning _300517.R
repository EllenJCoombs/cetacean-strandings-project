

#This code runs on from the cleandates.csv, i.e rerun that code to
#clean the dataset before doing the below 

# Load libraries/packages
library(dplyr)
library(tidyr)
library(ggplot2)

nhmcsip <- read.csv("cleandates.csv")
nhmcsip
names(nhmcsip) 

nhmcsip$Name.Current.Sci
nhmcsip$Name.Common


#This works for replacing NA with uknown
nhmcsip$Name.Current.Sci<-as.character(nhmcsip$Name.Current.Sci)
nhmcsip$Name.Current.Sci
replace(nhmcsip$Name.Current.Sci, is.na(nhmcsip$Name.Current.Sci), "Unknown")
nhmcsip$Name.Common


#Cleaning some of the common names 
nhmcsip$Name.Common
nhmcsip$Name.Common<-as.character(nhmcsip$Name.Common) 
nhmcsip$Name.Current.Sci<-as.character(nhmcsip$Name.Current.Sci) 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "WhiteNAsided dolphin"]<- "White-sided dolphin" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "LongNAfinned Pilot whale"] <- "Long-finned pilot whale" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "BottleNAnosed whale"] <- "Bottlenose whale" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "WhiteNAbeaked dolphin"] <- "White-beaked dolphin" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "BottleNAnosed dolphin"] <- "Bottlenose dolphin" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "Unknown delphinidae "] <- "Unknown delphinid" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "Unknown balaenoptera"] <- "Unknown balaenopterid" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "Unknown odontocete  "] <- "Unknown odontocete" 

nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown delphinidae "] <- "Unknown delphinid" 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown balaenoptera "] <- "Unknown balaenopterid" 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown balaenoptera"] <- "Unknown balaenopterid"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "unknown balaenoptera"] <- "Unknown balaenopterid"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown odontocete "] <- "Unknown odontocete" 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown odontocete"] <- "Unknown odontocete" 


nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "Un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "un. mysticete"] <- "Unknown mysticete"

nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "Un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "un. mysticete"] <- "Unknown mysticete"


#Replacing NA with "unknown" 
replace(nhmcsip$Name.Common, is.na(nhmcsip$Name.Common), "unknown cetacean")
replace(nhmcsip$Name.Current.Sci, is.na(nhmcsip$Name.Current.Sci), "unknown cetacean") 


#putting all in lowercase 
nhmcsip$Name.Current.Sci<-tolower(nhmcsip$Name.Current.Sci)
nhmcsip$Name.Current.Sci
nhmcsip$Name.Common<-tolower(nhmcsip$Name.Common)
nhmcsip$Name.Common

#variations in species name 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "physeter catodon"]<- "physeter macrocephalus" 
nhmcsip$Name.Current.Sci


#Code for using 'replace' instead of base R 
nhmcsip$Name.Common
replace(nhmcsip$Name.Common, "unknown delphinidae", "unknown delphinid")
replace(nhmcsip$Name.Common, "unknown odontocete", "unknown odontocete  ") 


write.csv(nhmcsip, file = "cleandatesnames.csv")

#Some formatting - not sure what happened but I had extra columns being added "X" and "X1" 
#So I deleted them 
#Also dates reverted back to character (class) so I changed them back to 'Date' (class)
names(cleandatesnames)
#Select columns I want 
namesdatesedit <- select(cleandatesnames, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Year, Date)
names(namesdatesedit) 
#Check what classes the columns are 
sapply(namesdatesedit, class)
View(namesdatesedit)
#Convert date to 'Date' 
namesdatesedit <- mutate(namesdatesedit, Date = as.Date(Date)) 
sapply(namesdatesedit, class) 



#making a final clean file 
write.csv(namesdatesedit, file = "cleaned.data.300517.csv")





