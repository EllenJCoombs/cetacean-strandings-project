

#This code runs on from the cleandates.csv, i.e rerun that code to
#clean the dataset before doing the below 

# Load libraries/packages
library(dplyr)
library(tidyr)
library(ggplot2)

nhmcsip <- read.csv("cleandatesnames.csv") 
nhmcsip
names(nhmcsip) 

nhmcsip$Name.Current.Sci
nhmcsip$Name.Common

nhmcsip$Name.Common

#Have a look at which names need to be changed 
levels(nhmcsip$Name.Current.Sci)
levels(nhmcsip$Name.Common)

#Cleaning some of the common names - Can't seem to be able to get replace to work 
#so using base R 
#This is just a quick clean up as I'll be focusing on Name.Current.Sci 
nhmcsip$Name.Common
nhmcsip$Name.Common<-as.character(nhmcsip$Name.Common) 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "WhiteNAsided dolphin"]<- "White-sided dolphin" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "LongNAfinned Pilot whale"] <- "Long-finned pilot whale" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "BottleNAnosed whale"] <- "Bottlenose whale" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "WhiteNAbeaked dolphin"] <- "White-beaked dolphin" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "BottleNAnosed dolphin"] <- "Bottlenose dolphin" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "Unknown delphinidae "] <- "Unknown delphinid" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "Unknown balaenoptera"] <- "Unknown balaenopterid" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "Un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "Unknown odontocete  "] <- "Unknown odontocete" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "Un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "un. mysticete"] <- "Unknown mysticete"

#Replace NA with "unknown" - the only replace command that I can get to work 
replace(nhmcsip$Name.Common, is.na(nhmcsip$Name.Common), "Unknown")



nhmcsip$Name.Current.Sci<-as.character(nhmcsip$Name.Current.Sci) 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown delphinid "] <- "Unknown delphinid" 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown delphinid"] <- "Unknown delphinid" 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown delphinidae "] <- "Unknown delphinid" 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown delphinidae"] <- "Unknown delphinid" 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown balaenoptera "] <- "Unknown balaenoptera" 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown balaenopterid"] <- "Unknown balaenoptera"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown balaenoptera"] <- "Unknown balaenoptera"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown odontocete "] <- "Unknown odontocete" 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown odontocete"] <- "Unknown odontocete" 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. mysticete"] <- "Unknown mysticete"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. Mysticete "] <- "Unknown mysticete"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. Mystitcete"] <- "Unknown mysticete"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Balaenoptera acutorostrata?"] <- "Balaenoptera acutorostrata"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Balaenoptera physalus?"] <- "Balaenoptera physalus"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Phocoena phocoena?"] <- "Phocoena phocoena"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "phocoena phocoena"] <- "Phocoena phocoena"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. Mystitcete "] <- "Unknown mysticete"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. Lagenorhyncus "] <- "Unknown delphinid"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. Ziphiidae "]<- "Unknown odontocete"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Physeter catodon"]<- "Physeter macrocephalus"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Lagenorhynchus acutus?"]<- "Lagenorhynchus acutus"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "Unknown "]<- "Unknown"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. Odontocete"] <- "Unknown odontocete"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un.  Balaenoptera"] <- "Unknown balaenopterid"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. Balaenoptera"] <- "Unknown balaenopterid"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un.Balaenoptera"] <- "Unknown balaenopterid"
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "un. Delphinidae"] <- "Unknown delphinid"
replace(nhmcsip$Name.Current.Sci, is.na(nhmcsip$Name.Current.Sci), "Unknown")

View(nhmcsip)

#putting all in lowercase - this was fir ease of quick tyoing - don't do normally 
#nhmcsip$Name.Current.Sci<-tolower(nhmcsip$Name.Current.Sci)
#nhmcsip$Name.Common<-tolower(nhmcsip$Name.Common)


#variations in species name 
 
nhmcsip$Name.Current.Sci

#Code for using 'replace' instead of base R - not working - not sure why! 
#replace(nhmcsip$Name.Common, "unknown delphinidae", "unknown delphinid")
#replace(nhmcsip$Name.Common, "unknown odontocete", "unknown odontocete  ") 


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

finalclean <- read.csv("cleandatesnames.csv")



