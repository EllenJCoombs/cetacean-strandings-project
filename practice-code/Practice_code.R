
#This is loads of messy practice code that has been copied and pasted into my actual code 
#Can be ignored! 
#Just saved for safe keeping




#merge the data sets 
library(dplyr)
#joining using dplyr #don't know why this isn't working now 
fulljoin<-full_join(nhm, csip, by = "Date")
fulljoin$Date
tail(fulljoin$Date)
#changing dates using dplyr and lubridate
library(lubridate)
fulljoin<-mutate(fulljoin, Date = dmy(Date))
#still 108 failing to parse - need to find and change these 
fulljoin$Date



#To do: physeter catodon -> physeter macrocephalus 

fulljoin$Name.Current.Sci.x<-tolower(fulljoin$Name.Current.Sci.x)
fulljoin$Name.Current.Sci.x
#not sure where names.current.sci y is...(thought it would be the csip names)
fulljoin$Name.Current.Sci.y<-tolower(fulljoin$Name.Current.Sci.y)
fulljoin$Name.Current.Sci.y

fulljoin$Grid.ref



#not sure why not working - different columns??
library(dplyr)
bindrows<-bind_rows(nhm, csip.id = NULL)
bindrows
names(bindrows)



library(dplyr)
install.packages("stringr") #dealing with NAs? 
library(stringr)
install.packages("magrittr") #need to use this clean up my data and avoid my nested code 
library(stringr)


names(fulljoin)
View(fulljoin$Name.Current.Sci.x)

select(fulljoin, Date, Name.Current.Sci.X) 

tbl_df(fulljoin)

speciesnhm<-(fulljoin$Name.Current.Sci.x)
speciescsip<-(fulljoin$Name.Current.Sci.y)
View(speciesnhm)

nhmcsip<-data.frame(speciesnhm, speciescsip)
nhmonly<-slice(nhmcsip, 1:4311)
csiponly<-slice(nhmcsip, 4312:17402)

x<-data.frame(nhm$Name.Current.Sci)
y<-data.frame(csip$Name.Current.Sci)
bind_rows(x,y, .id = "combo") 



#Joining data in base R  - don't need this 
merge(nhm, csip, by = "Date")
datecsipnhm<-merge(x = nhm, y = csip, by = "Date", "Location", "Name.Current.Sci", all = TRUE)
names(datecsipnhm)
summary(datecsipnhm)
dim(datecsipnhm)
head(datecsipnhm)
glimpse(datecsipnhm)
library(stringr)
unique(datecsipnhm$Date.x)
library(dplyr)
install.packages("lubridate")
library(lubridate)
library(tidyr)

datecsipnhm$Date.x


#Changing names (need to change vector name)
nhm$Name.Common<-as.character(nhm$Name.Common)
nhm$Name.Common



#This seems to do lots of other stuff! 
nhm %>% 
  mutate(Date = gsub("Summer", "1 Jul", Date)) %>% 
  mutate(Date = gsub("Winter", "1 Dec", Date)) %>%
  mutate(Date = gsub("Autumn", "1 Sep", Date)) %>%
  mutate(Date = gsub("Spring", "1 Mar")) %>%
  mutate(Date = gsub("-", "1", Date)) %>% 
  mutate(Date = gsub("mid", "14", Date)) %>%
  mutate(Date = gsub("beg", "1", Date)) %>%
  mutate(Date = gsub("end", "27"))


nhm$Date<-gsub("Summer", "1 Jun", gsub("Winter", "1 Dec", gsub("Spring", "1 Mar", gsub("Autumn", "1 Sept", gsub("beg", "1", gsub ("Beg", "1", gsub("mid", "14", gsub("Mid", "14", gsub("end", "27", gsub ("End", "27", gsub("Early", "1", gsub ("Ely", "1", gsub ("Early", "1", gsub("wk1", "1", gsub("wk2", "8", gsub("wk3", "15", gsub("wk4", "22", nhm$Date)))))))))))))))))
nhm$Date
tail(nhm$Date)
View(nhm)

install.packages("lubridate")
library(lubridate)
library(dplyr)
nhm$Date<-mutate(nhm, Date = dmy(Date))


############
#PLaying with names 




#This code runs on from the 240517 Dates and column names, i.e rerun that code to
#clean the dataset before doing the below 

# Load libraries/packages
library(dplyr)
library(tidyr)
library(ggplot2)

nhmcsip$Name.Current.Sci
nhmcsip$Name.Common



#Can't get mutate to work 
#Trying to change NA to unknown (like in the CSIP data)
#These don't work 
nhm$Name.Current.Sci
mutate(nhm$Name.Current.Sci = gsub("NA", "Unknown", nhm$Name.Current.Sci))
nhm$Name.Current.Sci <- gsub("NA", "Unknown", nhm$Name.Current.Sci)
View(nhm)

#This works for replacing NA with uknown
nhm$Name.Current.Sci<-as.character(nhm$Name.Current.Sci)
replace(nhm$Name.Current.Sci, is.na(nhm$Name.Current.Sci), "Unknown")

nhm$Name.Current.Sci<-tolower(nhm$Name.Current.Sci)
nhm$Name.Current.Sci[nhm$Name.Current.Sci %in% "physeter macrocephalus"]<- "physeter catodon" 
nhm$Name.Current.Sci


nhm$Name.Common<-as.character(nhm$Name.Common)
nhm$Name.Common[nhm$Name.Common %in% "WhiteNAsided dolphin"]<- "White-sided dolphin" 
nhm$Name.Common[nhm$Name.Common %in% "LongNAfinned Pilot whale"] <- "Long-finned pilot whale"
nhm$Name.Common[nhm$Name.Common %in% "BottleNAnosed whale"] <- "Bottlenose whale"
nhm$Name.Common[nhm$Name.Common %in% "WhiteNAbeaked dolphin"] <- "White-beaked dolphin"
nhm$Name.Common[nhm$Name.Common %in% "BottleNAnosed dolphin"] <- "Bottlenose dolphin"
nhm$Name.Common[nhm$Name.Common %in% "NA"] <- "Unknown"
nhm$Name.Common

#Replacing NA with "unknown" 
replace(nhm$Name.Common, is.na(nhm$Name.Common), "Unknown") 

#To lower case 
nhm$Name.Common<-tolower(nhm$Name.Common)
nhm$Name.Common

#This code runs on from the 240517 Dates and column names, i.e rerun that code to
#clean the dataset before doing the below 

# Load libraries/packages
library(dplyr)
library(tidyr)
library(ggplot2)

nhmcsip$Name.Current.Sci
nhmcsip$Name.Common


#This works for replacing NA with uknown
nhmcsip$Name.Current.Sci<-as.character(nhmcsip$Name.Current.Sci)
nhmcsip$Name.Current.Sci
replace(nhmcsip$Name.Current.Sci, is.na(nhmcsip$Name.Current.Sci), "Unknown")

#putting all in lowercase 
nhmcsip$Name.Current.Sci<-tolower(nhmcsip$Name.Current.Sci)
nhm$Name.Current.Sci[nhm$Name.Current.Sci %in% "physeter macrocephalus"]<- "physeter catodon" 
nhmcsip$Name.Current.Sci


#Cleaning some of the common names 
nhmcsip$Name.Common
nhmcsip$Name.Common<-as.character(nhmcsip$Name.Common)
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "WhiteNAsided dolphin"]<- "White-sided dolphin" 
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "LongNAfinned Pilot whale"] <- "Long-finned pilot whale"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "BottleNAnosed whale"] <- "Bottlenose whale"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "WhiteNAbeaked dolphin"] <- "White-beaked dolphin"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "BottleNAnosed dolphin"] <- "Bottlenose dolphin"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "unknown delphinidae "] <- "unknown delphinid"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "unknown balaenoptera"] <- "unknown balaenopterid"
nhmcsip$Name.Common[nhmcsip$Name.Common %in% "unknown odontocete  "] <- "unknown odontocete"

replace(nhmcsip$Name.Common, is.na(nhmcsip$Name.Common), "Unknown")
nhmcsip$Name.Common

#Replacing NA with "unknown" 
replace(nhmcsip$Name.Common, is.na(nhmcsip$Name.Common), "Unknown") 


nhmcsip$Name.Common<-tolower(nhmcsip$Name.Common)
nhmcsip$Name.Common

#variations in species name 
nhmcsip$Name.Current.Sci[nhmcsip$Name.Current.Sci %in% "physeter macrocephalus"]<- "physeter catodon" 
nhmcsip$Name.Current.Sci


#Code for using 'replace' instead of base R 
nhmcsip$Name.Common
replace(nhmcsip$Name.Common, "unknown delphinidae", "unknown delphinid")
replace(nhmcsip$Name.Common, "unknown odontocete", "unknown unknown odontocete") 





