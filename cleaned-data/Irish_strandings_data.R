#Irish data 

library(dplyr)
library(lubridate)
#First of all my cleaned NHM/CSIP data 

cleaneddata <- read.csv("cleandatesnames.csv")

levels(cleaneddata$Name.Current.Sci)

Irish_data <- read.csv("Irish_strandings_raw.csv")
Irish_data$X. <- NULL

Irish_data <- Irish_data %>%
  rename(Date = Event.Date)

#Remove non cetaceans 
Irish_data <- Irish_data %>% 
  filter(!(Species %in% c("leatherback turtle", "basking shark", "loggerhead turtle", "Kemp's ridley turtle")))

levels(Irish_data$Species)

#Unknowns 
#These are listed as "whale species in the Irish data 
Irish_data$Species<-as.character(Irish_data$Species)
Irish_data$Species[Irish_data$Species %in% "cetacean species"] <- "Unknown" 
Irish_data$Species[Irish_data$Species %in% "whale species"] <- "Unknown" 
Irish_data$Species[Irish_data$Species %in% "large whale species"] <- "Unknown" 
Irish_data$Species[Irish_data$Species %in% "lagenorhynchus species"] <- "Unknown delphinid" 
Irish_data$Species[Irish_data$Species %in% "common or striped dolphin"] <- "Unknown delphinid" 
Irish_data$Species[Irish_data$Species %in% "dolphin species"] <- "Unknown delphinid" 
Irish_data$Species[Irish_data$Species %in% "pilot/false killer whale"] <- "Unknown odontocete" 
Irish_data$Species[Irish_data$Species %in%  "dolphin species possibly harbour porpoise"] <- "Unknown"
Irish_data$Species[Irish_data$Species %in%  "sei fin or blue whale"] <- "Unknown mysticete" 
Irish_data$Species[Irish_data$Species %in%  "beaked whale species"] <- "Unknown odontocete"

#Filter out the years (1913:2015)
Irish_data <- Irish_data %>%
  filter(Year %in% c(1913:2015))

#Cleaning Scientific names 
Irish_data$Name.Current.Sci<-as.character(Irish_data$Name.Current.Sci)
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in% "cetacean species"] <- "Unknown" 
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in% "whale species"] <- "Unknown" 
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in% "large whale species"] <- "Unknown" 
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in% "lagenorhynchus species"] <- "Unknown delphinid" 
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in% "common or striped dolphin"] <- "Unknown delphinid" 
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in% "dolphin species"] <- "Unknown delphinid" 
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in% "pilot/false killer whale"] <- "Unknown odontocete" 
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in%  "dolphin species possibly harbour porpoise"] <- "Unknown"
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in%  "sei fin or blue whale"] <- "Unknown mysticete" 
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in%  "beaked whale species"] <- "Unknown odontocete"
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in%  "dolphin species possibly Phocoena phocoena"] <- "Unknown"
Irish_data$Name.Current.Sci[Irish_data$Name.Current.Sci %in%  "common or Stenella coeruleoalba"] <- "Unknown delphinid"


#Need to rename the variables to be the same as NHM/CSIP 
Irish_data <- Irish_data %>% 
  rename(Name.Common = Species) %>%
  rename(County = County.Region) %>%
  rename(Latitude = GPS.Lat) %>%
  rename(Longitude = GPS.Long) %>%
  rename(S.W.No. = Stranding.ID)

#Select out the columns
Irish_data <- Irish_data %>%
  select(Date, Year, Name.Common, Name.Current.Sci, Latitude, Longitude, County, S.W.No.)



Irish_data$Date <- Irish_data$Date <- lubridate::dmy(Irish_data$Date)
cleaneddata$Date <- cleaneddata$Date <- lubridate::dmy(cleaneddata$Date)

Irish_data$Name.Common <- as.factor(Irish_data$Name.Common)
Irish_data$Name.Current.Sci <- as.factor(Irish_data$Name.Current.Sci)

sapply(cleaneddata, class)
sapply(Irish_data, class)


#Bind cleanedata with Irish data 
UK_and_Irish <- bind_rows(cleaneddata, Irish_data)

#Arrange by date 
UK_and_Irish <- UK_and_Irish %>%
  arrange(Date)

arrange(UK_and_Irish, Date)


#Filtering out the EIRE data (these are duplicates) EIRE appears in the NHM/CSIP data 

a <- dplyr::filter(UK_and_Irish, !grepl("EIRE",County))

write.csv(UK_and_Irish, file = "UK_and_Irish_strandings.csv")


#for duplicates 
UK_and_Irish[!(duplicated(UK_and_Irish[c("Date","Name.Current.Sci", "County")]) | duplicated(df[c("Date","Name.Current.Sci", "County")], fromLast = TRUE)), ]
