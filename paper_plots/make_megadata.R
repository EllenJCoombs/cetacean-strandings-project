
cleaneddata <- read.csv("UK_and_Irish_strandings.csv")


library(maps)
library(mapdata)
library(dplyr)


latlong <- select(cleaneddata, Year, Name.Current.Sci, Longitude, Latitude)
bphysalus <- filter(latlong, Name.Current.Sci ==  "Balaenoptera physalus")
bactorostrata <- filter(latlong, Name.Current.Sci ==  "Balaenoptera acutorostrata")
bborealis <- filter(latlong, Name.Current.Sci ==  "Balaenoptera borealis")
bmusculus <- filter(latlong, Name.Current.Sci ==  "Balaenoptera musculus")
unmysticete <- filter(latlong, Name.Current.Sci ==  "Unknown mysticete")
unbalaenopterid <- filter(latlong, Name.Current.Sci ==  "Unknown balaenoptera")
mysticete <- filter(latlong, Name.Current.Sci == "Un. mystitcete")
mnovaeangliae <- filter(latlong, Name.Current.Sci == "Megaptera novaeangliae")

#bind all mysticetes
combinedmysticetes <- rbind(bphysalus, bactorostrata, bborealis, bmusculus, unmysticete, unbalaenopterid, mysticete, mnovaeangliae)
#arranging by year
sortedmysticetes <- arrange(combinedmysticetes,(Year))
#Stripping out odontocetes 
odontocetes <- latlong[ !(latlong$Name.Current.Sci %in% sortedmysticetes$Name.Current.Sci), ]

#Ordering and combining species by year - to arrange in descending order put "desc" before the year 
sortedodonts <- arrange(odontocetes,(Year))

sortedmysticetes$whatareyou <- "mysticete"
sortedodonts$whatareyou <- "odontocete"


# Natalie hates beaked whales
#beakers <- latlong %>% 
#  filter(Name.Current.Sci %in% c("Hyperoodon ampullatus", "Mesoplodon densirostris", "Mesoplodon europaeus", "Mesoplodon mirus"))
#
#beakers$whatareyou <- "beaker"


allall <- rbind(sortedodonts, sortedmysticetes)
allall$whatareyou <- "all"
allparv <- rbind(sortedodonts, sortedmysticetes, allall)


save(allparv, file="allparv.RData")
