#Splitting the dataset into smaller sections: guilds, regions, pre/post CSIP etc 

library(dplyr)
library(tidyverse)
library(picante)


#Splitting by odontocetes and mysticetes 

cleaneddata <- read.csv("cleandatesnames.csv")

#Mysitcetes

bphysalus <- filter(cleaneddata, Name.Current.Sci ==  "Balaenoptera physalus") 
bacutorostrata <- filter(cleaneddata, Name.Current.Sci ==  "Balaenoptera acutorostrata") 
bborealis <- filter(cleaneddata, Name.Current.Sci ==  "Balaenoptera borealis")
bmusculus <- filter(cleaneddata, Name.Current.Sci ==  "Balaenoptera musculus") 
unmysticete <- filter(cleaneddata, Name.Current.Sci ==  "Unknown mysticete") 
mnovaeangliae <- filter(cleaneddata, Name.Current.Sci == "Megaptera novaeangliae") 


Mysticetes <- combinedmysticetes <- rbind(bphysalus, bacutorostrata, bborealis, bmusculus, 
                                          unmysticete, mnovaeangliae)
Mysticetes$X <- NULL
Mysticetes$X.1 <- NULL

write.csv(Mysticetes, file = "Mysticetes.csv")

#Selecting out odontocetes from main dataset and then delete Mysticetes 
Odontocetes <- cleaneddata[ !(cleaneddata$Name.Current.Sci %in% Mysticetes$Name.Current.Sci), ]

write.csv(Odontocetes, file = "Odontocetes.csv")


#Mysticete and Odontocete richness 
#Have to order in this way for matrix to work

#Need to remove the unknowns 
Mysticetes_known <- Mysticetes %>% 
  filter(!(Name.Current.Sci=="Unknown mysticete"))


Mysticetes_year <- dplyr::count(Mysticetes_known, Name.Current.Sci, Year)
Mysticetes_year <- Mysticetes_year[c("Year","n", "Name.Current.Sci")]

Mysticete.matrix <- sample2matrix(Mysticetes_year)
#Number of species per year 
specnumber(Mysticete.matrix)

#Richness double check 
Mysticete_richness <- Mysticetes_year %>%
  count(Year)

Mysticete_richness <- Mysticete_richness %>%
  rename(Richness = nn)

#Odontocete richness 
#Mysticete and Odontocete richness 
#Have to order in this way for matrix to work 

Odontocetes_known <- Odontocetes %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown delphinid ",
                                   "Unknown delphinid")))

Odontocetes_year <- dplyr::count(Odontocetes_known, Name.Current.Sci, Year)
Odontocetes_year <- Odontocetes_year[c("Year","n", "Name.Current.Sci")]

Odontocetes.matrix <- sample2matrix(Odontocetes_year)
#Number of species per year 
specnumber(Odontocetes.matrix)

#Richness double check 
Odontocete_richness <- Odontocetes_year %>%
  count(Year)

Odontocete_richness <- Odontocete_richness %>%
  rename(Richness = nn)



