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

write.csv(Mysticete_richness, file = "Mysticete_richness.csv")

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

#Rename
Odontocete_richness <- Odontocete_richness %>%
  rename(Richness = nn)

write.csv(Odontocete_richness, file = "Odontocete_richness.csv")

#################################################################################################
#Odontocete and Mysticete stranding events 

#Can use duplicated - Odontocetes 
Odontocetes_known$X <- NULL
Odontocetes_known$X.1 <- NULL

duplicated(Odontocetes_known$S.W.No.)
#Or you can use unique 
unique(Odontocetes_known$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
Odontocete_events <- Odontocetes_known[!duplicated(Odontocetes_known$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
Odontocete_events$S.W.No. <- (sub("\\.\\d+$","", Odontocete_events$S.W.No.))
Odontocete_events <- Odontocete_events[!duplicated(Odontocete_events$S.W.No.), ]


ggplot(Odontocete_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)


write.csv(Odontocete_events, file = "Odontocete_strandings.csv")


#Can use duplicated - Mysticetes 
Mysticetes_known$X <- NULL
Mysticetes_known$X.1 <- NULL

duplicated(Mysticetes_known$S.W.No.)
#Or you can use unique 
unique(Mysticetes_known$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
Mysticete_events <- Mysticetes_known[!duplicated(Mysticetes_known$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
Mysticete_events$S.W.No. <- (sub("\\.\\d+$","", Mysticete_events$S.W.No.))
Mysticete_events <- Mysticete_events[!duplicated(Mysticete_events$S.W.No.), ]


ggplot(Mysticete_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

write.csv(Mysticete_events, file = "Mysticete_strandings.csv")

#################################################################################################
#By size 
#Big = all baleens (except Minkes) + Sperm whales 
#Medium = Orca, Minke, Hyperoodon 
#Small = everything else (small beakers, phocoena and dolphins)


#Big 
cleaneddata <- read.csv("cleandatesnames.csv")
bphysalus <- filter(cleaneddata, Name.Current.Sci ==  "Balaenoptera physalus") 
bborealis <- filter(cleaneddata, Name.Current.Sci ==  "Balaenoptera borealis")
bmusculus <- filter(cleaneddata, Name.Current.Sci ==  "Balaenoptera musculus") 
mnovaeangliae <- filter(cleaneddata, Name.Current.Sci == "Megaptera novaeangliae") 
pmacrocephalus <- filter(cleaneddata, Name.Current.Sci == "Physeter macrocephalus") 

Big_bs <- rbind(bphysalus, bborealis, bmusculus, mnovaeangliae, pmacrocephalus)
Big_bs$X.1 <- NULL
Big_bs$X <- NULL

write.csv(Big_bs, file = "Big_body_size.csv")

#Medium 
oorca <- filter(cleaneddata, Name.Current.Sci ==  "Orcinus orca")
hampullatus <- filter(cleaneddata, Name.Current.Sci ==  "Hyperoodon ampullatus")
bacutorostrata <- filter(cleaneddata, Name.Current.Sci ==  "Balaenoptera acutorostrata") 

Medium_bs <- rbind(oorca, hampullatus, bacutorostrata)
Medium_bs$X.1 <- NULL
Medium_bs$X <- NULL

write.csv(Medium_bs, file = "Medium_body_size.csv")

#Small - need to remove all of the uknowns as well 

Small_bs_clean <- cleaneddata[ !(cleaneddata$Name.Current.Sci %in% Big_bs$Name.Current.Sci), ] 
Small_bs_clean <- cleaneddata[ !(cleaneddata$Name.Current.Sci %in% Medium_bs$Name.Current.Sci), ] 


Small_bs <- Small_bs_clean %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown delphinid ",
                                   "Unknown delphinid")))

levels(Small_bs$Name.Current.Sci)

