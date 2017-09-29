#Splitting the dataset into smaller sections: guilds, regions, pre/post CSIP etc 

#NB: Richness = richness
#Events = single stranding events (mass strandings taken out)

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
#By body size 
#Big = all baleens (except Minkes) + Sperm whales 
#Medium = Orca, Minke, Hyperoodon (8 -10m)
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
Small_bs_clean2 <- Small_bs_clean[ !(Small_bs_clean$Name.Current.Sci %in% Medium_bs$Name.Current.Sci), ] 


Small_bs <- Small_bs_clean2 %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))

write.csv(Small_bs, file = "Small_body_size.csv")

##################################################################################################
#Splitting size into richness and stranding events 

#Big body size richness #########################################
Big_bs_year <- dplyr::count(Big_bs, Name.Current.Sci, Year)
Big_bs_year <- Big_bs_year[c("Year","n", "Name.Current.Sci")]

Big.bs.matrix <- sample2matrix(Big_bs_year)
#Number of species per year 
specnumber(Big.bs.matrix)

#Richness double check 
Big_bs_richness <- Big_bs_year %>%
  count(Year)

Big_bs_richness <- Big_bs_richness %>%
  rename(Richness = nn)

write.csv(Big_bs_richness, file = "Big_bs_richness.csv")

#Medium body size richness #######################################
Medium_bs_year <- dplyr::count(Medium_bs, Name.Current.Sci, Year)
Medium_bs_year <- Medium_bs_year[c("Year","n", "Name.Current.Sci")]

Medium.bs.matrix <- sample2matrix(Medium_bs_year)
#Number of species per year 
specnumber(Medium.bs.matrix)

#Richness double check 
Medium_bs_richness <- Medium_bs_year %>%
  count(Year)

Medium_bs_richness <- Medium_bs_richness %>%
  rename(Richness = nn)

write.csv(Medium_bs_richness, file = "Medium_bs_richness.csv")


#Small body size richness #######################################
Small_bs_year <- dplyr::count(Small_bs, Name.Current.Sci, Year)
Small_bs_year <- Small_bs_year[c("Year","n", "Name.Current.Sci")]

Small.bs.matrix <- sample2matrix(Small_bs_year)
#Number of species per year 
specnumber(Small.bs.matrix)

#Richness double check 
Small_bs_richness <- Small_bs_year %>%
  count(Year)

Small_bs_richness <- Small_bs_richness %>%
  rename(Richness = nn)

write.csv(Small_bs_richness, file = "Small_bs_richness.csv")

#Stranding events ###############################################################################
#Do this with: Big_bs, Medium_bs and Small_bs 

duplicated(Small_bs$S.W.No.)
#Or you can use unique 
unique(Small_bs$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
Small_bs_events <- Small_bs[!duplicated(Small_bs$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
Small_bs_events$S.W.No. <- (sub("\\.\\d+$","", Small_bs_events$S.W.No.))
Small_bs_events <- Small_bs_events[!duplicated(Small_bs_events$S.W.No.), ]


ggplot(Small_bs_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)


write.csv(Medium_bs_events, file = "Medium_bs_events.csv")


################################################################################################
#Splitting North and South 
#49N - 61 N 
#Midpoint = 55.5 
#South = 49N - 55.5 
#North = 55.5 - 61N 


cleaneddata <- read.csv("cleandatesnames.csv")
cleaneddata$X.1 <- NULL
cleaneddata$X <- NULL


North_strandings <- cleaneddata %>%
  filter(Latitude > 55.5)


write.csv(North_strandings, file = "North_strandings.csv")


#South_Strandings 

South_Strandings <- cleaneddata %>%
  filter(Latitude < 55.5) %>%
  filter(Longitude < 4)

South_Strandings$X.1 <- NULL
South_Strandings$X <-NULL

write.csv(South_Strandings, file = "South_strandings.csv")


#Testing that these work
gg1+
  geom_hex(data = North_strandings, aes(y = Latitude, x= Longitude), bins = 50, alpha = 0.5, 
           colour = "blue") +
  geom_hex(data = South_Strandings, aes(y = Latitude, x= Longitude), bins = 50, alpha = 0.5, 
           colour = "red") 


#Richness South and North ########################################################
#Used the same code for both of these (North and South)
#Surprisingly the Scottish strandings in 1942 were unknown species and unknown lat/long so have been
#dropped from the Northern dataset (i.e. 0 richness for 1942)

#Remove unknowns 
North_strandings <- North_strandings %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))


North_year <- dplyr::count(North_strandings, Name.Current.Sci, Year)
North_year <- North_year[c("Year","n", "Name.Current.Sci")]

North.matrix <- sample2matrix(North_year)
#Number of species per year 
specnumber(North.matrix)

#Richness double check 
North_richness <- North_year %>%
  count(Year)

North_richness <- North_richness %>%
  rename(Richness = nn)

write.csv(North_richness, file = "North_richness.csv")

#Stranding events - North and South #######################################
duplicated(North_strandings$S.W.No.)
#Or you can use unique 
unique(North_strandings$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
North_events <- North_strandings[!duplicated(North_strandings$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
North_events$S.W.No. <- (sub("\\.\\d+$","", North_events$S.W.No.))
North_events <- North_events[!duplicated(North_events$S.W.No.), ]


ggplot(North_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)


write.csv(North_events, file = "North_events.csv")

###############################################################################################
#Post and pre-CSIP stranding events and richness 
#CSIP started in 1990

cleaneddata <- read.csv("cleandatesnames.csv")

#Post-CSIP 
Post_CSIP <- cleaneddata %>%
  filter(Year %in% c(1990:2015))

Pre_CSIP <- cleaneddata %>% 
  filter(Year %in% c(1913:1989))


#Remove unknowns 
Post_CSIP <- Post_CSIP %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))

#Remove unknowns 
Pre_CSIP <- Pre_CSIP %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))


#Richness for Pre and Post CSIP 
Pre_CSIP_year <- dplyr::count(Pre_CSIP, Name.Current.Sci, Year)
Pre_CSIP_year <- Pre_CSIP_year[c("Year","n", "Name.Current.Sci")]

Pre.matrix <- sample2matrix(Pre_CSIP_year)
#Number of species per year 
specnumber(Pre.matrix)

#Richness double check 
Pre_CSIP_richness <- Pre_CSIP_year %>%
  count(Year)

Pre_CSIP_richness <- Pre_CSIP_richness %>%
  rename(Richness = nn)

Post_CSIP$X.1 <- NULL
Post_CSIP$X <- NULL 
Pre_CSIP$X.1 <- NULL 
Pre_CSIP$X <- NULL

write.csv(Pre_CSIP_richness, file = "Pre_CSIP_richness.csv")

#Stranding events ################################################
#Done for Pre and Post CSIP 
duplicated(Post_CSIP$S.W.No.)
#Or you can use unique 
unique(Post_CSIP$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
Post_CSIP_events <- Post_CSIP[!duplicated(Post_CSIP$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
Post_CSIP_events$S.W.No. <- (sub("\\.\\d+$","", Post_CSIP_events$S.W.No.))
Post_CSIP_events <- Post_CSIP_events[!duplicated(Post_CSIP_events$S.W.No.), ]


ggplot(Pre_CSIP_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)


write.csv(Pre_CSIP_events, file = "Pre_CSIP_events.csv")


test <- count(Post_CSIP_events, Year) 
