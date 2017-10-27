#Splitting the dataset into smaller sections: guilds, regions, pre/post CSIP etc 

#NB: Richness = richness
#Events = single stranding events (mass strandings taken out)

library(dplyr)
library(tidyverse)
library(picante)


#Splitting by odontocetes and mysticetes 

UK_and_Irish <- read.csv("UK_and_Irish_strandings.csv")

#Mysitcetes

bphysalus <- filter(UK_and_Irish, Name.Current.Sci ==  "Balaenoptera physalus") 
bacutorostrata <- filter(UK_and_Irish, Name.Current.Sci ==  "Balaenoptera acutorostrata") 
bborealis <- filter(UK_and_Irish, Name.Current.Sci ==  "Balaenoptera borealis")
bmusculus <- filter(UK_and_Irish, Name.Current.Sci ==  "Balaenoptera musculus") 
unmysticete <- filter(UK_and_Irish, Name.Current.Sci ==  "Unknown mysticete") 
mnovaeangliae <- filter(UK_and_Irish, Name.Current.Sci == "Megaptera novaeangliae") 

#Selecting the mysticetes 
Mysticetes <- combinedmysticetes <- rbind(bphysalus, bacutorostrata, bborealis, bmusculus, 
                                          unmysticete, mnovaeangliae)
Mysticetes$X <- NULL
Mysticetes$X.1 <- NULL

write.csv(Mysticetes, file = "Mysticetes.csv")

#Need to remove the unknowns 
Mysticetes_known <- Mysticetes %>% 
  filter(!(Name.Current.Sci=="Unknown mysticete"))


ggplot(Mysticetes_known, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)


#Selecting out odontocetes from main dataset and then delete Mysticetes 
Odontocetes <- UK_and_Irish[ !(UK_and_Irish$Name.Current.Sci %in% Mysticetes$Name.Current.Sci), ]

write.csv(Odontocetes, file = "Odontocetes.csv")


#Mysticete and Odontocete richness 
#Have to order in this way for matrix to work

Mysticetes_year <- dplyr::count(Mysticetes_known, Name.Current.Sci, Year)

#Mysticetes_year <- Mysticetes_year %>% 
  #complete(nesting(Name.Current.Sci), Year = seq(min(1913), max(2015), 1L))
#This makes the NAs 0s
#Mysticetes_year[is.na(Big_bs_year)] <- 0


Mysticetes_year <- Mysticetes_year[c("Year","n", "Name.Current.Sci")]

Mysticete.matrix <- sample2matrix(Mysticetes_year)
#Number of species per year 
specnumber(Mysticete.matrix)

#Richness double check 
Mysticete_richness <- Mysticetes_year %>%
  count(Year)

Mysticete_richness <- Mysticete_richness %>%
  rename(Mysticete_richness = nn)

write.csv(Mysticete_richness, file = "Mysticete_richness.csv")

#I've had to change this to put 0s in - need to sort the code for this....

Mysticete_richness <- read.csv("Mysticete_richness.csv")

#Odontocete richness 
#Mysticete and Odontocete richness 
#Have to order in this way for matrix to work 

Odontocetes_known <- Odontocetes %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
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
  rename(Odontocete_richness = Richness)

write.csv(Odontocete_richness, file = "Odontocete_richness.csv")

#################################################################################################
#Odontocete and Mysticete stranding events 

#Can use duplicated for Odontocetes 
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

#Removing events 
Odontocete_events <- Odontocete_events[!duplicated(Odontocete_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]

ggplot(Odontocete_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

#Count yearly events for GAM 
Odontocete_events_count <- count(Odontocete_events, Year) 
Odontocete_events_count <- Odontocete_events_count %>%
  rename(Odontocete_events = n)

write.csv(Odontocete_events_count, file = "Odontocete_events_count.csv")

##### Mysticetes 
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

#Removing duplicates 
Mysticete_events <- Mysticete_events[!duplicated(Mysticete_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]

ggplot(Mysticete_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

#Count yearly events for GAM 
Mysticete_events_count <- count(Mysticete_events, Year) 
Mysticete_events_count <- Mysticete_events_count %>%
  rename(Mysticete_events = n)
write.csv(Mysticete_events_count, file = "Mysticete_events_count.csv")


#Have had to fill in the 0s - need to sort the code for this 

Mysticete_events_count <- read.csv("Mysticete_events_count.csv")

#################################################################################################
#By body size 
#Big = all baleens (except Minkes) + Sperm whales 
#Medium = Orca, Minke, Hyperoodon (8 -10m)
#Small = everything else (small beakers, phocoena and dolphins)


#Big 
UK_and_Irish <- read.csv("UK_and_Irish_strandings.csv")
bphysalus <- filter(UK_and_Irish, Name.Current.Sci ==  "Balaenoptera physalus") 
bborealis <- filter(UK_and_Irish, Name.Current.Sci ==  "Balaenoptera borealis")
bmusculus <- filter(UK_and_Irish, Name.Current.Sci ==  "Balaenoptera musculus") 
mnovaeangliae <- filter(UK_and_Irish, Name.Current.Sci == "Megaptera novaeangliae") 
pmacrocephalus <- UK_and_Irish %>%
  filter(Name.Current.Sci %in% c("Physeter macrocephalus", "Physeter macrocephalus "))  

Big_bs <- rbind(bphysalus, bborealis, bmusculus, mnovaeangliae, pmacrocephalus)
Big_bs$X.1 <- NULL
Big_bs$X <- NULL


#Medium 
oorca <- UK_and_Irish %>% 
  filter(Name.Current.Sci %in% c("Orcinus orca", "Orcinus orca "))
hampullatus <- filter(UK_and_Irish, Name.Current.Sci ==  "Hyperoodon ampullatus")
bacutorostrata <- filter(UK_and_Irish, Name.Current.Sci ==  "Balaenoptera acutorostrata") 

Medium_bs <- rbind(oorca, hampullatus, bacutorostrata)
Medium_bs$X.1 <- NULL
Medium_bs$X <- NULL


#Small - need to remove all of the uknowns as well 
Small_bs_clean <- UK_and_Irish[ !(UK_and_Irish$Name.Current.Sci %in% Big_bs$Name.Current.Sci), ] 
Small_bs_clean2 <- Small_bs_clean[ !(Small_bs_clean$Name.Current.Sci %in% Medium_bs$Name.Current.Sci), ] 


Small_bs <- Small_bs_clean2 %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))

Small_bs$X.1 <- NULL
Small_bs$X <- NULL 



##################################################################################################
#Splitting size into richness and stranding events 

#Big body size richness #########################################
#We have a problem with big cetaceans because they aren't recorded every year
#This will mess up the bind_cols as there needs to be a count for wach year 
#Try...

Big_bs_year <- dplyr::count(Big_bs, Name.Current.Sci, Year)

#Note - the following matrix does not show years with 0 records 
#This is for the matrix 
Big_bs_year <- Big_bs_year[c("Year","n", "Name.Current.Sci")]

#Makes the matrix for species richness 
Big.bs.matrix <- sample2matrix(Big_bs_year)

#Number of species per year 
specnumber(Big.bs.matrix)

#The following code includes years which have 0 readings (important for richness)
#Select data 
Big_bs_year <- Big_bs_year %>% 
  select(Year, n)

Big_bs_year <- na.omit(Big_bs_year)

Big_bs_year <- Big_bs_year %>%
  dplyr::count(Year)

#Adds an extra column with all the years in 
Big_bs_year <- Big_bs_year %>% 
  complete(Year = seq(min(1913), max(2015), 1L))

#NAs -> 0 
Big_bs_year[is.na(Big_bs_year)] <- 0

Big_bs_year <- Big_bs_year %>%
  dplyr::rename(Big_bs_richness = nn)

Big_bs_richness <- Big_bs_year

write.csv(Big_bs_year, file = "Big_bs_richness.csv")


#Add a year colum to the matrix and then melt
#Big.bs.matrix <- cbind(Big.bs.matrix, Year = c(1913:2015))

#Convert the matrix into a dataframe 
#Big_bs_matrix <- melt(Big.bs.matrix, measure.vars=names(Big.bs.matrix)%except%"Year")

#Big_bs_matrix <- Big_bs_matrix %>%
  #dplyr::rename(Name.Current.Sci = variable)
  
#Big_bs_matrix <- Big_bs_matrix %>%
  #filter(Name.Current.Sci %in% c("Balaenoptera physalus", "Balaenoptera musculus", "Megaptera novaeangliae",
                         #"Physeter macrocephalus", "Physeter macrocephalus ", "Balaenoptera borealis"))


#This uses count but doesn't get rid of the 0s - it counts years with 0 too 
#Big_bs_richness <- Big_bs_matrix %>% 
  #complete(Year, fill = list(value = 0)) %>% 
  #group_by() %>% 
  #summarise(count = sum(value))


#Medium body size richness #######################################
Medium_bs_year <- dplyr::count(Medium_bs, Name.Current.Sci, Year)
Medium_bs_year <- Medium_bs_year[c("Year","n", "Name.Current.Sci")]

#This misses out the years where 0s are recorded 
Medium.bs.matrix <- sample2matrix(Medium_bs_year)
#Number of species per year 
specnumber(Medium.bs.matrix)


#This code includes the 0s 
Medium_bs_year <- Medium_bs_year %>% 
  select(Year, n)

Medium_bs_year <- na.omit(Medium_bs_year)

Medium_bs_year <- Medium_bs_year %>%
  dplyr::count(Year)

#Adds an extra column with all the years in 
Medium_bs_year <- Medium_bs_year %>% 
  complete(Year = seq(min(1913), max(2015), 1L))

#NAs -> 0 
Medium_bs_year[is.na(Medium_bs_year)] <- 0

Medium_bs_year <- Medium_bs_year %>%
  dplyr::rename(Medium_bs_richness = nn)

Medium_bs_richness <- Medium_bs_year

write.csv(Medium_bs_year, file = "Medium_bs_richness.csv")


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
  rename(Small_richness = nn)

Small_bs_richness$X <- NULL

write.csv(Small_bs_richness, file = "Small_bs_richness.csv")

#Stranding events ###############################################################################
#Small BS stranding events 

duplicated(Small_bs$S.W.No.)
#Or you can use unique 
unique(Small_bs$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
Small_bs_events <- Small_bs[!duplicated(Small_bs$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
Small_bs_events$S.W.No. <- (sub("\\.\\d+$","", Small_bs_events$S.W.No.))
Small_bs_events <- Small_bs_events[!duplicated(Small_bs_events$S.W.No.), ]

#Remove duplicates 
Small_bs_events <- Small_bs_events[!duplicated(Small_bs_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]

ggplot(Small_bs_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

Small_bs_events$X.2 <- NULL
Small_bs_events$X.1 <- NULL
Small_bs_events$X <- NULL

#Need to get a count of stranding events per year 
#Small_body_stranding events 

Small_bs_events_count <- count(Small_bs_events, Year)
Small_bs_events_count <- Small_bs_events_count %>% 
  rename(Small_events = n)

write.csv(Small_bs_events_count, file = "Small_bs_events_count.csv")

###################
#Medium BS stranding events 

duplicated(Medium_bs$S.W.No.)
#Or you can use unique 
unique(Medium_bs$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
Medium_bs_events <- Medium_bs[!duplicated(Medium_bs$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
Medium_bs_events$S.W.No. <- (sub("\\.\\d+$","", Medium_bs_events$S.W.No.))
Medium_bs_events <- Medium_bs_events[!duplicated(Medium_bs_events$S.W.No.), ]

#Remove duplicates 
Medium_bs_events <- Medium_bs_events[!duplicated(Medium_bs_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]


ggplot(Medium_bs_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

Medium_bs_events$X.2 <- NULL
Medium_bs_events$X.1 <- NULL
Medium_bs_events$X <- NULL

#Need to get a count of stranding events per year 
#Medium_body_stranding events 

Medium_bs_events_count <- dplyr::count(Medium_bs_events, Year)
Medium_bs_events_count <- Medium_bs_events_count %>% 
  dplyr::rename(Medium_events = n)


#Adds an extra column with all the years in 
Medium_bs_events_count <- Medium_bs_events_count %>% 
  complete(Year = seq(min(1913), max(2015), 1L))

#NAs -> 0 
Medium_bs_events_count[is.na(Medium_bs_events_count)] <- 0

write.csv(Medium_bs_events_count, file = "Medium_bs_events_count.csv")



##############
#Big BS stranding events 

duplicated(Big_bs$S.W.No.)
#Or you can use unique 
unique(Big_bs$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
Big_bs_events <- Big_bs[!duplicated(Big_bs$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
Big_bs_events$S.W.No. <- (sub("\\.\\d+$","", Big_bs_events$S.W.No.))
Big_bs_events <- Big_bs_events[!duplicated(Big_bs_events$S.W.No.), ]

#Remove duplicates 
Big_bs_events <- Big_bs_events[!duplicated(Big_bs_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]


ggplot(Big_bs_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

Big_bs_events$X.2 <- NULL
Big_bs_events$X.1 <- NULL
Big_bs_events$X <- NULL


#Need to get a count of stranding events per year 
#Big_body_stranding events 

Big_bs_events_count <- dplyr::count(Big_bs_events, Year)
Big_bs_events_count <- Big_bs_events_count %>% 
  dplyr::rename(Big_events = n)


#Adds an extra column with all the years in 
Big_bs_events_count <- Big_bs_events_count %>% 
  complete(Year = seq(min(1913), max(2015), 1L))

#NAs -> 0 
Big_bs_events_count[is.na(Big_bs_events_count)] <- 0

write.csv(Big_bs_events_count, file = "Big_bs_events_count.csv")

################################################################################################
#Splitting North and South 
#49N - 61 N 
#Midpoint = 55.5 
#South = 49N - 55.5 
#North = 55.5 - 61N 


UK_and_Irish <- read.csv("UK_and_Irish_strandings.csv")
UK_and_Irish$X.1 <- NULL
UK_and_Irish$X <- NULL


North_strandings <- UK_and_Irish %>%
  filter(Latitude > 55.5)


#South_Strandings 
South_strandings <- UK_and_Irish %>%
  filter(Latitude < 55.5) %>%
  filter(Longitude < 4)

South_Strandings$X.1 <- NULL
South_Strandings$X <-NULL


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
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))


North_year <- dplyr::count(North_strandings, Name.Current.Sci, Year)
North_year <- North_year[c("Year","n", "Name.Current.Sci")]

North.matrix <- sample2matrix(North_year)
#Number of species per year 
specnumber(North.matrix)

#Richness double check 
North_richness <- North_year %>%
  dplyr::count(Year)

North_richness <- North_richness %>%
  dplyr::rename(North_richness = nn)


#The following code includes years which have 0 readings (important for richness)
#Select data 
North_year <- North_year %>% 
  select(Year, n)

North_year <- na.omit(North_year)

North_year <- North_year %>%
  dplyr::count(Year)

#Adds an extra column with all the years in 
North_year <- North_year %>% 
  complete(Year = seq(min(1913), max(2015), 1L))

#NAs -> 0 
North_year[is.na(North_year)] <- 0

North_year <- North_year %>%
  dplyr::rename(North_richness = nn)

North_richness <- North_year

write.csv(North_richness, file = "North_richness.csv")


####### South richness 
South_strandings <- South_strandings %>%
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))


South_year <- dplyr::count(South_strandings, Name.Current.Sci, Year)
South_year <- South_year[c("Year","n", "Name.Current.Sci")]

South.matrix <- sample2matrix(South_year)
#Number of species per year 
specnumber(South.matrix)

#Richness double check 
South_richness <- South_year %>%
  count(Year)

South_richness <- South_richness %>%
  rename(South_richness = nn)

write.csv(South_richness, file = "South_richness.csv")


#Stranding events - North and South #######################################
duplicated(North_strandings$S.W.No.)
#Or you can use unique 
unique(North_strandings$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
North_events <- North_strandings[!duplicated(North_strandings$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
North_events$S.W.No. <- (sub("\\.\\d+$","", North_events$S.W.No.))
North_events <- North_events[!duplicated(North_events$S.W.No.), ]

#Removing duplicates 
North_events <- North_events[!duplicated(North_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]


ggplot(North_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

#Count for GAMs
North_events_count <- dplyr::count(North_events, Year)
North_events_count <- North_events_count %>% 
  dplyr::rename(North_events = n)

#Adds an extra column with all the years in 
North_events_count <- North_events_count %>% 
  complete(Year = seq(min(1913), max(2015), 1L))

#NAs -> 0 
North_events_count[is.na(North_events_count)] <- 0

write.csv(North_events_count, file = "North_events_count.csv")


####South events 
duplicated(South_strandings$S.W.No.)
#Or you can use unique 
unique(South_strandings$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
South_events <- South_strandings[!duplicated(South_strandings$S.W.No.), ]


#Removing duplicates from SW (CSIP data)
South_events$S.W.No. <- (sub("\\.\\d+$","", South_events$S.W.No.))
South_events <- South_events[!duplicated(South_events$S.W.No.), ]

#Removing duplicates 
South_events <- South_events[!duplicated(South_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]

ggplot(South_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

South_events_count <- count(South_events, Year) 
South_events_count <- South_events_count %>%
  rename(South_events = n)

write.csv(South_events_count, file = "South_events_count.csv")

###############################################################################################
#Post and pre-CSIP stranding events and richness 
#CSIP started in 1990

UK_and_Irish <- read.csv("UK_and_Irish_strandings.csv")

#Post-CSIP filter  
Post_CSIP <- UK_and_Irish %>%
  filter(Year %in% c(1990:2015))
#Pre-CSIP filter 
Pre_CSIP <- UK_and_Irish %>% 
  filter(Year %in% c(1913:1989))


#Remove unknowns 
Post_CSIP <- Post_CSIP %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))

#Remove unknowns 
Pre_CSIP <- Pre_CSIP %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))


#Richness for Pre CSIP 
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

Pre_CSIP$X.1 <- NULL
Pre_CSIP$X <- NULL 
Pre_CSIP$X.1 <- NULL 
Pre_CSIP$X <- NULL

Pre_CSIP_richness <- Pre_CSIP_richness %>%
  rename(Pre_CSIP_richness = Richness)

write.csv(Pre_CSIP_richness, file = "Pre_CSIP_richness.csv")

######################
#Post CSIP richness 
#Richness for post CSIP 
Post_CSIP_year <- dplyr::count(Post_CSIP, Name.Current.Sci, Year)
Post_CSIP_year <- Post_CSIP_year[c("Year","n", "Name.Current.Sci")]

Post.matrix <- sample2matrix(Post_CSIP_year)
#Number of species per year 
specnumber(Post.matrix)

#Richness double check 
Post_CSIP_richness <- Post_CSIP_year %>%
  count(Year)

Post_CSIP_richness <- Post_CSIP_richness %>%
  rename(Post_CSIP_richness = nn)

Post_CSIP$X.1 <- NULL
Post_CSIP$X <- NULL 
Post_CSIP$X.1 <- NULL 
Post_CSIP$X <- NULL

Post_CSIP_richness$X <- NULL

write.csv(Post_CSIP_richness, file = "Post_CSIP_richness.csv")

#Stranding events ################################################
#Pre-CSIP 
duplicated(Pre_CSIP$S.W.No.)
#Or you can use unique 
unique(Pre_CSIP$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
Pre_CSIP_events <- Pre_CSIP[!duplicated(Pre_CSIP$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
Pre_CSIP_events$S.W.No. <- (sub("\\.\\d+$","", Pre_CSIP_events$S.W.No.))
Pre_CSIP_events <- Pre_CSIP_events[!duplicated(Pre_CSIP_events$S.W.No.), ]


#Removing duplicates 
Pre_CSIP_events <- Pre_CSIP_events[!duplicated(Pre_CSIP_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]


ggplot(Pre_CSIP_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

#Count number of individual stranding events per year for the GAM 
Pre_CSIP_events_count <- count(Pre_CSIP_events, Year)
Pre_CSIP_events_count <- Pre_CSIP_events_count %>%
  rename(Pre_CSIP_events = n)
write.csv(Pre_CSIP_events_count, file = "Pre_CSIP_event_count.csv")


###### Post CSIP 
#Stranding events 

#Post CSIP 
duplicated(Post_CSIP$S.W.No.)
#Or you can use unique 
unique(Post_CSIP$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
Post_CSIP_events <- Post_CSIP[!duplicated(Post_CSIP$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
Post_CSIP_events$S.W.No. <- (sub("\\.\\d+$","", Post_CSIP_events$S.W.No.))
Post_CSIP_events <- Post_CSIP_events[!duplicated(Post_CSIP_events$S.W.No.), ]


#Removing duplicates 
Post_CSIP_events <- Post_CSIP_events[!duplicated(Post_CSIP_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]



ggplot(Post_CSIP_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)


#Count number of individual stranding events per year for the GAM 
Post_CSIP_events_count <- count(Post_CSIP_events, Year) 
Post_CSIP_events_count <- Post_CSIP_events_count %>%
  rename(Post_CSIP_events = n)
write.csv(Post_CSIP_events_count, file = "Post_CSIP_events_count.csv")

