#Splitting the dataset into smaller sections: guilds, regions, pre/post CSIP etc 

library(dplyr)
library(tidyverse)


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




