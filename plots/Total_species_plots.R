
#This code is looking at some species plots
#Splitting data into years and species and then attempting to look at mysticetes seperately 
#Messy variable names - need to clean 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape) 
library(viridis)


UK_and_Irish <- read.csv("UK_and_Irish_strandings.csv")
UK_and_Irish$X <- NULL

#write.csv(cleaneddata, file = "cleaneddata.csv")

#Having a look at how many of each species 
select(UK_and_Irish, Name.Current.Sci)

#Remove unknowns 
Strandings_known_IRL_UK <- UK_and_Irish %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))


#'Species' just looking at Name.Current.Sci
Species <- dplyr::count(Strandings_known_IRL_UK, Name.Current.Sci)
#'Speciesyearcount' cleaned data: a count of current scientific name and year 
speciesyearcount <- dplyr::count(Strandings_known_IRL_UK, Name.Current.Sci, Year) %>%
  na.omit()

#This is just species and the year - no counting or sorting 
#speciesyear <- select(cleaneddata, Year, Name.Current.Sci) %>%
  #na.omit()


#Geom_line of all species for every year 
speciesyearcount <- speciesyearcount %>%
  dplyr::rename(Species = Name.Current.Sci) 


p <- ggplot(data = speciesyearcount, aes(x = Year, y = n, colour= Species))+
  theme_bw() +
  labs(x = "Year", y = "Count") +
  geom_line() +
  theme(legend.position="bottom") +
  scale_fill_viridis(option="viridis") + 
  theme_minimal()
  
pp <- p + facet_wrap(~ Species, scales = 'free')
 
pp + theme(strip.background = element_rect(colour = "black", fill = "grey40")) +
  theme_bw()
  
  
#Looking at species by year - a count of each species per year
  speciestotal <- aggregate(n ~ Species, speciesyearcount, sum) %>%
    na.omit()

#The unknowns 
unknowns <- speciesyearcount %>%
  filter(Species %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                        "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete"))

#Geom_line of all species for every year 
#Can plot unknowns using "unknowns" 
ggplot(data = unknowns, aes(x = Year, y = n, colour= Name.Current.Sci))+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA)) +
  labs(x = "Year", y = "Species count") +
  geom_line() +
  scale_fill_manual(values=c("deeppink", "steelblue"), guide=FALSE)

