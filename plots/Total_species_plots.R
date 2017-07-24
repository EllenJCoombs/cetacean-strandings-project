
#This code is looking at some species plots - still messy 
#Splitting data into years and species and then attempting to look at mysticetes seperately 
#Messy variable names - need to clean 

install.packages("reshape")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape) 


data <- read.csv("cleaned-data/cleandatesnames.csv")
names(data)

#Getting rid of the extra X1 column - not sure where this comes from 
cleaneddata <- select(data, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Year, Date) 
levels(cleaneddata$Name.Current.Sci)

write.csv(cleaneddata, file = "cleaneddata.csv")

#Having a look at how many of each species 
select(cleaneddata, Name.Current.Sci)
#'Species' just looking at Name.Current.Sci
species <- count(cleaneddata, Name.Current.Sci)
#'Speciesyearcount' cleaned data: a count of current scientific name and year 
speciesyearcount <- count(cleaneddata, Name.Current.Sci, Year)
species_lat <- count(cleaneddata, Name.Current.Sci, Latitude)


View(species)
#This is just species and the year - no counting or sorting 
speciesyear <- select(cleaneddata, Year, Name.Current.Sci)


#Geom_line of all species for every year 
ggplot(data = speciesyearcount, aes(x = Year, y = n, colour= Name.Current.Sci))+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA)) +
  labs(x = "Year", y = "Species count") +
  geom_line() +
  scale_fill_manual(values=c("deeppink", "steelblue"), guide=FALSE) + 
  facet_wrap(~ Name.Current.Sci)


#Plot of all species - ugly histogram 
library(ggplot2)
ggplot(speciesyear, aes(x = Year)) +
  geom_histogram()

#Seperate species 
ggplot(speciesyear, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

#All species histograms seperated with better bin widths 
ggplot(speciesyear, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ Name.Current.Sci)

View(speciesyear)


#Looking at species by year - a count of each species per year
speciesbyyear <- aggregate(n ~ Name.Current.Sci, speciesyearcount, sum)
View(speciesbyyear)
#Arranging species by year by count 
speciesbyyear <- speciesyearcount %>%
  group_by(n, Year, Name.Current.Sci) %>%
  arrange(Year)


#Can't get this to work
#Showing each species with the full dataset in the background 
#Need to change the - 5 
d <- speciesbyyear      
d_bg <- d[, -1] 

ggplot(d, aes(x = Year)) +
  geom_histogram(data = d_bg, fill = "grey") +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ Name.Current.Sci) 

#....
#Changing the aesthetics 
ggplot(d, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(data = d_bg, fill = "grey", alpha = .5) +
  geom_histogram(colour = "black") +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ Name.Current.Sci) +
  guides(fill = FALSE) +  # to remove the legend
  theme_bw()              # for clean look overall



#Not sure why this isn't working...trying to plot all species with the total in the background 
a <- speciesyearcount         
a_bg <- a[, -5] 

ggplot(a, aes(x = Year, y = n, colour = Name.Current.Sci)) +
  geom_point(data = a_bg, colour = "grey", alpha = .2) +
  geom_point() + 
  facet_wrap(~ Species) +
  guides(colour = FALSE) +
  theme_bw()


#Plotting just two species together with seperate colours for both
#Need to change the scale on the X axis 

ggplot(sortedodonts, aes(x = Year, y = n, color = Name.Current.Sci)) +
  geom_point(data = , colour = "grey") +
  labs(x = "Year", y = "Count") +
  geom_point() +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA))


#Splitting out baleen whales - must be a better way to use filter....
bphysalus <- filter(speciesyear, Name.Current.Sci ==  "balaenoptera physalus") 
bactorostrata <- filter(speciesyear, Name.Current.Sci ==  "balaenoptera acutorostrata") 
bborealis <- filter(speciesyear, Name.Current.Sci ==  "balaenoptera borealis")
bmusculus <- filter(speciesyear, Name.Current.Sci ==  "balaenoptera musculus") 
unmysticete <- filter(speciesyear, Name.Current.Sci ==  "unknown mysticete") 
unbalaenopterid <- filter(speciesyear, Name.Current.Sci ==  "unknown balaenoptera") 
mysticete <- filter(speciesyear, Name.Current.Sci == "un. mystitcete")
mnovaeangliae <- filter(speciesyear, Name.Current.Sci == "megaptera novaeangliae") 



combinedmysticetes <- rbind(bphysalus, bactorostrata, bborealis, bmusculus, unmysticete, unbalaenopterid, mysticete, mnovaeangliae) 
#Putting year in ascending order 
sortedmysticetes <- arrange(combinedmysticetes,(Year))
#counting up each year - total mysts, not species specific 
sortmystcount <- count(sortedmysticetes, Year)
#Saving mysticete data to a file - this is each species occurance per year 
write.csv(sortedmysticetes, file = "mysticetes_by_year.csv")

#Selecting out odontocetes (did speciesyear (all species) - mysticetes data)
odontocetes <- speciesyear[ !(speciesyear$Name.Current.Sci %in% sortedmysticetes$Name.Current.Sci), ]
View(odontocetes)

#Ordering and combining species by year - to arrange in descending order put "desc" before the year 
sortedodonts <- arrange(odontocetes,(Year))
#Counting up each year 
sortodontscount <- count(odontocetes, Year)


#Plotting the mysticetes - really ugly 
ggplot(sortmystcount, aes(x = Year, y = n)) +
  geom_point(data = , colour = "grey") +
  labs(x = "Year", y = "Count") +
  geom_line()
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA))


#All strandings plot 
  ggplot(speciesyear, aes(x = Year)) +
    stat_count(width = 0.5)
  
  
#Mysticete plots - why does stat count work?
ggplot(sortedmysticetes, aes(x = Year)) +
  stat_count(width = 0.5) +
  facet_wrap(~ Name.Current.Sci)


#Odontocetes plot 
ggplot(sortedodonts, aes(x = Year)) +
  stat_count(width = 0.5) +
  facet_wrap(~ Name.Current.Sci)

library(ggplot2)


#Plotting all mysticetes, odontocetes and all strandings 
ggplot(speciesyear, aes(x = Year, binwidth = 0.5)) +
  geom_histogram(data = speciesyear, fill = "black", binwidth = 0.5) +
  geom_histogram(data = sortedodonts, fill = "white", alpha = 0.5, binwidth = 0.5) + 
  geom_histogram(data = sortedmysticetes, fill = "red", binwidth = 0.5, alpha = 1) 

colors()
#Don't think this worked 
bind_cols(sortodontscount, sortmystcount) 

write.csv(mysticetes, file = "Mysticetes.csv")



