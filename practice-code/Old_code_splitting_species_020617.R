
#This code is looking at some species plots - still messy 
#Splitting data into years and species and then attempting to look at mysticetes seperately 
#Messy variable names - need to clean 

library(dplyr)
library(tidyverse)
install.packages("dplyr")
library(ggplot2)


data <- read.csv("cleaned.data.300517.csv")
names(data)

#Getting rid of the extra X1 column - not sure where this comes from 
cleaneddata <- select(data, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Year, Date) 
levels(cleaneddata$Name.Current.Sci)


#Having a look at how many of each species 
select(cleaneddata, Name.Current.Sci)
#'Species' just looking at Name.Current.Sci
species <- count(cleaneddata, Name.Current.Sci)
#'Speciesyearcount' cleaned data: a count of current scientific name and year 
speciesyearcount <- count(cleaneddata, Name.Current.Sci, Year)


View(species)

speciesyear <- select(cleaneddata, Year, Name.Current.Sci)


#Geom_line of all species for every year year 
ggplot(data = speciesyearcount, aes(x = Year, y = n, colour= Name.Current.Sci))+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA)) +
  labs(x = "Year", y = "Species count") +
  geom_line() +
  scale_fill_manual(values=c("deeppink", "steelblue"), guide=FALSE) 


#Plot of all species 
library(ggplot2)
ggplot(speciesyear, aes(x = Year)) +
  geom_histogram()

#Seperate species (messy as there are so many and I need to sort out the names)
ggplot(speciesyear, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

#All species histograms seperated with better bin widths 
ggplot(speciesyear, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~ Name.Current.Sci)

View(speciesyear)


#Looking at species by year - count 
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


splitspeciesyear<-data.frame(speciesyear,stringsAsFactors = TRUE)
splitspeciecsyear


#Making these factors (not sure I actually need to do this...)
speciecsyear$Name.Current.Sci<-as.factor(speciesyear$Name.Current.Sci)
splitspeciesyear$Year<-as.factor(splitspeciesyear$Year)


#Splitting by year and species and calling it totals 
splitspeciesyear %>% group_by(Year)%>%group_by(Name.Current.Sci)
totals <- splitspeciesyear %>%
  group_by(Name.Current.Sci, Year)


#Not sure why this isn't working?? 
totals %>% 
  filter(Name.Current.Sci ==  "ziphius cavirostris") %>% 
  filter(Name.Current.Sci == "phocoena phocoena") 

#There is a better way of doing this!! Splitting out odontocetes 
ziphius <- filter(totals, Name.Current.Sci ==  "ziphius cavirostris") 
phocoena <- filter(totals, Name.Current.Sci == "phocoena phocoena") 
ddelphis <- filter(totals, Name.Current.Sci == "delphinus delphis") 
gmelas <- filter(totals, Name.Current.Sci == "globicephala melas") 
ggriseus <- filter(totals, Name.Current.Sci == "grampus griseus") 
hampullatus <- filter(totals, Name.Current.Sci == "hyperoodon ampullatus")


combinedodonts <- rbind(ziphius, phocoena, ddelphis, gmelas, ggriseus)
#Ordering and combining species by year - to arrange in descending order put "desc" before the year 
sortedodonts <- arrange(combinedodonts,(Year))
#Counting up each year 
sortodontscount <- count(sortdatayear, Year)


#Plotting just two species together with seperate colours for both
#Need to change the scale on the X axis 

ggplot(sortodontscount, aes(x = Year, y = n, color = Name.Current.Sci)) +
  geom_point(data = , colour = "grey") +
  labs(x = "Year", y = "Count") +
  geom_point() +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA))


#Splitting out baleen whales - must be a better way to use filter....
bphysalus <- filter(totals, Name.Current.Sci ==  "balaenoptera physalus") 
bactorostrata <- filter(totals, Name.Current.Sci ==  "balaenoptera acutorostrata") 
bborealis <- filter(totals, Name.Current.Sci ==  "balaenoptera borealis")
bmusculus <- filter(totals, Name.Current.Sci ==  "balaenoptera musculus") 
unmysticete <- filter(totals, Name.Current.Sci ==  "unknown mysticete") 
unbalaenopterid <- filter(totals, Name.Current.Sci ==  "unknown balaenoptera") 
unbalaenopterid <- filter(totals, Name.Current.Sci ==  "unknown balaenoptera") 

combinedmysticetes <- rbind(bphysalus, bactorostrata, bborealis, bmusculus, unmysticete, unbalaenopterid) 
#Putting year in ascending order 
sortedmysticetes <- arrange(combinedmysticetes,(Year))
#counting up each year 
sortmystcount <- count(sortedmysticetes, Year)


#Plotting the mysticetes - really ugly 
ggplot(sortmystcount, aes(x = Year, y = n, color = Name.Current.Sci)) +
  geom_point(data = , colour = "grey") +
  labs(x = "Year", y = "Count") +
  geom_point() +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA))


#Mysticete plots - why does stat count work???
ggplot(sortedmysticetes, aes(x = Year)) +
  stat_count(width = 0.5)











