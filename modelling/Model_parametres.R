#Bringing all the model parameters together 

library(dplyr)
library(tidyr)

#1 - Storm data 

storms <- read.csv("Storm_data.csv")
#Select data 
storms <- storms %>% 
  select(Year, Count)

#Counting up and keeping 0 as 0 
storms <- storms %>% 
  complete(Year, fill = list(Count = 0)) %>% 
  group_by(Year) %>% 
  summarise(count = sum(Count))

###################################################################################################
#Population data 
#This is a data file that is combined with yearly strandings 

Population <- read.csv("Population_UK.csv")


###################################################################################################
#Global max magnetic data 

geomag <- read.csv("Geomagnetic_data.csv")

sapply(geomag, class)
#Changing Date to dat (ymd)
geomag <- mutate(geomag, Date = ymd(Date))

#Take a yearly average?? 
#Firstly split the data in to year, month and day 
geomag <- separate(geomag, "Date", c("Year", "Month", "Day"), sep = "-") 
#Take average by year 
geomag_yearly_mean <- aggregate(k ~ Year, FUN=mean, data=geomag)
#monthly mean (I'm not sure yearly shows enough definition)
geomag_monthly_mean <- aggregate(k ~ Year + Month, FUN=mean, data=geomag) %>%
  arrange(Year)

#Yearly max Kp values 
geomag_yearly_max <- aggregate(geomag$k, by = list(geomag$Year), max)
#Renaming the columns
geomag_yearly_max <- geomag_yearly_max %>%
  dplyr::rename(Year = Group.1)

write.csv(geomag_yearly_max, file = "Geomag_yearly_global_max.csv")


##################################################################################################
#Species richness 

library(vegan)
library(picante)

cleaneddata <- read.csv("cleandatesnames.csv")
speciesyearcount <- dplyr::count(cleaneddata, Name.Current.Sci, Year) %>%
  na.omit()

speciesbyyear <- aggregate(n ~ Name.Current.Sci, speciesyearcount, sum) %>%
  na.omit()

reordering <- speciesyearcount[c("Year", "n", "Name.Current.Sci")]
whale.matrix <- sample2matrix(reordering)

#Number of species per year 
specnumber(whale.matrix)
