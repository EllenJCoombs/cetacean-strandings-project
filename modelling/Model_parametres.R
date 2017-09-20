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

#Renaming count to storms 
storms <- storms %>%
  rename(Storms = count)

###################################################################################################
#Population data 
#This is a data file that is combined with yearly strandings 

Population <- read.csv("Population_UK.csv")
Population <- Population %>%
  rename(Year = YEAR) %>%
  rename(Population = POPULATION)


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

speciesrichness <- speciesyearcount %>%
  count(Year)

#rename 
speciesrichness <- speciesrichness %>%
  rename(Richness = nn)

#################################################################################################
#Geomagnetic max daily, max yearly, max station 

Final_geom <- read.csv("Geom_mean_max.csv")
Final_geom$X <- NULL

##################################################################################################
#Organisational counts 

orgs <- read.csv("Organisations.csv")
#Picked up NAs for some reason: filter and then replace count with orgs 
orgs <- orgs %>%
  filter(Year %in% c(1913:2015)) %>%
  rename(Organisations = Count)

###############################################################################################
#SST
#Yearly max recorded temperature taken from 14 different places around the UK and Ireland 

SST_yearly_max <- read.csv("SST_yearly.max.csv")
test <- bind_cols(speciesrichness, Population, storms, Final_geom, orgs, SST_yearly_max)

test <- test %>% 
  dplyr::rename(Max_SST_temp = year_max)

###############################################################################################
#Stranding events 

stranding_events_count <- read.csv("Stranding_events_count.csv")
stranding_events_count$X <- NULL

test <- bind_cols(speciesrichness, Population, storms, Final_geom, 
                  orgs, SST_yearly_max, stranding_events_count)

test$Year1 <- NULL 
test$Year2 <- NULL
test$Year3 <- NULL
test$Year4 <- NULL 
test$year <- NULL
test$Year5 <- NULL


test <- test %>% 
  dplyr::rename(Stranding_count = n)


write.csv(test, file = "Model_data.csv")



