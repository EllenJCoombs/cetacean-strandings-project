#Bringing all the model parameters together 

library(dplyr)
library(tidyr)
library(zoo)

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
#Population data UK
#This is a data file that is combined with yearly strandings 

Population <- read.csv("Population_UK.csv")
Population <- Population %>%
  rename(Year = YEAR) %>%
  rename(Population = POPULATION)


###################################################################################################
#Global max magnetic data 

#library(zoo)

#geomag <- read.csv("Geomagnetic_data.csv")

#sapply(geomag, class)
#Changing Date to dat (ymd)
#geomag <- mutate(geomag, Date = ymd(Date))

#Take a yearly average?? 
#Firstly split the data in to year, month and day 
#geomag <- separate(geomag, "Date", c("Year", "Month", "Day"), sep = "-") 
#Take average by year 
#geomag_yearly_mean <- aggregate(k ~ Year, FUN=mean, data=geomag)
#monthly mean (I'm not sure yearly shows enough definition)
#geomag_monthly_mean <- aggregate(k ~ Year + Month, FUN=mean, data=geomag) %>%
  #arrange(Year)

#Yearly max Kp values 
#geomag_yearly_max <- aggregate(geomag$k, by = list(geomag$Year), max)
#Renaming the columns
#geomag_yearly_max <- geomag_yearly_max %>%
  #plyr::rename(Year = Group.1)



##################################################################################################
#Species richness 

library(vegan)
library(picante)

#Or can just put: 
#Richness <- read.csv("richness.csv")

speciesrichness <- read.csv("Richness.csv")
speciesrichness$X <- NULL 


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

SST_yearly_max <- read.csv("SST_yearly_max.csv")
SST_yearly_max <- SST_yearly_max %>% 
  rename(Max_SST = year_max)

#test <- bind_cols(speciesrichness, Population, storms, Final_geom, orgs, SST_yearly_max)

#test <- test %>% 
  #dplyr::rename(Max_SST_temp = year_max)

###############################################################################################
#Stranding events 
#Using the stranding count data 

UK_IRL_stranding_events_count <- read.csv("Stranding_events_count_IRL_UK.csv")

UK_IRL_stranding_events_count <- UK_IRL_stranding_events_count %>%
  rename(Stranding_events = Count)


#This model dataset has all stranding events (including stranding events and richness for the
#whole dataset)


#Tying all of the data together 

test <- bind_cols(speciesrichness, Population, storms, Final_geom, 
                  orgs, SST_yearly_max, UK_IRL_stranding_events_count)

test$X <- NULL
test$Year1 <- NULL 
test$Year2 <- NULL
test$Year3 <- NULL
test$Year4 <- NULL 
test$year <- NULL
test$Year5 <- NULL


write.csv(test, file = "Model_data.csv")

#Make a model parametre table for 1. Small body size (Richness, Stranding events)
#same for medium and big 
#Seperate model tables for 2. North and South (Richness, Stranding events)
#Seperate model tables for 3. Guilds, odontocetes, mysticetes (Richness, Stranding events)
#Pre and Post CSIP 4. Richness and Stranding events 

#Body size (using the above parametres but swapping in richness and stranding events for
#the different body sizes 

#Small body size first 
#Read in richness and stranding event data 

Small_bs_events_count <- read.csv("Small_bs_events_count.csv")  #Stranding events 
Small_bs_richness <- read.csv("Small_bs_richness.csv")   #richness 

Small_model <- bind_cols(Small_bs_events_count, Small_bs_richness, Population, storms, Final_geom, 
                  orgs, SST_yearly_max)

Small_model$X <- NULL
Small_model$X1 <- NULL
Small_model$Year1 <- NULL
Small_model$Year2 <- NULL
Small_model$Year3 <- NULL
Small_model$Year4 <- NULL
Small_model$Year5 <- NULL
Small_model$X2 <- NULL
Small_model$year <- NULL 

write.csv(Small_model, file = "Small_model.csv")

#North and South models 
North_richness <- read.csv("North_richness.csv")
North_events_count <- read.csv("North_events_count.csv")

North_model <- bind_cols(North_richness, North_events_count, Population, storms, Final_geom, 
                         orgs, SST_yearly_max)

North_model$X <- NULL
North_model$X1 <- NULL
North_model$Year1 <- NULL
North_model$Year2 <- NULL
North_model$Year3 <- NULL
North_model$Year4 <- NULL
North_model$Year5 <- NULL
North_model$X2 <- NULL
North_model$year <- NULL 

write.csv(North_model, file = "North_model.csv")
