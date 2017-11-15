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
  dplyr::group_by(Year) %>% 
  dplyr::summarise(count = sum(Count))

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


############################################################################################
#NAO Data
#From cleaned data 
#Cleaned already 

NAO_data <- read.csv("NAO_data.csv")
NAO_data$X <- NULL

############################################################################################
#Stranding events 
#Using the stranding count data 

Stranding_events_count <- read.csv("Stranding_events_count.csv")

Stranding_events_count <- Stranding_events_count %>%
  rename(Total_events = n)
Stranding_events_count$X <- NULL


#This model dataset has all stranding events (including stranding events and richness for the
#whole dataset)
#Tying all of the data together 
All_model <- bind_cols(speciesrichness, Stranding_events_count, Population, storms, Final_geom, 
                  orgs, SST_yearly_max, NAO_data)

All_model$X <- NULL
All_model$Year1 <- NULL 
All_model$Year2 <- NULL
All_model$Year3 <- NULL
All_model$Year4 <- NULL 
All_model$year <- NULL
All_model$Year5 <- NULL
All_model$X1 <- NULL 
All_model$X2 <- NULL
All_model$X <- NULL 
All_model$Year6 <- NULL


write.csv(All_model, file = "Model_data.csv")

#Make a model parametre table for 1. Small body size (Richness, Stranding events)
#same for medium and big 
#Seperate model tables for 2. North and South (Richness, Stranding events)
#Seperate model tables for 3. Guilds, odontocetes, mysticetes (Richness, Stranding events)
#Pre and Post CSIP 4. Richness and Stranding events 


#################################################################################################
#Parvorder plots 
#Mysticetes and Odontocetes 

Mysticete_events_count <- read.csv("Mysticete_events_count.csv")  #Stranding events 
Mysticete_richness <- read.csv("Mysticete_richness.csv")   #richness 

Mysticete_model <- bind_cols(Mysticete_events_count, Mysticete_richness, Population, storms, Final_geom, 
                       orgs, SST_yearly_max, NAO_data)

Mysticete_model$X <- NULL
Mysticete_model$X1 <- NULL
Mysticete_model$Year1 <- NULL
Mysticete_model$Year2 <- NULL
Mysticete_model$Year3 <- NULL
Mysticete_model$Year4 <- NULL
Mysticete_model$Year5 <- NULL
Mysticete_model$X2 <- NULL
Mysticete_model$year <- NULL 
Mysticete_model$Year6 <- NULL

write.csv(Mysticete_model, file = "Mysticete_model.csv")

###### Odontocetes 

Odontocete_events_count <- read.csv("Odontocete_events_count.csv")  #Stranding events 
Odontocete_richness <- read.csv("Odontocete_richness.csv")   #richness 

Odontocete_model <- bind_cols(Odontocete_events_count, Odontocete_richness, Population, storms, Final_geom, 
                             orgs, SST_yearly_max, NAO_data)

Odontocete_model$X <- NULL
Odontocete_model$X1 <- NULL
Odontocete_model$Year1 <- NULL
Odontocete_model$Year2 <- NULL
Odontocete_model$Year3 <- NULL
Odontocete_model$Year4 <- NULL
Odontocete_model$Year5 <- NULL
Odontocete_model$X2 <- NULL
Odontocete_model$year <- NULL 
Odontocete_model$Year6 <- NULL

write.csv(Odontocete_model, file = "Odontocete_model.csv")


#################################################################################################
#Body size (using the above parametres but swapping in richness and stranding events for
#the different body sizes 


Big_bs_events_count <- read.csv("Big_bs_events_count.csv")  #Stranding events 
Big_bs_richness <- read.csv("Big_bs_richness.csv")   #richness 

Big_model <- bind_cols(Big_bs_events_count, Big_bs_richness, Population, storms, Final_geom, 
                         orgs, SST_yearly_max, NAO_data)

Big_model$X <- NULL
Big_model$X1 <- NULL
Big_model$Year1 <- NULL
Big_model$Year2 <- NULL
Big_model$Year3 <- NULL
Big_model$Year4 <- NULL
Big_model$Year5 <- NULL
Big_model$X2 <- NULL
Big_model$year <- NULL 
Big_model$Year6 <- NULL

write.csv(Big_model, file = "Big_model.csv")


#Medium body size 
Medium_bs_events_count <- read.csv("Medium_bs_events_count.csv")  #Stranding events 
Medium_bs_richness <- read.csv("Medium_bs_richness.csv")   #richness 

Medium_model <- bind_cols(Medium_bs_events_count, Medium_bs_richness, Population, storms, Final_geom, 
                       orgs, SST_yearly_max, NAO_data)

Medium_model$X <- NULL
Medium_model$X1 <- NULL
Medium_model$Year1 <- NULL
Medium_model$Year2 <- NULL
Medium_model$Year3 <- NULL
Medium_model$Year4 <- NULL
Medium_model$Year5 <- NULL
Medium_model$X2 <- NULL
Medium_model$year <- NULL 
Medium_model$Year6 <- NULL

write.csv(Medium_model, file = "Medium_model.csv")


#Small body size
#Read in richness and stranding event data 

Small_bs_events_count <- read.csv("Small_bs_events_count.csv")  #Stranding events 
Small_bs_richness <- read.csv("Small_bs_richness.csv")   #richness 

Small_model <- bind_cols(Small_bs_events_count, Small_bs_richness, Population, storms, Final_geom, 
                  orgs, SST_yearly_max, NAO_data)

Small_model$X <- NULL
Small_model$X1 <- NULL
Small_model$Year1 <- NULL
Small_model$Year2 <- NULL
Small_model$Year3 <- NULL
Small_model$Year4 <- NULL
Small_model$Year5 <- NULL
Small_model$X2 <- NULL
Small_model$year <- NULL 
Small_model$Year6 <- NULL

write.csv(Small_model, file = "Small_model.csv")

######
#North and South models 
North_richness <- read.csv("North_richness.csv")
North_events_count <- read.csv("North_events_count.csv")

North_model <- bind_cols(North_richness, North_events_count, Population, storms, Final_geom, 
                         orgs, SST_yearly_max, NAO_data)

North_model$X <- NULL
North_model$X1 <- NULL
North_model$Year1 <- NULL
North_model$Year2 <- NULL
North_model$Year3 <- NULL
North_model$Year4 <- NULL
North_model$Year5 <- NULL
North_model$X2 <- NULL
North_model$year <- NULL 
North_model$Year6 <- NULL

write.csv(North_model, file = "North_model.csv")

##########
#South model 
South_richness <- read.csv("South_richness.csv")
South_events_count <- read.csv("South_events_count.csv")

South_model <- bind_cols(South_richness, South_events_count, Population, storms, Final_geom, 
                         orgs, SST_yearly_max, NAO_data)

South_model$X <- NULL
South_model$X1 <- NULL
South_model$Year1 <- NULL
South_model$Year2 <- NULL
South_model$Year3 <- NULL
South_model$Year4 <- NULL
South_model$Year5 <- NULL
South_model$X2 <- NULL
South_model$year <- NULL 
South_model$Year6 <- NULL

write.csv(South_model, file = "South_model.csv")


########
#Pre and post CSIP 
#Pre = 77 years (1913 - 1989)

Pre_CSIP_events_count <- read.csv("Pre_CSIP_event_count.csv")
Pre_CSIP_richness <- read.csv("Pre_CSIP_richness.csv")

#Filtering population to 1913-1989 
Pop_pre_CSIP <- Population %>%
  filter(Year %in% c(1913:1989))

#Filtering storms pre CSIP 
Storms_pre_CSIP <- storms %>%
  filter(Year %in% c(1913:1989))

#Filtering geomagnetic data pre CSIP 
Geom_pre_CSIP <- Final_geom %>%
  filter(Year %in% c(1913:1989))

SST_pre_CSIP <- SST_yearly_max %>%
  filter(year %in% c(1913:1989))

NAO_pre_CSIP <- NAO_data %>%
  filter(Year %in% c(1913:1989))


Pre_CSIP_model <- bind_cols(Pre_CSIP_events_count, Pre_CSIP_richness, Pop_pre_CSIP, 
                            Storms_pre_CSIP, Geom_pre_CSIP, SST_pre_CSIP, NAO_pre_CSIP)
Pre_CSIP_model$X <- NULL
Pre_CSIP_model$X1 <- NULL
Pre_CSIP_model$Year1 <- NULL
Pre_CSIP_model$Year2 <- NULL
Pre_CSIP_model$Year3 <- NULL
Pre_CSIP_model$Year4 <- NULL
Pre_CSIP_model$Year5 <- NULL
Pre_CSIP_model$X2 <- NULL
Pre_CSIP_model$year <- NULL 


write.csv(Pre_CSIP_model, file = "Pre_CSIP_model.csv")

#####
#Post CSIP 
#This needs to be filtered: 1990-2015 

Post_CSIP_events_count <- read.csv("Post_CSIP_events_count.csv")
Post_CSIP_richness <- read.csv("Post_CSIP_richness.csv")


Pop_post_CSIP <- Population %>%
  filter(Year %in% c(1990:2015))

#Filtering storms pre CSIP 
Storms_post_CSIP <- storms %>%
  filter(Year %in% c(1990:2015))

#Filtering geomagnetic data pre CSIP 
Geom_post_CSIP <- Final_geom %>%
  filter(Year %in% c(1990:2015))

SST_post_CSIP <- SST_yearly_max %>%
  filter(year %in% c(1990:2015))

NAO_post_CSIP <- NAO_data %>% 
  filter(Year %in% c(1990:2015))


Post_CSIP_model <- bind_cols(Post_CSIP_events_count, Post_CSIP_richness, Pop_post_CSIP, 
                            Storms_post_CSIP, Geom_post_CSIP, SST_post_CSIP, NAO_post_CSIP)

Post_CSIP_model$X <- NULL
Post_CSIP_model$X1 <- NULL
Post_CSIP_model$Year1 <- NULL
Post_CSIP_model$Year2 <- NULL
Post_CSIP_model$Year3 <- NULL
Post_CSIP_model$Year4 <- NULL
Post_CSIP_model$Year5 <- NULL
Post_CSIP_model$X2 <- NULL
Post_CSIP_model$year <- NULL 


write.csv(Post_CSIP_model, file = "Post_CSIP_model.csv")
