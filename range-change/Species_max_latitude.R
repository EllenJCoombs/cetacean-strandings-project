#Max latitude per species for stranding events 

library(ggplot2)
library(dplyr)

UK_IRL_stranding_events <- read.csv("UK_IRL_stranding_events.csv")
UK_and_Irish_stranding_events$X <- NULL

Lat_list <- UK_IRL_stranding_events %>%
  select(Name.Current.Sci, Latitude, Year)

Lat_list <- Lat_list %>%
  arrange(Name.Current.Sci)

#Removing NAs (mostly missing Irish coordinates) as these end up giving the whole year an NA 
#even when other values are recorded for that year. 
#Which rows have NAs?
row.has.na <- apply(Lat_list, 1, function(x){any(is.na(x))})
#How many NAs
sum(row.has.na)
#Remove NAs
Lat_list <- Lat_list[!row.has.na,]

#Split out max latitude per species per year 
levels(Lat_list$Name.Current.Sci)[1]

Max_lat_species<-vector("list")
for(i in 1:length(levels(Lat_list$Name.Current.Sci))){
#Specific species 
Species_lat <- Lat_list %>%
  filter(Name.Current.Sci == levels(Lat_list$Name.Current.Sci)[i])


#Extracting max latitude per year 
Species_lat <- aggregate(Species_lat$Latitude, by = list(Species_lat$Year), max)%>%
  mutate(Species=levels(Lat_list$Name.Current.Sci)[i])

#Doing this for the ith species 
Max_lat_species[[i]]<-Species_lat
}

#Binding all ith species max lats per year 
Max_lat_species<-tbl_df(do.call(rbind,Max_lat_species))

#Rename the columns 
Max_lat_species <- Max_lat_species %>%
  rename(Year = Group.1) %>%
  rename(Maximum_latitude = x)

#Plot the data 
ggplot(data = Max_lat_species,
       aes(x = Year, y = Maximum_latitude, colour = Species, ylab = "Maximum stranding latitude recorded for each species")) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species)


#################################################################################################
#Range switch: 0s and 1s 
#This is per species per stranding event - looks awful! 

UK_IRL_stranding_events <- read.csv("UK_IRL_stranding_events.csv")
UK_and_Irish_stranding_events$X <- NULL


#Adding a new column for 0s and 1s 
Max_lat_species["Copy"] <- NA
Max_lat_species$Copy <- Max_lat_species$Maximum_latitude

Lat_change <- mutate(Max_lat_species, D = Copy - lag(Maximum_latitude))

Lat_change <- Lat_change %>%
  rename(Latitude_change = D)


ggplot(data = Lat_change,
       aes(x = Year, y = D, colour = Species, ylab = "Latitude change")) +
  geom_line() +
  facet_wrap(~Species)


#Adding a new column to make N1, N5, N10 (change in degrees north (latidude year on year)
#Not sure why I still get error messages: Stack overflow advised me to make a list of NAs as 
#done here 
Lat_change["N1"] <- NA
Lat_change$N1 <- Lat_change$Latitude_change

#Changing a lat change of 1 degree to a 1 and everything else to a 0 

Lat_change$N1[Lat_change$N1 > 2] <- 0
Lat_change$N1[Lat_change$N1 == 1] <- 1
Lat_change$N1[Lat_change$N1 < 1] <- 0
Lat_change$N1[Lat_change$N1 > 1 & Lat_change$N1 < 2] <- 1


library(zoo)


#Split out max latitude per species per year 
levels(Lat_list$Name.Current.Sci)[1]

Max_lat_species<-vector("list")
for(i in 1:length(levels(Lat_list$Name.Current.Sci))){
  #Specific species 
  Species_lat <- Lat_list %>%
    filter(Name.Current.Sci == levels(Lat_list$Name.Current.Sci)[i])
  
  
  #Extracting max latitude per year 
  Species_lat <- aggregate(Species_lat$Latitude, by = list(Species_lat$Year), max)%>%
    mutate(Species=levels(Lat_list$Name.Current.Sci)[i])
  
  #Doing this for the ith species 
  Max_lat_species[[i]]<-Species_lat
}

#Binding all ith species max lats per year 
Max_lat_species<-tbl_df(do.call(rbind,Max_lat_species))














#Rename variable 
Lat_list <- Lat_list %>%
  rename(Species = Name.Current.Sci)

Lat_binary <- Lat_list %>% 
  select(Species, Year, Binary)

#Plot this by species 
ggplot(data = Lat_binary,
       aes(x = Year, y = Binary, colour = Species, ylab = "South:North")) +
  geom_point() +
  facet_wrap(~Species)


###################################################################################################
#0s and 1s per max latitude for species 

Max_lat_species["Binary"] <- NA
Max_lat_species$Binary <- Max_lat_species$Maximum_latitude 

#Changing <55.5 = 0, >55.5 = 1 
Max_lat_species$Binary[Max_lat_species$Binary < 55.5] <- 0
Max_lat_species$Binary[Max_lat_species$Binary > 55.5] <- 1
Max_lat_species$Binary[Max_lat_species$Binary == 55.5] <- 1

ggplot(data = Max_lat_species,
       aes(x = Year, y = Binary, colour = Species, ylab = "South:North")) +
  geom_line()+
  facet_wrap(~Species)



