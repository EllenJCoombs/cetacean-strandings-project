#Max latitude per species for stranding events 

library(ggplot2)
library(dplyr)

UK_IRL_stranding_events <- read.csv("UK_IRL_stranding_events.csv")
UK_and_Irish_stranding_events$X <- NULL

Lat_list <- UK_IRL_stranding_events %>%
  select(Name.Current.Sci, Latitude, Year)

Lat_list <- Lat_list %>%
  arrange(Name.Current.Sci)


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

Lat_list <- UK_IRL_stranding_events %>%
  select(Name.Current.Sci, Latitude, Year)

Lat_list <- Lat_list %>%
  arrange(Name.Current.Sci)


#Adding a new column for 0s and 1s 
Lat_list["Binary"] <- NA
Lat_list$Binary <- Lat_list$Latitude 

#Changing <55.5 = 0, >55.5 = 1 
Lat_list$Binary[Lat_list$Binary < 55.5] <- 0
Lat_list$Binary[Lat_list$Binary > 55.5] <- 1
Lat_list$Binary[Lat_list$Binary == 55.5] <- 1



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



