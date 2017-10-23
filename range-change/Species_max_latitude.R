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

#Rounding to 1DP
Lat_change <- Lat_change %>% 
  mutate(N1 = round(N1, 0))

#Changing a lat change of 1 degree to a 1 and everything else to a 0 
#This is 1 degrees north
Lat_change$N1[Lat_change$N1 > 2] <- 0
Lat_change$N1[Lat_change$N1 == 1] <- 1
Lat_change$N1[Lat_change$N1 > 1] <- 0

#If you want to select between a range 
#Lat_change$N1[Lat_change$N1 > 1 & Lat_change$N1 < 2] <- 1

#1 - 5 degrees of movement north 
Lat_change["N5"] <- NA
Lat_change$N5 <- Lat_change$Latitude_change

#Rounding to 1DP
Lat_change <- Lat_change %>% 
  mutate(N5 = round(N5, 0))

Lat_change$N5[Lat_change$N5 == 1] <- 0
Lat_change$N5[Lat_change$N5 < 1] <- 0
Lat_change$N5[Lat_change$N5 > 5] <- 0
Lat_change$N5[Lat_change$N5 > 1] <- 1
Lat_change$N5[Lat_change$N5 == 5] <- 1
Lat_change$N5[Lat_change$N5 > 2 & Lat_change$N5 < 5] <- 1


#6 - 10 degrees of movement north 
Lat_change["N10"] <- NA
Lat_change$N10 <- Lat_change$Latitude_change

#Rounding to 1DP
Lat_change <- Lat_change %>% 
  mutate(N10 = round(N10, 0))

Lat_change$N10[Lat_change$N10 > 10] <- 0
Lat_change$N10[Lat_change$N10 == 5] <- 0
Lat_change$N10[Lat_change$N10 < 5] <- 0
Lat_change$N10[Lat_change$N10 == 6] <- 1
Lat_change$N10[Lat_change$N10 > 6 & Lat_change$N10 < 10] <- 1
Lat_change$N10[Lat_change$N10 == 10] <- 1

#+10 degrees
Lat_change["N10+"] <- NA
Lat_change$`N10+` <- Lat_change$Latitude_change

#Rounding to 1DP
Lat_change <- Lat_change %>% 
  mutate(`N10+` = round(`N10+`, 0))

Lat_change$`N10+`[Lat_change$`N10+` < 10] <- 0
Lat_change$`N10+`[Lat_change$`N10+` == 10] <- 0
Lat_change$`N10+`[Lat_change$`N10+` > 10] <- 1


ggplot(data = Lat_change,
       aes(x = Year, y = N10, colour = Species, ylab = "Latitude change")) +
  geom_line() +
  facet_wrap(~Species)



##################################################################################################
#A couple of cool papers have looked at max lat and max SST for each species 
#How to turn a matrix into a 
#SST and latitude max??

library(reshape)
library(reshape2)
library(mvbutils)
library(picante)

#Removign the copy list that I don't need 
Max_lat_species$Copy <- NULL 

Max_lat_matrix <- sample2matrix(Max_lat_species)

#Add max SST to the dataframe 
#This is in the clean data folder

SST_yearly_max <- read.csv("SST_yearly_max.csv")

#Bind
Max_SST_max_lat <- bind_cols(Max_lat_matrix, SST_yearly_max)

#Remove
Max_SST_max_lat$X <- NULL 
Max_SST_max_lat$year <- NULL 

#Rename
Max_SST_max_lat <- Max_SST_max_lat %>% 
  dplyr::rename(Year_max_SST = year_max)

#Add a year column 
Max_SST_max_lat <- cbind(Max_SST_max_lat, Year = c(1913:2015))

#This was really quick, I just did labels(Max_SST_max_lat) and copied it 
Max_SST_max_lat <- Max_SST_max_lat[,c("Year", "Year_max_SST", "Balaenoptera acutorostrata", "Balaenoptera borealis", "Balaenoptera musculus",
                        "Balaenoptera physalus", "Delphinapterus leucas", "Delphinus delphis",
                        "Globicephala melas", "Grampus griseus", "Hyperoodon ampullatus",    
                        "Kogia breviceps", "Kogia sima","Lagenodelphis hosei",     
                        "Lagenorhynchus acutus", "Lagenorhynchus albirostris", "Megaptera novaeangliae",  
                        "Mesoplodon bidens","Mesoplodon densirostris","Mesoplodon europaeus",      
                        "Mesoplodon mirus", "Monodon monoceros", "Orcinus orca",         
                        "Peponocephala electra", "Phocoena phocoena", "Physeter macrocephalus",    
                        "Pseudorca crassidens", "Stenella coeruleoalba", "Tursiops truncatus",
                        "Ziphius cavirostris")]

#Want to plot all species against year max SST (which paper was this from??)
ggplot(data = ,
       aes(x = Year, y = N10, colour = Species, ylab = "Latitude change")) +
  geom_line() +
  facet_wrap(~Species)




