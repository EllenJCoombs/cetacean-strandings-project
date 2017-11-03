#Max latitude per species for stranding events 

library(ggplot2)
library(dplyr)


UK_IRL_stranding_events <- read.csv("UK_IRL_stranding_events.csv")
UK_IRL_stranding_events$X <- NULL

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
  dplyr::rename(Year = Group.1) %>%
  dplyr::rename(Maximum_latitude = x)

#Plot the data 
ggplot(data = Max_lat_species,
       aes(x = Year, y = Maximum_latitude, colour = Species, ylab = "Maximum stranding latitude recorded for each species")) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species)


#Mean lat per species 
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

Mean_lat_species<-vector("list")
for(i in 1:length(levels(Lat_list$Name.Current.Sci))){
  #Specific species 
  Species_lat <- Lat_list %>%
    filter(Name.Current.Sci == levels(Lat_list$Name.Current.Sci)[i])
  
  
  #Extracting max latitude per year 
  Species_lat <- aggregate(Species_lat$Latitude, by = list(Species_lat$Year), mean)%>%
    mutate(Species=levels(Lat_list$Name.Current.Sci)[i])
  
  #Doing this for the ith species 
  Mean_lat_species[[i]]<-Species_lat
}

#Binding all ith species max lats per year 
Mean_lat_species<-tbl_df(do.call(rbind,Mean_lat_species))

#Rename the columns 
Mean_lat_species <- Mean_lat_species %>%
  dplyr::rename(Year = Group.1) %>%
  dplyr::rename(Mean_latitude = x)


#Bind the two (Max and mean lat)
All_lat_species <- bind_cols(Max_lat_species, Mean_lat_species)
All_lat_species$Group.1 <- NULL
All_lat_species$Species1 <- NULL 
All_lat_species$Year1 <- NULL

#Rearrange 
All_lat_species <- All_lat_species %>% 
  select(Year, Species, Maximum_latitude, Mean_latitude)

#Bind to model dataset 
#This is model_data.csv (in the modelling folder)

Model_data <- read.csv("Model_data.csv")
Model_data$X <- NULL

Model_data_wlat <- full_join(All_lat_species, Model_data, by = "Year")

write.csv(Model_data_wlat, file = "Model_data_wlat.csv")


#Plotting these - anything interesting? 
ggplot()+ 
  geom_point(data = Model_data_wlat, aes(x = Max_SST, y = Maximum_latitude)) + 
  theme_bw() + 
  facet_wrap(~ Species)

#################################################################################################
#Range switch: 0s and 1s 
#This is per species per stranding event - looks awful! 

UK_IRL_stranding_events <- read.csv("UK_IRL_stranding_events.csv")
UK_and_Irish_stranding_events$X <- NULL

#Adding a new column for 0s and 1s 
Max_lat_species["Copy"] <- NA
Max_lat_species$Copy <- Max_lat_species$Maximum_latitude

#Taking the max lat from one year, away from the max lat from a previous year (latitude change)
Lat_change <- dplyr::mutate(Max_lat_species, D = Copy - lag(Maximum_latitude))

#Remove duplicates so R doesn't take one species away from another 
#Means I lose the first value for 1913 for each species - is this a problem?
Max_lat_species$Copy[which(duplicated(Max_lat_species$Species) == FALSE)] <- NA

Lat_change <- Lat_change %>%
  dplyr::rename(Latitude_change = D)


ggplot(data = Lat_change,
       aes(x = Year, y = Latitude_change, colour = Species, ylab = "Latitude change")) +
  geom_line() +
  geom_smooth() + 
  facet_wrap(~Species)


#Adding a new column to make N1, N5, N10 (change in degrees north (latidude year on year)
#Not sure why I still get error messages: Stack overflow advised me to make a list of NAs as 
#done here 
Lat_change["N1"] <- NA
Lat_change$N1 <- Lat_change$Latitude_change

#Rounding to 1DP
Lat_change <- Lat_change %>% 
  mutate(N1 = round(N1, 0))


#The following code looks at any latitude increase, from 1 - 10 degrees 
#Additive compared to the code below 

#Changing a lat change of 1 degree to a 1 and everything else to a 0 
#This is 1-2 degrees north
#If you want to select between a range 
Lat_change$N1[Lat_change$N1 > 1 & Lat_change$N1 < 2] <- 1
Lat_change$N1[Lat_change$N1 > 2] <- 0
Lat_change$N1[Lat_change$N1 == 1] <- 1
Lat_change$N1[Lat_change$N1 == 2] <- 1
Lat_change$N1[Lat_change$N1 < 1] <- 0


#1 - 5 degrees of movement north 
Lat_change["N5"] <- NA
Lat_change$N5 <- Lat_change$Latitude_change

#Rounding to 1DP
Lat_change <- Lat_change %>% 
mutate(N5 = round(N5, 0))

#A range of numbers 
Lat_change$N5[Lat_change$N5 > 1 & Lat_change$N5 < 5] <- 1
Lat_change$N5[Lat_change$N5 == 5] <- 1
#Lat_change$N5[Lat_change$N5 == 1] <- 0
Lat_change$N5[Lat_change$N5 < 1] <- 0
Lat_change$N5[Lat_change$N5 > 5] <- 0
#Lat_change$N5[Lat_change$N5 > 1] <- 1


#1 - 10 degrees of movement north 
Lat_change["N10"] <- NA
Lat_change$N10 <- Lat_change$Latitude_change

#Rounding to 1DP
Lat_change <- Lat_change %>% 
mutate(N10 = round(N10, 0))

Lat_change$N10[Lat_change$N10 > 1 & Lat_change$N10 < 10] <- 1
Lat_change$N10[Lat_change$N10 > 10] <- 0
Lat_change$N10[Lat_change$N10 < 1] <- 0
#Lat_change$N10[Lat_change$N10 == 5] <- 0
#Lat_change$N10[Lat_change$N10 < 5] <- 0
#Lat_change$N10[Lat_change$N10 == 6] <- 1

Lat_change$N10[Lat_change$N10 == 10] <- 1

#+10 degrees
Lat_change["N10+"] <- NA
Lat_change$`N10+` <- Lat_change$Latitude_change

#Rounding to 1DP
Lat_change <- Lat_change %>% 
mutate(`N10+` = round(`N10+`, 0))

#Just shoving in 20 as I know non of the animals have moved 20 degrees 
Lat_change$`N10+`[Lat_change$`N10+` > 1 & Lat_change$`N10+` < 20] <- 1
Lat_change$`N10+`[Lat_change$`N10+` < 1] <- 0
#Lat_change$`N10+`[Lat_change$`N10+` == 10] <- 0
#Lat_change$`N10+`[Lat_change$`N10+` > 10] <- 1

#The following code looks at each increment seperately 

#Changing a lat change of 1 degree to a 1 and everything else to a 0 
#This is 1 degrees north
#Lat_change$N1[Lat_change$N1 > 2] <- 0
#Lat_change$N1[Lat_change$N1 == 1] <- 1
#Lat_change$N1[Lat_change$N1 > 1] <- 0

#If you want to select between a range 
#Lat_change$N1[Lat_change$N1 > 1 & Lat_change$N1 < 2] <- 1

#1 - 5 degrees of movement north 
#Lat_change["N5"] <- NA
#Lat_change$N5 <- Lat_change$Latitude_change

#Rounding to 1DP
#Lat_change <- Lat_change %>% 
  #mutate(N5 = round(N5, 0))

#Lat_change$N5[Lat_change$N5 == 1] <- 0
#Lat_change$N5[Lat_change$N5 < 1] <- 0
#Lat_change$N5[Lat_change$N5 > 5] <- 0
#Lat_change$N5[Lat_change$N5 > 1] <- 1
#Lat_change$N5[Lat_change$N5 == 5] <- 1
#Lat_change$N5[Lat_change$N5 > 2 & Lat_change$N5 < 5] <- 1


#6 - 10 degrees of movement north 
#Lat_change["N10"] <- NA
#Lat_change$N10 <- Lat_change$Latitude_change

#Rounding to 1DP
#Lat_change <- Lat_change %>% 
  #mutate(N10 = round(N10, 0))

#Lat_change$N10[Lat_change$N10 > 10] <- 0
#Lat_change$N10[Lat_change$N10 == 5] <- 0
#Lat_change$N10[Lat_change$N10 < 5] <- 0
#Lat_change$N10[Lat_change$N10 == 6] <- 1
#Lat_change$N10[Lat_change$N10 > 6 & Lat_change$N10 < 10] <- 1
#Lat_change$N10[Lat_change$N10 == 10] <- 1

#+10 degrees
#Lat_change["N10+"] <- NA
#Lat_change$`N10+` <- Lat_change$Latitude_change

#Rounding to 1DP
#Lat_change <- Lat_change %>% 
  #mutate(`N10+` = round(`N10+`, 0))

#Lat_change$`N10+`[Lat_change$`N10+` < 10] <- 0
#Lat_change$`N10+`[Lat_change$`N10+` == 10] <- 0
#Lat_change$`N10+`[Lat_change$`N10+` > 10] <- 1


#ggplot(data = Lat_change,
       #aes(x = Year, y = N10, colour = Species, ylab = "Latitude change")) +
  #geom_line() +
  #facet_wrap(~Species)



#Plotting the above 

Lat_change_N <- Lat_change %>%
  dplyr::select(Year, Species, N1, N5, N10,`N10+`)

#Measuring success (move north) and failure (move south or no move north) using binomial

model1 <- glm(formula = `N10+` ~ Year * Latitude_change, family = binomial, data = Lat_change)
          
summary(model1)


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

#Want to plot all species against year max SST (which paper was this from??) *See Chapter 1 draft
#There must be a better way of doing this....
#Having to rename eveeyhing so it will plot 
#Remove 0s?? 0s are messing up my plots 

Max_SST_max_lat[Max_SST_max_lat == 0] <- NA

Max_SST_max_lat <- Max_SST_max_lat %>%
  dplyr::rename(Balaenoptera_acutorostrata = "Balaenoptera acutorostrata")

ggplot() + 
  geom_line(data = Max_SST_max_lat, aes(x = Year_max_SST, y = Balaenoptera_acutorostrata, colour = "Balaenoptera acutorostrata")) +
  ylab("Max lat") +
  theme_bw()

#Adding a column to Max_lat_species 
#How do I make this so that each year has a specific SST?
Max_lat_species$SST<-NA

