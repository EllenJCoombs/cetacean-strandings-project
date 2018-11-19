
#Models with rare species kept in 

#Datasets are in the cleaned-data folder 
#Keep the rare species in 

All_strandings_data <- read.csv("UK_and_Irish_strandings.csv")

#This is 20155 records including unknowns 
#This runs the model with the unknows in it as well - unknowns are grouped 

speciesyearcount <- count(All_strandings_data, Name.Current.Sci, Year) %>%
  na.omit()

#Adding a 0 to each year without a record 
speciesyearcount <- speciesyearcount %>% 
  complete(Year = seq(min(1913), max(2015), 1L), Name.Current.Sci = unique(speciesyearcount$Name.Current.Sci))
#NAs -> 0 
speciesyearcount[is.na(speciesyearcount)] <- 0

#Rename n to "Total_strandings"
#Had to add dplyr:: argument - I think because tidyr is 
speciesyearcount <- speciesyearcount %>% 
  dplyr::rename(Total_strandings = n) %>%
  dplyr::rename(Species = Name.Current.Sci)

#Same predictors as before 
#From the raw data file - no need to cut as looking at the whole dataset (1913-2015)
#Human population 
Population <- read.csv("Population_UK.csv")

#SST 
SST_yearly_max <- read.csv('SST_yearly_max.csv')

#Geomagnetic
Geom_mean_max <- read.csv('Geom_mean_max.csv')

#Storms 
Storms <- read.csv('Storm_data.csv')
#Run the below code if the data aren't cleaned 
#Select data 
#storms <- Storm_data %>% 
#select(Year, Count)
#Counting up and keeping 0 as 0 
#storms <- storms %>% 
#complete(Year, fill = list(Count = 0)) %>% 
#dplyr::group_by(Year) %>% 
#dplyr::summarise(count = sum(Count))
#Renaming count to storms 
#Storms <- storms %>%
#rename(Storms = count)
#Save cleaned storms data 
#write.csv(Storms, file = "Storm_data.csv")

#NAO
NAO_index <- read.csv('NAO_data.csv')

#Fishing 
Fishing <- read.csv("Fishing_data_UK.csv")


#Bind the data 
All_model <- bind_cols(Population, Storm_data, Geom_mean_max, SST_yearly_max, NAO_index, 
                       Fishing)
All_model$X <- NULL
All_model$Year1 <- NULL
All_model$X1 <- NULL
All_model$year <- NULL 
All_model$Year2 <-NULL
All_model$Year <-NULL
All_model$X2 <-NULL
All_model$Year3 <- NULL 

#Variable name changes 
#Have to add dplyr:: as tidyr masks it 
All_model <- All_model %>% 
  dplyr::rename(Year = YEAR) %>%
  dplyr::rename(Population = POPULATION) %>%
  dplyr::rename(Max_SST = year_max) %>%
  dplyr::rename(Fish_catch = Annual.catches..1000.tonnes.)

#Now join the two datasets
All_strandings_model <- full_join(speciesyearcount, All_model, by = "Year")


#Run same GAMs as before 
#install.packages("mgcv")
library(mgcv)
#Running same as orignal GAM but with genus level 
#This is to check how high to make the k value (k-1)
#k (almost always k-1)
unique(Genus_model$Storms)
unique(Genus_model$Max_K_index)
unique(Genus_model$Max_SST)
unique(Genus_model$Genus)

#GAM for the above with Species as the factor smooth 
All_rare <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
               s(Storms, k=7, bs="ts") +
               s(Max_K_index, k=4, bs="ts") +
               s(Max_SST, bs="ts") +
               s(NAO_index, bs="ts") + 
               s(Fish_catch, bs="ts"),
             data= All_strandings_model, 
             method = "REML",
             family=tw(a=1.2))

#GAM summary and GAM plots 
summary(All_rare)
par(mfrow = c(2,2))
plot(All_rare)

#Gam.check
par(mfrow=c(2,2))
gam.check(All_rare)

#family=tw(a=1.2))


