
#Genus instead of species models 
#Need to split species column 

#For whole datset 
#Then for 1990s onwards 

library(dplyr)
#Seperate
UK_and_Irish_genus <- UK_and_Irish_sp %>% 
  separate(Name.Current.Sci, c('Genus', 'Species'), sep=" ")

#Remove species column 
UK_and_Irish_genus$X.1 <- NULL
UK_and_Irish_genus$X <- NULL 
UK_and_Irish_genus$Species <- NULL 

genusyearcount <- count(UK_and_Irish_genus, Genus, Year) %>%
  na.omit()

#Adding a 0 to each year without a record 
genusyearcount <- genusyearcount %>% 
  complete(Year = seq(min(1913), max(2015), 1L), Genus = unique(genusyearcount$Genus))
#NAs -> 0 
genusyearcount[is.na(genusyearcount)] <- 0

#Changing the name of the dataset 
Genus_strandings <- genusyearcount 

#Rename n to "Total_strandings"
#Had to add dplyr:: argument - I think because tidyr is 
Genus_strandings <- Genus_strandings %>% 
  dplyr::rename(Total_strandings = n)

#Same predictors as before 
#From the raw data file - no need to cut as looking at the whole dataset (1913-2015)
#Human population 
Population <- read.csv("Population_UK.csv")

#SST 
SST_yearly_max <- read.csv('SST_yearly_max.csv')

#Geomagnetic
Geom_mean_max <- read.csv('Geom_mean_max.csv')

#Storms 
Storm_data <- read.csv('Storm_data.csv')
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

#Bind the data 
All_model <- bind_cols(Population, Storm_data, Geom_mean_max, SST_yearly_max, NAO_index)
All_model$X <- NULL
All_model$Year1 <- NULL
All_model$X1 <- NULL
All_model$year <- NULL 
All_model$Year2 <-NULL
All_model$Year <-NULL
All_model$X2 <-NULL

#Variable name changes 
#Have to add dplyr:: as tidyr masks it 
All_model <- All_model %>% 
  dplyr::rename(Year = YEAR) %>%
  dplyr::rename(Population = POPULATION) %>%
  dplyr::rename(Max_SST = year_max)

#Now join the two datasets
Genus_model <- full_join(Genus_strandings, All_model, by = "Year")

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
Genus <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Genus, bs="fs") +
                        s(Storms, k=7, bs="ts") +
                        s(Max_K_index, k=5, bs="ts") +
                        s(Max_SST, bs="ts") +
                        s(NAO_index, bs="ts"), 
                      data= Genus_model, 
                      method = "REML",
                      family=tw(a=1.2))

#GAM summary and GAM plots 
summary(Genus)
par(mfrow = c(2,2))
plot(Genus)


#Gam.check
par(mfrow=c(2,2))
gam.check(Genus)


