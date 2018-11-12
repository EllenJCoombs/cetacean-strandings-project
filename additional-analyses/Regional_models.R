#Regional population and strandings 

#SW England = Devon, Corwnall, Somerset, and Dorset. Total area = 17,093 km^2
#W Scotland = Argyll, Inverclyde, Ayrshire, and Western Isles. Total area = 16,781 km^2 
#These counties were chosen because they have a shoreline, make up a fairly similar area
#for which yearly population data were available, and for which county boundaries/localities/
#borders haven't changed too significantly 


library(dplyr)
#SW England 
#POPULATION 
Regional_pop <- read.csv("County_data_UK.csv")
#Random columns of Xs 
SW_England <- Regional_pop %>%
  select(YEAR, SW.ENGLAND)

#Select strandings data from this region of the UK from 1991-2015
#Read in UK_and_Irish_sp data 

#UK_and_Irish_sp <- read.csv("UK_and_Irish_sp.csv")
#Snip data down to 1991-2015 
#STRANDINGS
SW_strandings <- UK_and_Irish_sp %>%
  filter(row_number() %in% 4114:17491)

#Filter out regional specifics 
#Argyll, Inverclyde, Ayreshire 

#Filter out the years (1913:2015)
SW_strandings <- SW_strandings %>%
  filter(County %in% c("Devon", "Somerset", "Cornwall", "North Somerset", "Dorset"))


#Count of species per year 
#Strandings count per year 1991 - 2015 
#There should be no unkwnowns 
regionalyearcount <- count(SW_strandings, Name.Current.Sci, Year) %>%
  na.omit()

#Adding a 0 to each year without a record 
#Min is now 1990
regionalyearcount <- regionalyearcount %>% 
  complete(Year = seq(min(1991), max(2015), 1L), Name.Current.Sci = unique(regionalyearcount$Name.Current.Sci))
#NAs -> 0 
regionalyearcount[is.na(regionalyearcount)] <- 0

#Changing the name of the dataset 
SW_strandings <- regionalyearcount 
#Rename n to "Total_strandings"
SW_strandings <- SW_strandings %>% 
  dplyr::rename(Total_strandings = n)
#Reaname Name.Current.Sci to Species 
SW_strandings <- SW_strandings %>%
  dplyr::rename(Species = Name.Current.Sci)


#Filter all of the covariates to 1991-2015 

#Model data (1990s - present)
#SST 
#NAO
#Geomagnetic 
#Storms 

#From the raw data file 
#Human population 
Population <- read.csv("Population_UK.csv")

#Cut to 1990
Population<- Population %>% 
  filter(row_number() %in% 79:103)

#SST 
SST_yearly_max <- read.csv('SST_yearly_max.csv')
#Cut to 1990
SST_yearly_max <- SST_yearly_max %>% 
  filter(row_number() %in% 79:103)

#Geomagnetic
Geom_mean_max <- read.csv('Geom_mean_max.csv')
Geom_mean_max <- Geom_mean_max %>% 
  filter(row_number() %in% 79:103)

#Storms 
Storms <- read.csv('Storm_data.csv')
#Select data 
Storms <- Storms %>% 
  select(Year, Count)
#Counting up and keeping 0 as 0 
Storms <- Storms %>% 
  complete(Year, fill = list(Count = 0)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(count = sum(Count))
#Renaming count to storms 
Storms <- Storms %>%
  rename(Storms = count)
#Save cleaned storms data 
#write.csv(Storms, file = "Storm_data.csv")

Storms <- Storms %>% 
  filter(row_number() %in% 79:103)

#NAO
NAO_index <- read.csv('NAO_data.csv')
NAO_index <- NAO_index %>% 
  filter(row_number() %in% 79:103)

#Regional fiahing data 
#Fishing <- read.csv("Fishing_data_UK.csv")
#Fishing <- Fishing %>% 
  #filter(row_number() %in% 79:103)

SW_model <- bind_cols(SW_England, Storms, Geom_mean_max, SST_yearly_max, NAO_index)

SW_model$X <- NULL
SW_model$X1 <- NULL 
SW_model$Year1 <- NULL
SW_model$X2 <- NULL 
SW_model$year <- NULL 
SW_model$Year <- NULL
SW_model$Year2 <- NULL 
SW_model$X <-NULL 
SW_model$Year3 <- NULL

#Rename
SW_model <- SW_model %>% 
  dplyr::rename(Year = YEAR) %>%
  dplyr::rename(Population = SW.ENGLAND) %>%
  dplyr::rename(Max_SST = year_max)#%>%
  #dplyr::rename(Fish_catch = Annual.catches..1000.tonnes.)

#Now bind all datasets 
#join the two datasets
SW_model <- full_join(SW_strandings, SW_model, by = "Year")

#Run same GAMs as before 
#install.packages("mgcv")
library(mgcv)

#This is to check how high to make the k value (k-1)
#k (almost always k-1)
unique(SW_model$Storms)
unique(SW_model$Max_K_index)
unique(SW_model$Max_SST)

#GAM for the above with Species as the factor smooth 
SW_strandings_model <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                        s(Storms, k=7, bs="ts") +
                        s(Max_K_index, k=5, bs="ts") +
                        s(Max_SST, bs="ts") +
                        s(NAO_index, bs="ts"), 
                      data= SW_model, 
                      method = "REML",
                      family=nb())

#GAM summary and GAM plots 
summary(SW_strandings_model)
par(mfrow = c(2,2))
plot(SW_strandings_model)

#Gam.check
par(mfrow=c(2,2))
gam.check(SW_strandings_model)

#family=tw(a=1.2))
#===========================================================================================
#Scotland 

library(dplyr)
#SW England 

Regional_pop <- read.csv("County_data_UK.csv")
#Random columns of Xs 
W_Scotland <- Regional_pop %>%
  select(YEAR, W.SCOTLAND)

#Select strandings data from this region of the UK from 1991-2015
#Read in UK_and_Irish_sp data 

#UK_and_Irish_sp <- read.csv("UK_and_Irish_sp.csv")
#Snip data down to 1991-2015 

Scottish_strandings <- UK_and_Irish_sp %>%
  filter(row_number() %in% 4114:17491)

#Filter out regional specifics 
#Dorset, Cornwall, Somerset, North Somerset 

#Filter out the years (1913:2015)
Scottish_strandings <- Scottish_strandings %>%
  filter(County %in% c("Ayrshire", "Argyll and Bute", "Argyll, Scotland", 
                       "Argyllshire, Scotland", "Ayrshire, Scotland", "Inverclyde",
                       "Western Isles", "Western Isles, Scotland"))


#Count of species per year 
#Strandings count per year 1991 - 2015 
#There should be no unkwnowns 
regionalyearcount <- count(Scottish_strandings, Name.Current.Sci, Year) %>%
  na.omit()

#Adding a 0 to each year without a record 
#Min is now 1990
regionalyearcount <- regionalyearcount %>% 
  complete(Year = seq(min(1991), max(2015), 1L), Name.Current.Sci = unique(regionalyearcount$Name.Current.Sci))
#NAs -> 0 
regionalyearcount[is.na(regionalyearcount)] <- 0

#Changing the name of the dataset 
Scottish_strandings <- regionalyearcount 
#Rename n to "Total_strandings"
Scottish_strandings <- Scottish_strandings %>% 
  dplyr::rename(Total_strandings = n)
#Reaname Name.Current.Sci to Species 
Scottish_strandings <- Scottish_strandings %>%
  dplyr::rename(Species = Name.Current.Sci)


#Filter all of the covariates to 1991-2015 

#Model data (1990s - present)
#SST 
#NAO
#Geomagnetic 
#Storms 

#From the raw data file 
#Human population 
Population <- read.csv("Population_UK.csv")

#Cut to 1990
Population<- Population %>% 
  filter(row_number() %in% 79:103)

#SST 
SST_yearly_max <- read.csv('SST_yearly_max.csv')
#Cut to 1990
SST_yearly_max <- SST_yearly_max %>% 
  filter(row_number() %in% 79:103)

#Geomagnetic
Geom_mean_max <- read.csv('Geom_mean_max.csv')
Geom_mean_max <- Geom_mean_max %>% 
  filter(row_number() %in% 79:103)

#Storms 
Storms <- read.csv('Storm_data.csv')
#Select data 
Storms <- Storms %>% 
  select(Year, Count)
#Counting up and keeping 0 as 0 
Storms <- Storms %>% 
  complete(Year, fill = list(Count = 0)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(count = sum(Count))
#Renaming count to storms 
Storms <- Storms %>%
  rename(Storms = count)
#Save cleaned storms data 
#write.csv(Storms, file = "Storm_data.csv")

Storms <- Storms %>% 
  filter(row_number() %in% 79:103)

#NAO
NAO_index <- read.csv('NAO_data.csv')
NAO_index <- NAO_index %>% 
  filter(row_number() %in% 79:103)

WScotland_model <- bind_cols(W_Scotland, Storms, Geom_mean_max, SST_yearly_max, NAO_index)

WScotland_model$X <- NULL
WScotland_model$X1 <- NULL 
WScotland_model$Year1 <- NULL
WScotland_model$X2 <- NULL 
WScotland_model$year <- NULL 
WScotland_model$Year <- NULL
WScotland_model$Year2 <- NULL 
WScotland_model$X <-NULL 

#Rename
WScotland_model <- WScotland_model %>% 
  dplyr::rename(Year = YEAR) %>%
  dplyr::rename(Population = W.SCOTLAND) %>%
  dplyr::rename(Max_SST = year_max)

#Now bind all datasets 
#join the two datasets
WScotland_model <- full_join(Scottish_strandings, WScotland_model, by = "Year")
WScotland_model$X <- NULL

#Run same GAMs as before 
#install.packages("mgcv")
library(mgcv)

#This is to check how high to make the k value (k-1)
#k (almost always k-1)
unique(SW_model$Storms)
unique(SW_model$Max_K_index)
unique(SW_model$Max_SST)

#GAM for the above with Species as the factor smooth 
NS_strandings_model <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                             s(Storms, k=7, bs="ts") +
                             s(Max_K_index, k=5, bs="ts") +
                             s(Max_SST, bs="ts") +
                             s(NAO_index, bs="ts"), 
                           data= WScotland_model, 
                           method = "REML",
                           family=nb())

#GAM summary and GAM plots 
summary(NS_strandings_model)
par(mfrow = c(2,2))
plot(NS_strandings_model)

#Gam.check
par(mfrow=c(2,2))
gam.check(NS_strandings_model)

#family=tw(a=1.2))

