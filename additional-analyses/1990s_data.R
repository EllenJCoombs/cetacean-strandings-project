#1990s data only using the data with the rare species left in as confident with species IDs

#Chop data to just 1990 onwards 
UK_and_Irish_sp <- read.csv("UK_and_Irish_sp.csv") #without rares (we've justifed why species)

#Filter data to just CSIP and IWDG (post 1990s)
#This is for running the models for just the 1990s data

#rares are excluded because we are still using a species smooth

#install.packages('dplyr')
library(dplyr)
#install.packages('tidyr')
library(tidyr)


Strandings_CSIP_IWDG <- UK_and_Irish_sp %>% 
  filter(row_number() %in% 3904:17491)

#Strandings count per year 1990 - 2015 
#There should be no unkwnowns 
speciesyearcount <- count(Strandings_CSIP_IWDG, Name.Current.Sci, Year) %>%
  na.omit()

#Adding a 0 to each year without a record 
#Min is now 1990
speciesyearcount <- speciesyearcount %>% 
  complete(Year = seq(min(1990), max(2015), 1L), Name.Current.Sci = unique(speciesyearcount$Name.Current.Sci))
#NAs -> 0 
speciesyearcount[is.na(speciesyearcount)] <- 0

#Changing the name of the dataset 
CSIP_strandings <- speciesyearcount 
#Rename n to "Total_strandings"
CSIP_strandings <- CSIP_strandings %>% 
  dplyr::rename(Total_strandings = n)
#Reaname Name.Current.Sci to Species 
CSIP_strandings <- CSIP_strandings %>%
  dplyr::rename(Species = Name.Current.Sci)

#Save if required 

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
  filter(row_number() %in% 78:103)

#SST 
SST_yearly_max <- read.csv('SST_yearly_max.csv')
#Cut to 1990
SST_yearly_max <- SST_yearly_max %>% 
  filter(row_number() %in% 78:103)

#Geomagnetic
Geom_mean_max <- read.csv('Geom_mean_max.csv')
Geom_mean_max <- Geom_mean_max %>% 
  filter(row_number() %in% 78:103)

#Storms 
Storms <- read.csv('Storm_data.csv')
Storms <- storms %>%
  rename(Storms = count)
#Save cleaned storms data 
#write.csv(Storms, file = "Storm_data.csv")

Storms <- Storms %>% 
  filter(row_number() %in% 78:103)

#NAO
NAO_index <- read.csv('NAO_data.csv')
NAO_index <- NAO_index %>% 
  filter(row_number() %in% 78:103)


Fishing <- read.csv("Fishing_data_UK.csv")
Fishing <- Fishing %>%
  filter(row_number() %in% 78:103)


Shipping <- read.csv("Shipping_data.csv")
#Select required rows 
Shipping <- Shipping %>%
  select(Year, Ship.gross.weight..tons.)

Shipping <- Shipping %>%
  filter(row_number() %in% 41:66)


#Bind all predictors together 

All_model <- bind_cols(Population, Storms, Geom_mean_max, SST_yearly_max, NAO_index, Fishing,
                       Shipping)
All_model$X <- NULL
All_model$Year1 <- NULL
All_model$X1 <- NULL
All_model$year <- NULL 
All_model$Year2 <-NULL
All_model$Year <-NULL
All_model$X2 <- NULL
All_model$Year3 <- NULL
All_model$Year4 <- NULL

#Variable name changes 
All_model <- All_model %>% 
  dplyr::rename(Year = YEAR) %>%
  dplyr::rename(Population = POPULATION) %>%
  dplyr::rename(Max_SST = year_max) %>%
  dplyr::rename(Fish_catch = Annual.catches..1000.tonnes.) %>%
  dplyr::rename(Ships_tons = Ship.gross.weight..tons.)


#Now bind all datasets 
Final_model_1990 <- full_join(CSIP_strandings, All_model, by = "Year")

#Run same GAMs as before 
#install.packages("mgcv")
library(mgcv)

#This is to check how high to make the k value (k-1)
#k (almost always k-1)
unique(Final_model_1990$Storms)
unique(Final_model_1990$Max_K_index)
unique(Final_model_1990$Max_SST)

#GAM for the above with Species as the factor smooth 
All_strand1990 <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                        s(Storms, k=7, bs="ts") +
                        s(Max_K_index, k=5, bs="ts") +
                        s(Max_SST, bs="ts") +
                        s(NAO_index, bs="ts") +
                        s(Fish_catch, bs="ts") + 
                        s(Ships_tons, bs="ts"),
                      data= Final_model_1990, 
                      method = "REML",
                      family=nb())

#GAM summary and GAM plots 
summary(All_strand1990)
par(mfrow = c(2,2))
plot(All_strand1990)

#Gam.check
par(mfrow=c(2,2))
gam.check(All_strand1990)

#family=tw(a=1.2))


