
#Shipping data 
#The best I can find is United Kingdom and Crown Dependency 
#registered trading vessels of 500 gross tons and over from 1950-2015
#Cannot find Irish equivalent 

#1950-2015 strandings only 

strandings_1950s <- UK_and_Irish_sp %>%
  filter(row_number() %in% 1486:17491)

#Count of species per year 
#Strandings count per year 1950 - 2015 
#There should be no unkwnowns 
strandings1950s <- count(strandings_1950s, Name.Current.Sci, Year) %>%
  na.omit()

#Adding a 0 to each year without a record 
#Min is now 1990
strandings1950s <- strandings1950s %>% 
  complete(Year = seq(min(1950), max(2015), 1L), Name.Current.Sci = unique(strandings1950s$Name.Current.Sci))
#NAs -> 0 
strandings1950s[is.na(strandings1950s)] <- 0

#Rename n to "Total_strandings"
strandings1950s <- strandings1950s %>% 
  dplyr::rename(Total_strandings = n)
#Reaname Name.Current.Sci to Species 
strandings1950s <- strandings1950s %>%
  dplyr::rename(Species = Name.Current.Sci)

#Predictors 
Shipping_UK <- read.csv("Shipping_data.csv")

#From the raw data file 
#Human population 
Population <- read.csv("Population_UK.csv")

#Cut to 1950
Population<- Population %>% 
  filter(row_number() %in% 38:103)

#SST 
SST_yearly_max <- read.csv('SST_yearly_max.csv')
#Cut to 1990
SST_yearly_max <- SST_yearly_max %>% 
  filter(row_number() %in% 38:103)

#Geomagnetic
Geom_mean_max <- read.csv('Geom_mean_max.csv')
Geom_mean_max <- Geom_mean_max %>% 
  filter(row_number() %in% 38:103)

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
  filter(row_number() %in% 38:103)

#NAO
NAO_index <- read.csv('NAO_data.csv')
NAO_index <- NAO_index %>% 
  filter(row_number() %in% 38:103)

#Regional fiahing data 
Fishing <- read.csv("Fishing_data_UK.csv")
Fishing <- Fishing %>% 
  filter(row_number() %in% 38:103)

#bind data with strandings 
Shipping_model <- bind_cols(Population, Storms, Geom_mean_max, SST_yearly_max, NAO_index, Fishing)

Shipping_model$X <- NULL
Shipping_model$X1 <- NULL 
Shipping_model$Year1 <- NULL
Shipping_model$X2 <- NULL 
Shipping_model$year <- NULL 
Shipping_model$Year <- NULL
Shipping_model$Year2 <- NULL 
Shipping_model$X <-NULL 
Shipping_model$Year3 <- NULL

#Rename
Shipping_model <- Shipping_model %>% 
  dplyr::rename(Year = YEAR) %>%
  dplyr::rename(Population = POPULATION) %>%
  dplyr::rename(Max_SST = year_max) %>%
  dplyr::rename(Fish_catch = Annual.catches..1000.tonnes.)

#Now bind all datasets 
#join the two datasets
Shipping1950s <- full_join(strandings1950s, Shipping_model, by = "Year")

#Run same GAMs as before 
#install.packages("mgcv")
library(mgcv)

#This is to check how high to make the k value (k-1)
#k (almost always k-1)
unique(Shipping1950s$Storms)
unique(Shipping1950s$Max_K_index)
unique(Shipping1950s$Max_SST)

#GAM for the above with Species as the factor smooth 
strandings_1950smodel <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                             s(Storms, k=7, bs="ts") +
                             s(Max_K_index, k=5, bs="ts") +
                             s(Max_SST, bs="ts") +
                             s(NAO_index, bs="ts") + 
                             s(Fish_catch, bs="ts"),
                           data= Shipping1950s, 
                           method = "REML",
                           family=nb())

#GAM summary and GAM plots 
summary(strandings_1950smodel)
par(mfrow = c(2,2))
plot(strandings_1950smodel)

#Gam.check
par(mfrow=c(2,2))
gam.check(strandings_1950smodel)

#family=tw(a=1.2))
filter(row_number() %in% 38:103)