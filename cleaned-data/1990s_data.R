
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
  rename(Total_strandings = n)
#Reaname Name.Current.Sci to Species 
CSIP_strandings <- CSIP_strandings %>%
  rename(Species = Name.Current.Sci)

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
SST_yearly_mean <- read.csv('SST_yearly_max.csv')
#Cut to 1990
SST_yearly_mean <- SST_yearly_mean %>% 
  filter(row_number() %in% 78:103)

#Geomagnetic
Geom_mean_max <- read.csv('Geom_mean_max.csv')
Geom_mean_max <- Geom_mean_max %>% 
  filter(row_number() %in% 78:103)

#Storms 
Storm_data <- read.csv('Storm_data.csv')
#Select data 
storms <- Storm_data %>% 
  select(Year, Count)
#Counting up and keeping 0 as 0 
storms <- storms %>% 
  complete(Year, fill = list(Count = 0)) %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(count = sum(Count))
#Renaming count to storms 
Storms <- storms %>%
  rename(Storms = count)
#Save cleaned storms data 
#write.csv(Storms, file = "Storm_data.csv")



