#Attempting to look at geomagnetic data 

#Reading in geomagnetic data - these are Kp values.
#This global measure is the Kp index, a 3-hour index of mid-latitude (i.e. all longitudes) 
#activity going back to 1932.

library(dplyr)
library(tidyr)

#This is the global geomag data
geomag <- read.csv("Geomagnetic_data.csv")
labels(geomag)

sapply(geomag, class)
#Changing Date to dat (ymd)
geomag <- mutate(geomag, Date = ymd(Date))

#Take a yearly average?? 
#Firstly split the data in to year, month and day 
geomag <- separate(geomag, "Date", c("Year", "Month", "Day"), sep = "-") 
#Take average by year 
geomag_yearly_mean <- aggregate(k ~ Year, FUN=mean, data=geomag)
#monthly mean (I'm not sure yearly shows enough definition)
geomag_monthly_mean <- aggregate(k ~ Year + Month, FUN=mean, data=geomag) %>%
  arrange(Year)

#Plot monthly kp-values 
ggplot(data = geomag_monthly_mean, aes(x = Year, y = k)) +
  geom_point() +   
  labs(y = "Kp value")

#Yearly max Kp values 
geomag_yearly_max <- aggregate(geomag$k, by = list(geomag$Year), max)
#Renaming the columns
geomag_yearly_max <- geomag_yearly_max %>%
  dplyr::rename(Year = Group.1)

#Plot
ggplot(data = geomag_yearly_max, aes(x = Year, y = x, group = 1)) +
  geom_line() +
  labs(y = "Kp value")


##################################################################################################
#Reading in multiple data files from UK oberservatories 
#For Lerwick (LER) and Eskdalemuir (ESK) this is 1940 – present
#For Hartland (HAD), this folder contains data for Greenwich (GRW 1868 - 1925) 
#and Abinger (ABN 1926 - 1956) both based in and near London before these observatory 
#operations were moved to Hartland (1957 – present) in Devon when the electrification of 
#tramlines and railway disturbances made measurements in London impossible

#Each data file contains a year of K-index data. This is a 3-houly scale of local geomagnetic activity, weighted to its locality. 
#The scale ranged from 0 (quiet) – 9 (most disturbed)
#The file format is:
#Day        Month  Year       Day-number      Eight 3-hourly K-index values


require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()

#First, the Eskdalemuir (ESK) files 
files_esk <- dir(pattern = "*.esk")
files_esk

data_esk <- files_esk %>% 
 map_dfr(files_esk)

test <- dir(pattern = "*.txt")
test
data_test <- test %>%  
  map_dfr(data_test)

data_test <- test %>%
  map(read_delim, delim = "\t") %>%    
  reduce(rbind)     
data_esk



