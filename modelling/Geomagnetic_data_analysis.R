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
require(stringr)

# Function to fix the weirdly formatted magnet data
# Natalie Cooper 2017


fix_magnet_data <- function(filename){
  
  # Read in the magnet data
  raw.data <- read_lines(filename)
  
  # Make list of column names
  # You will need to edit these 
  names.col <- c("Day","Month","Year","Day_number","3_1", "3_2", "3_3", "3_4", "3_5", "3_6", "3_7", "3_8")
  
  # Remove whitespace at the front of the first column
  # Replace spaces with commas
  # Remove excess commas
  # Split into columns using commas as delimiters
  # Outputs a list but with separators in right places
  fixed.data <- raw.data %>%
    str_trim() %>%
    str_replace_all(" ", ",") %>%
    str_replace_all(",,,,", ",") %>%
    str_replace_all(",,,", ",") %>%
    str_replace_all(",,", ",") %>%
    str_split(",")
  
  # Coerce list into a dataframe and name columns
  df1 <- setNames(do.call(rbind.data.frame, fixed.data), names.col)
  
}

# To run the code...

# Extract all the files from a folder
# Due to the weird file extensions you'll need a folder that
# contains _only_ your magnet data
files <- list.files()

# Read in the files
# Tidy them up
# Combine them all
magnet.data.ler <- map_dfr(files, fix_magnet_data)

# This may throw some warning messages about making factors 
# into characters, which isn't an issue at present
# but may need fixing later

write.csv(magnet.data.ler, file ="Geomag_ler.csv")


################################################################################################
#Cleaning up the geomag data 

#Firstly the esk.data (Eskdalemuir)
geomag_esk <- read.csv("Geomag_esk.csv")

geomag_esk <- geomag_esk %>%
  select(Day, Month, Year, Mean_daily, Max_daily)

#Take yearly max value 
#Rename variables 
esk_yearly_max <- aggregate(geomag_esk$Max_daily, by = list(geomag_esk$Year), max)
esk_yearly_max <- esk_yearly_max %>%
  rename(Year = Group.1) %>%
  rename(K_index = x)
#Plot
ggplot(data = esk_yearly_max, aes(x = Year, y = K_index, group = 1)) +
  geom_line() +
  labs(y = "Kp value")

#Hartland (Had)
geomag_had <- read.csv("Geomag_had.csv")
geomag_had <- geomag_had %>%
  select(Day, Month, Year, Mean_daily, Max_daily)

#Take yearly max value 
#Rename variables 
had_yearly_max <- aggregate(geomag_had$Max_daily, by = list(geomag_had$Year), max)
had_yearly_max <- had_yearly_max %>%
  rename(Year = Group.1) %>%
  rename(K_index = x)

ggplot(data = had_yearly_max, aes(x = Year, y = K_index, group = 1)) +
  geom_line() +
  labs(y = "Kp value")


#Lerwick (Ler) 
geomag_ler <- read.csv("Geomag_ler.csv")
geomag_ler <- geomag_ler %>%
  select(Day, Month, Year, Mean_daily, Max_daily)

#Take yearly max value 
#Rename variables 
ler_yearly_max <- aggregate(geomag_ler$Max_daily, by = list(geomag_ler$Year), max)
ler_yearly_max <- ler_yearly_max %>%
  rename(Year = Group.1) %>%
  rename(K_index = x)

ggplot(data = ler_yearly_max, aes(x = Year, y = K_index, group = 1)) +
  geom_line() +
  labs(y = "Kp value")


###############################################################################################
#Combining all the data 
#1913-1940: Had 
#1940-2015: All 

geomag_had1913 <- geomag_had %>%
  filter(Year %in% c(1913:1939))

geom_had1940 <- geomag_had %>%
  filter(Year %in% c(1940:2015))

geom_esk1940 <- geomag_esk %>%
  filter(Year %in% c(1940:2015))

geom_ler1940 <- geomag_ler %>%
  filter(Year %in% c(1940:2015))


#Combining all of the data 
geomag_1940_2015 <- bind_cols(geom_ler1940, geom_esk1940, geom_had1940)

geomag_1940_2015 <- geomag_1940_2015 %>%
  select(Day, Month, Year, Max_daily, Max_daily1, Max_daily2)

#write.csv(geomag_1940_2015, file = "Geomag_1940_2015.csv")

#Slimming the data down 
geomag_1940_2015 <- read.csv("Geomag_1940_2015.csv")
#Selecting only a few files 
geomag_1940_2015 <- geomag_1940_2015 %>%
  select(Day, Month, Year, Mean_max, Absolute_max)

#geomag_1940_2015 <- geomag_1940_2015 %>%
  #select(Day, Month, Year, Max_daily, Max_daily1, Max_daily2)

#Get yearly max 
#Rename the variables 
multiple_stations_max <- aggregate(geomag_1940_2015$Absolute_max, by = list(geomag_1940_2015$Year), max) 
multiple_stations_max <- multiple_stations_max %>%
  rename(Year = Group.1) %>%
  rename(Max_K_index = x)

#And the same for geom_had1913 
geomag_1913_max <- aggregate(geomag_had1913$Max_daily, by = list(geomag_had1913$Year), max)
#rename 
geomag_1913_max <- geomag_1913_max %>%
  rename(Year = Group.1) %>%
  rename(Max_K_index = x)

#Bind both datasets - 1913 and the 1940
All_geom_absolute_max <- bind_rows(multiple_stations_max, geomag_1913_max)
All_geom_absolute_max <- arrange(All_geom_absolute_max, Year)
  
ggplot(data = All_geom_absolute_max, aes(x = Year, y = Max_K_index, group = 1)) +
  geom_line() +
  labs(y = "Max Kp value")


write.csv(All_geom_absolute_max, file = "Geom_absolute_max.csv")

##################################################################################################
#This is doing the same as the above but taking the mean of all the maxes, rather than the 
#absolute max 

#Need geomag_1940_2015 andtake mean max from this + geomag_had1913 as these are the only records for 
#1913 - 1939 (so no mean can be taken)

#Get the mean max values from geomag_1940_2015 
geomag_1940_2015_mean_max <- aggregate(geomag_1940_2015$Mean_max, by = list(geomag_1940_2015$Year), max)
#Rename for binding 
geomag_1940_2015_mean_max<- geomag_1940_2015_mean_max %>%
rename(Year = Group.1) %>%
  rename(Max_K_index = x)

#Bind with geomag_had1913 
All_geom_mean_max <- bind_rows(geomag_1940_2015_mean_max, geomag_1913_max)
All_geom_mean_max <- arrange(All_geom_mean_max, Year)

ggplot(data = All_geom_mean_max, aes(x = Year, y = Max_K_index, group = 1)) +
  geom_line() +
  labs(y = "Max Kp value")
