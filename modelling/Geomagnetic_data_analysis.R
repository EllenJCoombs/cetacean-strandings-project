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

write.csv(magnet.data.had, file ="Geomag_ler.csv")


################################################################################################
#Cleaning up the geomag data 

#Firstly the esk.data 
geomag_esk <- read.csv("Geomag_esk.csv")

geomag_esk <- geomag_esk %>%
  select(Day, Month, Year, Mean_daily)

#Take yearly max value 
#Rename variables 
esk_yearly_max <- aggregate(geomag_esk$Mean_daily, by = list(geomag_esk$Year), max)
esk_yearly_max <- esk_yearly_max %>%
  rename(Year = Group.1) %>%
  rename(K_index = x)
#Plot
ggplot(data = esk_yearly_max, aes(x = Year, y = K_index, group = 1)) +
  geom_line() +
  labs(y = "Kp value")

#Yearly mean 


