#Attempting to look at geomagnetic data 

#Reading in geomagnetic data - these are Kp values.
#This global measure is the Kp index, a 3-hour index of mid-latitude (i.e. all longitudes) 
#activity going back to 1932.

library(dplyr)
library(tidyr)

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
