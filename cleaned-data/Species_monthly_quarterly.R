#Splitting data into monthly/quarterly time slots 

#Using the cleaneddata dataset 
library(dplyr)
library(ggplot2)
library(lubridate)


cleaneddata <- read.csv("cleandatesnames.csv")
sapply(cleaneddata, class)

#Having problems with the data format - need dates to be 'date' class

cleaneddata <- mutate(cleaneddata, Date = dmy(Date))
#Trying this instead - works by changing all 2000s to 1900s
#cleaneddata <- mutate(cleaneddata, Date = format(as.Date(Date, "%d-%b-%y"), "19%y-%m-%d"))

write.csv(cleaneddata, file = "cleandatesnames.csv")

#Now Date should be 'Date' class 

#Selecting the chosen area (if required)
L_acutus <- cleaneddata %>% 
  filter(Name.Current.Sci == "Lagenorhynchus acutus") 

#Use a variation of the following code if you want to select a specfic window 
#nw_window <- nw_scotland_strandings %>% 
  #filter(Year %in% c(1989:2000))

#Shows this datset by year but I need it by month.....
#ggplot(nw_window, aes(x = Date)) + 
  #stat_count(width = 0.5) 

la_window <- L_acutus %>%
  filter(Year %in% c(1913:2015))


install.packages("zoo")
library(zoo)
library(dplyr)

#Splitting my data into strandings per month 
la_window$monthYear <- as.Date(as.yearmon(la_window$Date))
la_window$quarterYear <- as.Date(as.yearqtr(la_window$Date))
head(la_window)
#Strandings gathered into months 
la_monthly <- head(la_window %>% group_by(monthYear) %>% summarise(n = n()), 424)
#Strandings per quarter 
la_quarter <- head(la_window %>% group_by(quarterYear) %>% summarise(n = n()), 199)

#Grouping by week number - not really needed - not running properly
la_window$week <- as.Date("1913-03-06")+7*trunc((nw_window$joinTimestamp / 1000)/(3600*24*7))

#Line plot of strandings per month (grouped)
ggplot(data = la_monthly, aes(x = monthYear, y = n, group=1)) +
  geom_line()

ggplot() +
  

#Looking at individual years 
#Just use above code and change the years 

ggplot(data = nw_monthly, aes(x = monthYear, y = n, group=1)) +
  geom_line()

#Need to use this if plot window isn't opening
dev.off()




