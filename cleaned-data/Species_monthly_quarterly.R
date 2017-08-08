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
pphocoena <- cleaneddata %>% 
  filter(Name.Current.Sci == "Phocoena phocoena") 

#Use a variation of the following code if you want to select a specfic window 
#nw_window <- nw_scotland_strandings %>% 
  #filter(Year %in% c(1989:2000))

#Shows this datset by year but I need it by month.....
#ggplot(nw_window, aes(x = Date)) + 
  #stat_count(width = 0.5) 

pp_window <- pphocoena %>%
  filter(Year %in% c(1913:2015))


install.packages("zoo")
library(zoo)
library(dplyr)

#Splitting my data into strandings per month 
pp_window$monthYear <- as.Date(as.yearmon(pp_window$Date))
pp_window$quarterYear <- as.Date(as.yearqtr(pp_window$Date))
head(pp_window)
#Strandings gathered into months 
pp_monthly <- head(pp_window %>% group_by(monthYear) %>% summarise(n = n()), 7667)
#Strandings per quarter 
pp_quarter <- head(pp_window %>% group_by(quarterYear) %>% summarise(n = n()), 818)

#Grouping by week number - not really needed - not running properly
pp_window$week <- as.Date("1913-01-01")+7*trunc((pp_window$joinTimestamp / 1000)/(3600*24*7))

#Line plot of strandings per month (grouped)
ggplot(data = pp_monthly, aes(x = monthYear, y = n, group=1)) +
  geom_line()

ggplot(la_monthly, aes(n)) +
  geom_histogram(binwidth = 0.5)
  






