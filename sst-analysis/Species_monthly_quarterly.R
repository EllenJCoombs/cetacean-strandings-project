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
#Have changed monthYear to Date 
ggplot(data = pp_monthly, aes(x = Date, y = n, group=1)) +
  geom_line()

ggplot(pp_monthly, aes(n)) +
  geom_histogram(binwidth = 0.5)
  

##########################################################################################
#Plotting species against temperature 
#Example here is Phocoena monthly strandings against STT monthly temps (UK averaged 
#from 14 locations)

#Monthly stranding data is from the above code 
#SST is from the HadISST_Uk.R code 

UK_Ireland_SST <- read.csv("UK_Ireland_SST.csv")

#Renaming time to Date 
UK_Ireland_SST <- UK_Ireland_SST %>%
  dplyr::rename(Date = time)

#Selecting only the UK_mean 
UK_mean_SST <- UK_Ireland_SST %>%
  select(Date, UK_mean) 

#Selecting only 1913:2015
#Need to make Data a date first 
UK_mean_SST <- mutate(UK_mean_SST, Date = ymd(Date))
sapply(UK_mean_SST, class)

#Filteeing out specific dates 
UK_mean_SST <- UK_mean_SST %>%
  filter(Date >= "1912-12-16", Date <= "2016-01-16")


#Plotting UK mean temperature 
#Need to play with the months!
bb1 <- ggplot() + 
  geom_line(data = UK_mean_SST, aes(x = Date, y = UK_mean, group = 1)) + 
  #geom_smooth(se = FALSE) +
  #scale_colour_gradient(low='yellow', high='#de2d26') +
  labs(y = "SST",
       x = "time") +
  ylim(0,17) +
  ggtitle("Daily measured sea-surface-temperature, 1870 - 2017",   
          subtitle = "UK mean") + 
  theme_classic()

bb1

#Plotting strandings against temp 

#Changing "monthYear" to "Date" in PP data 
pp_monthly <- pp_monthly %>%
  dplyr::rename(Date = monthYear)
  
  
  #PPhocoena monthly strandings and monthly SST
  ggplot() + 
    geom_line(data = UK_mean_SST, aes(x = Date, y = UK_mean)) +
    geom_line(data = pp_monthly, aes(x = Date, y = n/4)) +
    scale_y_continuous(sec.axis = sec_axis(~.*4, name = "Monthly strandings")) +
    labs(y = "SST ("~degree~"C)",
         x = "Year")
