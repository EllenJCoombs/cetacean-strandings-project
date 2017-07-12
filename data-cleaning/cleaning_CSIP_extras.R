
#Working with the CSIP dataset only as it records bycatch, pathologist and date certainty
#which the NHM fails to do in detail 

#Code for cleaning the dates in the CSIP dataset 
#Code for renaming columns so that both datsets have the same variables 
#Code for binding the two datasets together 

#Should have named cause_death -> "bycatch_deaths" or similar 

#Load libraries/packages
library(dplyr)
library(tidyr)
library(ggplot2)


#Just working with CSIP data here 
#I've already cleaned this data before but need to do so again as I want to look at some 
#additional columns e.g. cause of death 
#read.csv("EDITCSIPdata.csv")
csip <- read.csv("EDITCSIPdata.csv", header = TRUE)
names(csip)

#Keep these the same as the NHM categories - for consistency 
csip <- rename(csip, County = Local.Authority)
csip <- rename(csip, Name.Common = Name.common)
csip <- rename(csip, Year = year)
csip <- rename(csip, Date = Date.Found)

#Changing date format in the NHM dataset
install.packages("lubridate")
library(lubridate)
library(dplyr)


#Changing CSIP date to YYYYMMMDD
csip$Date
csip <- mutate(csip, Date = dmy(Date))

#Tidying names - as before 
#Names are tidy in the CSIP dataset (would use code as before if not)

#Columns that I want to work with 
csipchoices <- select(csip, Name.Current.Sci, Name.Common, Latitude, Longitude, County, 
                      Date, Date.Certainty, Year, Pathologist, Cause.of.Death.Category)

write.csv(csipchoices, file = "csip_choices.csv")

#Selecting by-catch
cause_death <- csipchoices %>% 
  filter(Cause.of.Death.Category %in% c("Bycatch", "Bycatch (known)", 
                                        "Entanglement", "Entanglement (known)")) 


#BYCATCH
#grouping all to bycatch (including entanglement)
#replace didn't work

#cause_death <- replace(cause_death, "Bycatch (unknown)" , "Bycatch") %>%
#replace(cause_death, "Entanglement (unknown)", "Entanglement") 

cause_death$Cause.of.Death.Category[cause_death$Cause.of.Death.Category %in% "Bycatch (known)"] <- "Bycatch"
cause_death$Cause.of.Death.Category[cause_death$Cause.of.Death.Category %in% "Entanglement (known)"] <- "Entanglement" 

cause_death$Cause.of.Death.Category

speciesyear <- select(csipchoices, Year, Name.Current.Sci)
speciesyearcount <- count(csipchoices, Name.Current.Sci, Year)
speciesbyyear <- speciesyearcount %>%
  group_by(n, Year, Name.Current.Sci) %>%
  arrange(Year)


speciestotal <- aggregate(n ~ Name.Current.Sci, speciesyearcount, sum)

#ugly plot of total deaths and number of deaths by bycatch and with species if you put 
#in facet_wrap line 
d <- cause_death 
d_bg <- speciesyear
ggplot(d, aes(x = Year)) +
  geom_histogram(data = d_bg, fill = "grey", alpha = 0.3) +
  geom_histogram(colour = "grey") +
  xlim (1990, 2015) +
  facet_wrap(~ Name.Current.Sci)


#This is plotting species by bycatch
ggplot(cause_death, aes(x = Year)) +
  geom_histogram(colour = "grey") +
  facet_wrap(~ Name.Current.Sci)

#This is plotting each total species from CSIP
ggplot(speciesyear, aes(x = Year)) +
  geom_histogram(colour = "black") +
  facet_wrap(~ Name.Current.Sci)

#Want to look at bycatch per species with the total species deaths in background 
d <- speciesyearcount
d_bg <- cause_death
ggplot(speciesyearcount, aes(x = Year)) +
  facet_wrap(~ Name.Current.Sci) +
  geom_histogram(data = d_bg, fill = "grey", alpha = 0.3) +
  geom_histogram(colour = "grey") +
  xlim (1990, 2015) +
  facet_wrap(~ Name.Current.Sci)



#Focusing on phocoena 
pphocoena <- filter(csipchoices, Name.Current.Sci == "Phocoena phocoena")
pphocoenabycatch <- filter(cause_death, Name.Current.Sci == "Phocoena phocoena")
#Phocoena bycatch 
ggplot(pphocoenabycatch, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  theme_bw()

#phocoena all deaths and bycatch deaths 
ggplot(pphocoenabycatch, aes(x = Year)) +
  geom_histogram(data = pphocoena, fill = "grey", alpha = 0.3) +
  geom_histogram(colour = "grey") +
  xlim (1990, 2015)



#Map bycatch ###############################################################################
library(maps)
library(mapdata)

uk <- map_data("world", regions = c('UK', 'Ireland'))
uk <- map_data("uk")
ggplot() + geom_polygon(data = uk, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)


#changing map outline and colours - just for fun 
ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = NA, color = "red") +
  
  
  geom_polygon(data = ukcounty, aes(x = long, y = lat, group = group), fill = NA, color = "black")
coord_fixed(1.3)


#Plotting counties with GADM - for United Kingdom 
gadm <- readRDS("/Users/ellencoombs/Desktop/Projects/Strandings/data/GBR_adm2 (1).rds")
plot(gadm)


#Plotting counties - southern Ireland
gadmireland <- readRDS("/Users/ellencoombs/Desktop/Projects/Strandings/data/IRL_adm1.rds")
plot(gadmireland)


gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "white", color = "black") + 
  geom_polygon(data = gadm, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  geom_polygon(data = gadmireland, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) 

#Adds long and lat points 
points <- data.frame(
  long = cause_death$Longitude,
  lat = cause_death$Latitude,
  species = cause_death$Name.Current.Sci) 

#Plotting lats and longs - last line is a zoom in to Scotland - remove if wanting to see the whole dataset
gg1 +
  geom_point(data = points, aes(x = long, y = lat), color = "red", size = 0.5) +
  geom_point(data = points, aes(x = long, y = lat), color = "red", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9)) + 
  facet_wrap(~ species)


install.packages("viridis")
library(viridis)
install.packages("RColorBrewer")
library(RColorBrewer)

UKmap <- readRDS("/Users/ellencoombs/Desktop/Projects/Strandings/data/GBR_adm2 (1).rds")
IRmap <- readRDS("/Users/ellencoombs/Desktop/Projects/Strandings/data/IRL_adm1.rds")
map.df <- fortify(UKmap)
ir.df <- fortify(IRmap)

ggplot(points, aes(x = long, y = lat)) + 
  stat_density2d(aes(fill = ..level..), alpha = 1, geom ="polygon")+
  geom_point(colour = "black", size = 0.5)+
  geom_path(data = map.df, aes(x = long, y = lat, group = group), colour ="grey50")+
  geom_path(data = ir.df, aes(x = long, y = lat, group = group), colour ="grey50")+
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", space = "Lab", 
                       na.value = "grey50", guide = "colourbar") +
  xlim(-10,+2.5) +
  coord_map(xlim = c(-11,3), ylim = c(49,60.9))


########################################################################################
#Physical trauma - ship strike 

trauma <- csipchoices %>% 
  filter(Cause.of.Death.Category %in% c("Physical Trauma", 
                                        "Physical Trauma Boat/Ship Strike")) 


#Mapping these
#Adds long and lat points 
trauma_points <- data.frame(
  long = trauma$Longitude,
  lat = trauma$Latitude,
  species = trauma$Name.Current.Sci) 

gg1 +
  geom_point(data = trauma_points, aes(x = long, y = lat), color = "red", size = 0.5) +
  geom_point(data = trauma_points, aes(x = long, y = lat), color = "red", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9)) + 
  facet_wrap(~ species)


ggplot(trauma_points, aes(x = long, y = lat)) + 
  stat_density2d(aes(fill = ..level..), alpha = 1, geom ="polygon")+
  geom_point(colour = "black", size = 0.5)+
  geom_path(data = map.df, aes(x = long, y = lat, group = group), colour ="grey50")+
  geom_path(data = ir.df, aes(x = long, y = lat, group = group), colour ="grey50")+
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", space = "Lab", 
                       na.value = "grey50", guide = "colourbar") +
  xlim(-10,+2.5) +
  coord_map(xlim = c(-11,3), ylim = c(49,60.9))


dev.off() 

##############################################################################
#Sonar and military testing 

#Using the cleaneddata dataset 
library(dplyr)
library(ggplot2)

#Sleceting the chosen areas 
nw_scotland_strandings <- cleaneddata %>% 
  filter(County %in% c("Western Isles", "Highland", "Argyll and Bute", "Highland, Scotland", "Argyll and Bute, Scotland", "Western Isles, Scotland"))

#Have been using 1986 - 2005 for full window (for which military testing data is available)
nw_window <- nw_scotland_strandings %>% 
  filter(Year %in% c(1989:2000))

#Shows this datset by year but I need it by month.....
ggplot(nw_window, aes(x = Date)) + 
  stat_count(width = 0.5) 

install.packages("zoo")
library(zoo)
library(dplyr)

#Splitting my data into strandings per month 
nw_window$monthYear <- as.Date(as.yearmon(nw_window$Date))
nw_window$quarterYear <- as.Date(as.yearqtr(nw_window$Date))
head(nw_window)
#Strandings gathered into months 
nw_monthly <- head(nw_window %>% group_by(monthYear) %>% summarise(n = n()), 720)
#Strandings per quarter 
nw_quarter <- head(nw_window %>% group_by(quarterYear) %>% summarise(n = n()), 143)

#Grouping by week number - not really needed 
nw_window$week <- as.Date("1984-07-01")+7*trunc((nw_window$joinTimestamp / 1000)/(3600*24*7))

#Line plot of strandings per month (grouped)
ggplot(data = nw_monthly, aes(x = monthYear, y = n, group=1)) +
  geom_line()

#Looking at individual years 
#Just use above code and change the years 

ggplot(data = nw_monthly, aes(x = monthYear, y = n, group=1)) +
  geom_line()
  
#Need to use this if plot window isn't opening
dev.off()




