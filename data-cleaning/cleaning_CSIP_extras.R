
#Working with the CSIP dataset only as it records bycatch, pathologist and date certainty
#which the NHM fails to do in detail 

#Code for cleaning the dates in the CSIP dataset 
#Code for renaming columns so that both datsets have the same variables 
#Code for binding the two datasets together 

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

d <- cause_death 
d_bg <- speciesyear
ggplot(d, aes(x = Year)) +
  geom_histogram(data = d_bg, fill = "grey", alpha = 0.3) +
  geom_histogram(colour = "grey") +
  xlim (1990, 2015)

ggplot(speciesyearcount, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) 

ggplot(speciesyear, aes(x = Year)) +
  geom_histogram()  



dev.off() 

