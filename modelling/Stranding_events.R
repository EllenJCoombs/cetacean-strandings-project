#Stranding events 
library(dplyr) 
#Looking at only stranding events (rather than individual strandings)
#Based on S.W.No. 


#Read in the cleaned UK and Irish data (with unknowns removed and rare species removed)
UK_and_Irish_sp <- read.csv("UK_and_Irish_sp.csv")
UK_and_Irish_sp$X <- NULL
UK_and_Irish_sp$X.1 <- NULL

#Which ones ae duplicated? This gives a TRUE (duplicates) and FALSE (non duplicate)
duplicated(UK_and_Irish_sp$S.W.No.)
#Or you can use unique - this produces unique S.W.No 
unique(UK_and_Irish_sp$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
Stranding_events <- UK_and_Irish_sp[!duplicated(UK_and_Irish_sp$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
Stranding_events$S.W.No. <- (sub("\\.\\d+$","",Stranding_events$S.W.No.))
Stranding_events <- Stranding_events[!duplicated(Stranding_events$S.W.No.), ]

#something like sub("\\.\\d+$","","SW1234.1")
#substitute a . (\\.) followed by 1 or more digits (\\d+) followed by the end of the line ($) with nothing ("")
#replace "SW1234.1" with the column you want
#e.g., data$thingo
#then I think we used unique()
#on the data.frame?

#This deletes all duplicates 
#UK_IRL_stranding_events <- UK_IRL_stranding_events[!(duplicated(UK_IRL_stranding_events[c("Date", "Name.Current.Sci", "Latitude", "Longitude")]) | duplicated(UK_IRL_stranding_events[c("Date", "Name.Current.Sci", "Latitude", "Longitude")], fromLast = TRUE)), ]

#This deletes one of the duplicates and keeps the other based on:
#Species name, Latitude, Longitude, Date 
Stranding_events <- Stranding_events[!duplicated(Stranding_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]

#Remove unknowns if wanted 
#UK_IRL_stranding_events <- UK_IRL_stranding_events %>% 
  #filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   #"Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))

#Plot if desired 
#This plots by species 
#ggplot(Stranding_events, aes(x = Year, fill = Name.Current.Sci)) +
  #geom_histogram(binwidth = 0.5)


write.csv(Stranding_events, file = "Stranding_events.csv")


#Getting these ready for adding to the model data 
#How many stranding events per year? 
Stranding_events_count <- count(Stranding_events, Year)
Stranding_events_count <- Stranding_events_count %>%
  rename(Total_events = n)

#Saved in cleaned data in 'cleaned-data'
write.csv(Stranding_events_count, file = "Stranding_events_count.csv")
