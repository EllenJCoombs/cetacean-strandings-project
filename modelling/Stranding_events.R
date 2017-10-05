#Stranding events 

#Looking at only stranding events (rather than individual strandings)
#Based on S.W.No. 

UK_and_Irish <- read.csv("UK_and_Irish_strandings.csv")
UK_and_Irish$X <- NULL

duplicated(UK_and_Irish$S.W.No.)
#Or you can use unique 
unique(UK_and_Irish$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
UK_IRL_stranding_events <- UK_and_Irish[!duplicated(UK_and_Irish$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
UK_IRL_stranding_events$S.W.No. <- (sub("\\.\\d+$","",UK_IRL_stranding_events$S.W.No.))
UK_IRL_stranding_events <- UK_IRL_stranding_events[!duplicated(UK_IRL_stranding_events$S.W.No.), ]

#something like sub("\\.\\d+$","","SW1234.1")
#substitute a . (\\.) followed by 1 or more digits (\\d+) followed by the end of the line ($) with nothing ("")
#replace "SW1234.1" with the column you want
#e.g., data$thingo
#then I think we used unique()
#on the data.frame?


#Remove unknowns if wanted 
#UK_IRL_stranding_events <- UK_IRL_stranding_events %>% 
  #filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   #"Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))


ggplot(UK_IRL_stranding_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)


write.csv(UK_IRL_stranding_events, file = "Stranding_events_IRL_UK.csv")


#Getting these ready for adding to the model data 
#How many stranding events per year? 
UK_IRL_stranding_events_count <- count(UK_IRL_stranding_events, Year)
UK_IRL_stranding_events_count <- UK_IRL_stranding_events_count %>%
  rename(Count = n)

write.csv(UK_IRL_stranding_events_count, file = "Stranding_events_count_IRL_UK.csv")
