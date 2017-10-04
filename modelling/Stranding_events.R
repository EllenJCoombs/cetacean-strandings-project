#Stranding events 

#Looking at only stranding events (rather than individual strandings)
#Based on S.W.No. 

cleaneddata <- read.csv("cleandatesnames.csv")
cleaneddata$X <- NULL
cleaneddata$X.1 <- NULL 



duplicated(cleaneddata$S.W.No.)
#Or you can use unique 
unique(cleaneddata$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
stranding_events <- cleaneddata[!duplicated(cleaneddata$S.W.No.), ]
stranding_events$X.1 <- NULL
stranding_events$X <- NULL

#Removing duplicates from SW (CSIP data)
stranding_events$S.W.No. <- (sub("\\.\\d+$","",stranding_events$S.W.No.))
stranding_events <- cleaneddata[!duplicated(cleaneddata$S.W.No.), ]

#Remove unknowns 
stranding_events <- stranding_events %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))


ggplot(stranding_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)


#Getting these ready for adding to the model data 
#How many stranding events per year? 

stranding_events_count <- count(stranding_events, Year)

write.csv(stranding_events_count, file = "Stranding_events_count.csv")
