#Max latitude per species for stranding events 
library(readr)
library(ggplot2) 
library(gridExtra)
library(lazyeval)

# Plot whale occurrences through time
# Note that I hardcoded the Year variable due to some issues I had with ggplot
# You'd need to change this if you changed the name of that variable.
# species.name = name of the species if looking at only one
# binwidth = width of bins in histogram
# start.date and end.date - allows you to change the plotting window years
# You might want to do other things like control the y axis limits, or labels etc.


UK_IRL_stranding_events <- read.csv("UK_IRL_stranding_events.csv")
UK_and_Irish_stranding_events$X <- NULL

Lat_list <- UK_IRL_stranding_events %>%
  select(Name.Current.Sci, Latitude, Year)

Lat_list <- Lat_list %>%
  arrange(Name.Current.Sci)

#Specific species 
Species_lat <- Lat_list %>%
  filter(Name.Current.Sci == "Delphinus delphis")


#Extracting max latitude per year 
Species_lat <- aggregate(Species_lat$Latitude, by = list(Species_lat$Year), max)

ggplot(data = Species_lat,
       aes(x = Group.1, y = x)) +
  geom_point() + 
  geom_smooth()








