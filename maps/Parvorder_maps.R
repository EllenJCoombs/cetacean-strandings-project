

library(maps) 
library(mapdata)
library(tidyverse)


latlong <- select(cleaneddata, Year, Name.Current.Sci, Longitude, Latitude)
bphysalus <- filter(latlong, Name.Current.Sci ==  "balaenoptera physalus") 
bactorostrata <- filter(latlong, Name.Current.Sci ==  "balaenoptera acutorostrata") 
bborealis <- filter(latlong, Name.Current.Sci ==  "balaenoptera borealis")
bmusculus <- filter(latlong, Name.Current.Sci ==  "balaenoptera musculus") 
unmysticete <- filter(latlong, Name.Current.Sci ==  "unknown mysticete") 
unbalaenopterid <- filter(latlong, Name.Current.Sci ==  "unknown balaenoptera") 
mysticete <- filter(latlong, Name.Current.Sci == "un. mystitcete")
mnovaeangliae <- filter(latlong, Name.Current.Sci == "megaptera novaeangliae") 

#bind all mysticetes
combinedmysticetes <- rbind(bphysalus, bactorostrata, bborealis, bmusculus, unmysticete, unbalaenopterid, mysticete, mnovaeangliae)
#arranging by year
sortedmysticetes <- arrange(combinedmysticetes,(Year))
#Stripping out odontocetes 
odontocetes <- latlong[ !(latlong$Name.Current.Sci %in% sortedmysticetes$Name.Current.Sci), ]
View(odontocetes)

#Ordering and combining species by year - to arrange in descending order put "desc" before the year 
sortedodonts <- arrange(odontocetes,(Year))

write.csv(sortedmysticetes, file = "mysticetes_map_data.csv")
write.csv(sortedodonts, file = "odontocetes_map_data.csv")


#Mysticetes by year using Natalie's function 
mysticetemap <- read.csv("mysticetes_map_data.csv")
# Extract map data
uk <- map_data("world", regions = c('UK', 'Ireland'))
# Create base map
base.map <- 
  ggplot() + 
  # Add country polygons
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") + 
  # Define coordinates to plot between
  coord_map(xlim = c(-11, 3), ylim = c(49, 60.9)) +
  # Remove grey background
  theme_bw() +
  # Remove x and y axes labels and tick marks
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# Create a list for the start years of each decade
decades <- c(seq(from = 1913, to = 2003, by = 10), 2013)
# Make an empty list that you're going to put all your
# ggplot maps into
map.list <- list()
# Loop through each of the starting years in "decades"
for(i in seq_along(decades)){
  
  # Define the start and end year of the decade
  start.year <- decades[i]
  end.year <- decades[i] + 9
  # Add a short if statement so that you don't go into the future!
  if(end.year > 2017){
    end.year <- 2017
  }
  
  
  # Use filter to select just the records for that decade
  one.decade <- filter(mysticetemap, Year >= start.year & Year <= end.year)
  
  # Add the points to the base map
  # Create a different map for each decade with a different name
  # placed into map.list
  map.list[[i]] <- 
    base.map +
    geom_point(data = one.decade, aes(x = Longitude, y = Latitude),
               color = "red", size = 0.5) +
    # Add title with years covered
    labs(title = paste(start.year, "-", end.year))
} # end loop
# Plot all the plots in map.list
do.call(grid.arrange, map.list)



#Odontocetes by year using Natalie's function 
odontocetemap <- read.csv("odontocetes_map_data.csv")
# Extract map data
uk <- map_data("world", regions = c('UK', 'Ireland'))
# Create base map
base.map <- 
  ggplot() + 
  # Add country polygons
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") + 
  # Define coordinates to plot between
  coord_map(xlim = c(-11, 3), ylim = c(49, 60.9)) +
  # Remove grey background
  theme_bw() +
  # Remove x and y axes labels and tick marks
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# Create a list for the start years of each decade
decades <- c(seq(from = 1913, to = 2003, by = 10), 2013)
# Make an empty list that you're going to put all your
# ggplot maps into
map.list <- list()
# Loop through each of the starting years in "decades"
for(i in seq_along(decades)){
  
  # Define the start and end year of the decade
  start.year <- decades[i]
  end.year <- decades[i] + 9
  # Add a short if statement so that you don't go into the future!
  if(end.year > 2017){
    end.year <- 2017
  }
  
  # Use filter to select just the records for that decade
  one.decade <- filter(odontocetemap, Year >= start.year & Year <= end.year)
  
  # Add the points to the base map
  # Create a different map for each decade with a different name
  # placed into map.list
  map.list[[i]] <- 
    base.map +
    geom_point(data = one.decade, aes(x = Longitude, y = Latitude),
               color = "red", size = 0.5) +
    # Add title with years covered
    labs(title = paste(start.year, "-", end.year))
} # end loop
# Plot all the plots in map.list
do.call(grid.arrange, map.list)



#Plotting both nysticetes and odontocetes (gg1 section to plot if this isn't in the Environment already)
gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  #Add in counties if needed 
  #geom_polygon(data = gadm, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  #geom_polygon(data = gadmireland, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) 


gg1 +  
  geom_point(data = sortedmysticetes, aes(x = Longitude, y = Latitude, colour = "red"), size = 0.5) + 
  geom_point(data = sortedodonts, aes(x = Longitude, y = Latitude), size = 0.5) + 
  coord_map(xlim=c(-11,3), ylim=c(49,60.9)) 


#unknowns 
uk <- map_data("world", regions = c('UK', 'Ireland'))
# Create base map
base.map <- 
  ggplot() + 
  # Add country polygons
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") + 
  # Define coordinates to plot between
  coord_map(xlim = c(-11, 3), ylim = c(49, 60.9)) +
  # Remove grey background
  theme_bw() +
  # Remove x and y axes labels and tick marks
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# Create a list for the start years of each decade
decades <- c(seq(from = 1913, to = 2003, by = 10), 2013)
# Make an empty list that you're going to put all your
# ggplot maps into
map.list <- list()
# Loop through each of the starting years in "decades"
for(i in seq_along(decades)){
  
  # Define the start and end year of the decade
  start.year <- decades[i]
  end.year <- decades[i] + 9
  # Add a short if statement so that you don't go into the future!
  if(end.year > 2017){
    end.year <- 2017
  }
  
  # Use filter to select just the records for that decade
  one.decade <- filter(unknowns, Year >= start.year & Year <= end.year)
  
  # Add the points to the base map
  # Create a different map for each decade with a different name
  # placed into map.list
  map.list[[i]] <- 
    base.map +
    geom_point(data = one.decade, aes(x = Longitude, y = Latitude),
               color = "red", size = 0.5) +
    # Add title with years covered
    labs(title = paste(start.year, "-", end.year))
} # end loop
# Plot all the plots in map.list
do.call(grid.arrange, map.list)


##################################################################################
#Beaker maps 

beakers <- cleaneddata %>% 
  filter(Name.Current.Sci %in% c("Hyperoodon ampullatus", "Mesoplodon densirostris", "Mesoplodon europaeus", "Mesoplodon mirus")) 


uk <- map_data("world", regions = c('UK', 'Ireland'))
# Create base map
base.map <- 
  ggplot() + 
  # Add country polygons
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") + 
  # Define coordinates to plot between
  coord_map(xlim = c(-11, 3), ylim = c(49, 60.9)) +
  # Remove grey background
  theme_bw() +
  # Remove x and y axes labels and tick marks
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# Create a list for the start years of each decade
decades <- c(seq(from = 1913, to = 2003, by = 10), 2013)
# Make an empty list that you're going to put all your
# ggplot maps into
map.list <- list()
# Loop through each of the starting years in "decades"
for(i in seq_along(decades)){
  
  # Define the start and end year of the decade
  start.year <- decades[i]
  end.year <- decades[i] + 9
  # Add a short if statement so that you don't go into the future!
  if(end.year > 2017){
    end.year <- 2017
  }
  
  # Use filter to select just the records for that decade
  one.decade <- filter(beakers, Year >= start.year & Year <= end.year)
  
  # Add the points to the base map
  # Create a different map for each decade with a different name
  # placed into map.list
  map.list[[i]] <- 
    base.map +
    geom_point(data = one.decade, aes(x = Longitude, y = Latitude),
               color = "red", size = 0.5) +
    # Add title with years covered
    labs(title = paste(start.year, "-", end.year))
} # end loop
# Plot all the plots in map.list
do.call(grid.arrange, map.list)
