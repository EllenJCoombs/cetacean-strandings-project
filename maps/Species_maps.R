
#Mapping individual species 

## Some code to make maps for each decade and plot in the same
# plotting window
# Load libraries 
install.packages("maps")
install.packages("mapdata")
library(maps)
library(mapdata)
#Install gridExtra if using grid arrange - if not, use facet_wrap
install.packages("gridExtra")
library(gridExtra)

#Had to install this to get it running - no idea why - was running fine before without 
install.packages("mapproj")
library(mapproj)


nhmcsip <- read.csv("cleaned.data.300517.csv")
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
  one.decade <- filter(nhmcsip, Year >= start.year & Year <= end.year) 

  
  
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



#Mapping all species 
gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  #Add in counties if needed 
  #geom_polygon(data = gadm, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  #geom_polygon(data = gadmireland, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) 


#Adds long and lat points 
points <- data.frame(
  long = nhmcsip$Longitude,
  lat = nhmcsip$Latitude,
  Name.Current.Sci = nhmcsip$Name.Current.Sci
)  

#Plotting lats and longs
gg1 +
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9)) + 
  facet_wrap(~Name.Current.Sci)


######
#One species at a time 
#Plotting one species e.g. Orcinus orca 
pointsorca <- nhmcsip %>% 
  select(Latitude, Longitude, Name.Current.Sci) %>%
  filter(Name.Current.Sci == "orcinus orca")

#Plotting lats and longs
gg1 +
  geom_point(data = pointsorca, aes(x = Longitude, y = Latitude), color = "red", size = 0.5) +
  geom_point(data = pointsorca, aes(x = Longitude, y = Latitude), color = "red", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))
 
######
#Common dolphin 
pointsddelphis <- nhmcsip %>% 
  select(Latitude, Longitude, Name.Current.Sci) %>%
  filter(Name.Current.Sci == "delphinus delphis")

#Plotting lats and longs 
gg1 +
  geom_point(data = pointsddelphis, aes(x = Longitude, y = Latitude), color = "red", size = 0.5) +
  geom_point(data = pointsddelphis, aes(x = Longitude, y = Latitude), color = "red", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))

#######
#Multiple species maps 
pointsmultiple <- nhmcsip %>% 
  select(Latitude, Longitude, Name.Current.Sci) %>%
    filter(Name.Current.Sci %in% c("delphinus delphis", "phocoena phocoena", "kogia sima", "orcinus orca")) %>%
  rename(Species = Name.Current.Sci)

gg1 +  
  geom_point(data = pointsmultiple, aes(x = Longitude, y = Latitude, color = Species), size = 0.5) + 
    geom_point(data = pointsmultiple, aes(x = Longitude, y = Latitude, color = Species), size = 0.5) + 
    coord_map(xlim=c(-11,3), ylim=c(49,60.9)) 
  



