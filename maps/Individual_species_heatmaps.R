

library(gridExtra)
library(maps)
library(dplyr)
library(ggplot2)


# Read in the strandings data
UK_and_Irish_sp <- read.csv("UK_and_Irish.csv")
#Strip out specific species 
P_phocoena <- UK_and_Irish_sp %>%
  filter(Name.Current.Sci == "Phocoena phocoena")

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
  one.decade <- filter(P_phocoena, Year >= start.year & Year <= end.year)
  
  # Add the points to the base map
  # Create a different map for each decade with a different name
  # placed into map.list
  map.list[[i]] <- 
    ggplot(data = one.decade, aes(x = Longitude, y = Latitude)) + 
    stat_density2d(aes(fill = ..level..), alpha = 1, geom ="polygon") +
    geom_polygon(data = uk, aes(x = long, y = lat, group = group), 
                 fill = "white", color = "black") +
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", space = "Lab", 
                         na.value = "grey50", guide = "colourbar") +
    coord_map(xlim = c(-11, 3), ylim = c(49, 60.9)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(title = paste(start.year, "-", end.year))
} # end loop
# Plot all the plots in map.list
do.call(grid.arrange, map.list)
