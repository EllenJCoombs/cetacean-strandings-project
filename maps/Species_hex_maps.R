# Plot strandings as hex grids rather than points
# Because it looks cool :)

library(ggplot2)
library(viridis)
library(dplyr)
library(maps)
library(hexbin)
library(ggmap)

#This code plots one map with all strandings on it 
# Extract UK map
#Added Ireland 
uk <- map_data("world", regions = c('UK', 'Ireland', 'Guernsey', 'Jersey', 'Isle of Man'))

# Create base map
gg1 <- 
  ggplot() + 
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") + 
  coord_fixed(1.3) 

# Read in the strandings data
#This dataset has all unknowns removed and all rare species removed 
ds <- read.csv("UK_and_Irish_sp.csv")

# Remove NAs from coordinates
# And restrict to things in UK waters
#Filter species here too 
ds <- ds %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Latitude < 62 & Latitude > 45) %>%
  filter(Longitude < 3 & Latitude > -11) 

# Basic plot using viridis colour scheme
# Note that you can change bins and transparency 
gg1+
  geom_hex(data = ds, aes(y = Latitude, x= Longitude), bins = 25, alpha = 0.5) +
  scale_fill_gradientn(colours = viridis(4))

# More complex plot, with axes removed, smaller bins, defined colours, and simpler legend
#The lat longs and species S.W.No. were put in for extra detail so that I can remove errorneous values 

gg1+
  geom_hex(data = ds, aes(y = Latitude, x= Longitude), bins = 200, alpha = 0.5) +
  scale_fill_gradientn(colours = c("blue", "red")) +
  scale_x_discrete(name ="long", 
                   limits=c("-11" : "3")) +
  scale_y_discrete(name ="lat", 
                 limits=c("-48" : "65")) +
  geom_text(data=ds, aes(y = Latitude, x = Longitude, label = S.W.No.), size=0.3)
  
  
  theme(axis.line  = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(title = NULL, ticks = FALSE))


# And finally (I am in love with this feature)
# if you want to know the values for each of the hexagons...

gg2 <- gg1+
  geom_hex(data = ds, aes(y = Latitude, x= Longitude), bins = 100, alpha = 0.5) +
  scale_fill_gradientn(colours = viridis(4))

gg2

#This gives you information on the plots 
pg <- ggplot_build(gg2)

# Look at this object
pg

# Hex values are in pg$data[[2]]$count
pg$data[[2]]$count

#Looking at longs 
pg$data[[2]]$x
#Looking at lats 
pg$data[[2]]$y

###############################################################################################
#Map as above - and then add individual species 

# Remove NAs from coordinates
# And restrict to things in UK waters
#Filter species here too - filtering out by latitude 
S_coeruleoalba <- ds %>%
  filter(Name.Current.Sci == "Stenella coeruleoalba")

#Soecies filtered out by latitude 
S_coeruleoalba <- S_coeruleoalba %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Latitude < 65 & Latitude > 45) %>%
  filter(Longitude < 3 & Latitude > -11) 

# Basic plot using viridis colour scheme
# Note that you can change bins and transparency
gg1+
  geom_hex(data = S_coeruleoalba, aes(y = Latitude, x= Longitude), bins = 25, alpha = 0.5) +
  scale_fill_gradientn(colours = viridis(4))

# More complex plot, with axes removed, smaller bins, defined colours, and simpler legend
gg1+
  geom_hex(data = S_coeruleoalba, aes(y = Latitude, x= Longitude), bins = 100, alpha = 0.5) +
  scale_fill_gradientn(colours = c("blue", "red")) +
  theme(axis.line  = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  guides(fill = guide_colorbar(title = NULL, ticks = FALSE))


# And finally (I am in love with this feature)
# if you want to know the values for each of the hexagons...

gg2 <- gg1+
  geom_hex(data = S_coeruleoalba, aes(y = Latitude, x= Longitude), bins = 100, alpha = 0.5) +
  scale_fill_gradientn(colours = viridis(4))

pg <- ggplot_build(gg2)

# Look at this object
pg

# Hex values are in pg$data[[2]]$count
pg$data[[2]]$count

################################################################################################
#Decadal hex plots 
library(maps)
library(mapdata)
#Install gridExtra if using grid arrange - if not, use facet_wrap
library(gridExtra)
library(mapproj)
library(lubridate)


ds <- read.csv("UK_and_Irish_sp.csv")

#Set limits 
#This removes 53 NAs (lat long) as well 
ds <- ds %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Latitude < 62 & Latitude > 45) %>%
  filter(Longitude < 3 & Latitude > -11) 

# Extract map data
uk <- map_data("world", regions = c('UK', 'Ireland', 'Guernsey', 'Jersey', 'Isle of Man'))
# Create base map
base.map <- 
  ggplot() + 
  # Add country polygons
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), 
               fill = "lemonchiffon3", color = "grey49", size = 0.1, alpha = 0.3) + 
  coord_fixed(1.3)
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
  if(end.year > 2015){
    end.year <- 2015
  }
  
  
  # Use filter to select just the records for that decade
  one.decade <- filter(ds, Year >= start.year & Year <= end.year)
  
  
  # Add the points to the base map
  # Create a different map for each decade with a different name
  # placed into map.list
  #A few additions for hex plots 
  map.list[[i]] <- 
    base.map + 
    geom_hex(data = one.decade, aes(y = Latitude, x= Longitude), bins = 50, alpha = 0.5) +
    coord_equal() + 
    stat_binhex() +
    scale_fill_gradientn(colours = viridis(4), limits=c(1,200)) +
    theme(axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background=element_rect(fill = "white"), #removes white background 
          panel.background=element_rect(fill= "white"),
          panel.border = element_rect(colour = "white", fill = NA, size = 0.25),
          axis.title = element_blank()) + #Removes axis 
    guides(fill = guide_colorbar(title = "Stranding intensity", ticks = TRUE)) +
    # Add title with years covered
    labs(title = paste(start.year, "-", end.year)) +
    theme(plot.title = element_text(hjust = 0.5))
} # end loop
# Plot all the plots in map.list
do.call(grid.arrange, map.list)



