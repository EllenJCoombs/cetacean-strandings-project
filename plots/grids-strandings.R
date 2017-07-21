# Plot strandings as hex grids rather than points
# Because it looks cool :)

library(ggplot2)
library(viridis)
library(dplyr)

# Extract UK map
#Added Ireland 
uk <- map_data("world", regions = c('UK', 'Ireland'))

# Create base map
gg1 <- 
  ggplot() + 
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") + 
  coord_fixed(1.3) 

# Read in the strandings data
ds <- read.csv("cleandatesnames.csv")

# Remove NAs from coordinates
# And restrict to things in UK waters
#Filter species here too 
ds <- ds %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Latitude < 65 & Latitude > 45) %>%
  filter(Longitude < 3 & Latitude > -8) 

# Basic plot using viridis colour scheme
# Note that you can change bins and transparency
gg1+
geom_hex(data = ds, aes(y = Latitude, x= Longitude), bins = 25, alpha = 0.5) +
scale_fill_gradientn(colours = viridis(4))

# More complex plot, with axes removed, smaller bins, defined colours, and simpler legend
gg1+
  geom_hex(data = ds, aes(y = Latitude, x= Longitude), bins = 100, alpha = 0.5) +
  scale_fill_gradientn(colours = c("blue", "red")) +
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

pg <- ggplot_build(gg2)

# Look at this object
pg

# Hex values are in pg$data[[2]]$count
pg$data[[2]]$count
