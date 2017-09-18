
#Splitting the UK up into 7 sections 
#Have tried Lat/long (max and min)
#Have tried various counties but some appear in both regions 

# Plot strandings as hex grids rather than points
# Because it looks cool :)

library(ggplot2)
library(viridis)
library(dplyr)
library(maps)
library(hexbin)
library(ggmap)

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
ds <- read.csv("cleandatesnames.csv")

# Remove NAs from coordinates
# And restrict to things in UK waters
#Filter species here too 
ds <- ds %>%
  filter(County %in% c("Sutherland, Scotland", "Highland, Scotland", "Shetland Isles, Scotland", 
                       "Shetland Islands, Scotland", "Fair Isle, Shetland Isles", "Shetland", 
                       "Orkney Islands, Scotland", "Orkney", "Caithness, Scotland"))

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

pg <- ggplot_build(gg2)

# Look at this object
pg

# Hex values are in pg$data[[2]]$count
pg$data[[2]]$count

#Looking at longs 
pg$data[[2]]$x
#Looking at lats 
pg$data[[2]]$y

#################################################################################################
library(sp)
install.packages("raster")
library(raster)
library(rgdal)
library(rgeos)
install.packages("rgeos")
install.packages("maptools")
library(maptools)


#Use the above map info for the below map 

#Creat polygon specifics with lat longs  
library(ggmap)
Longitude<-c(-5.00976, -3.021124, 0.659180, -5.00976)
Latitude<-c(58.65247, 58.61635, 61.34868, 58.65247)
mydata<-as.data.frame(cbind(Longitude,Latitude))
mydata
UK<-get_map("United Kingdom",zoom=5)
gg2 +
  geom_polygon(data=mydata,aes(x=Longitude,y=Latitude),alpha=0.3,colour="red",fill="red")+
  geom_path(data=mydata,aes(x=Longitude,y=Latitude),colour="white",alpha=0.7,size=0.5)

