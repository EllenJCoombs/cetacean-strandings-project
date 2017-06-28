#This code runs on from the 240517 Dates and column names, i.e rerun that code to
#clean the dataset before doing the below 

#Load these if not already loaded 
library(dplyr)
library(tidyverse)
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("ggmap")
library(ggmap)
install.packages("sp")
library(sp)
library(ggfortify)

install.packages("maps")
install.packages("mapdata")
library(maps)
library(mapdata)


nhmcsip <- read.csv("cleandates.csv")


#Map that includes all of the islands as well 
map('worldHires',
    c('UK', 'Ireland', 'Isle of Man','Isle of Wight', 'Wales:Anglesey'))

#change the scale 
map('worldHires',
    c('UK', 'Ireland', 'Isle of Man','Isle of Wight'),
    xlim=c(-11,3), ylim=c(49,60.9))

points(nhmcsip$Longitude, nhm$Latitude, col=2,pch=18)  


#UK map - got southern Ireland! 
uk <- map_data("world", regions = c('Ireland','UK')) 
uk <- map_data("uk") # we already did this, but we can do it again
ggplot() + geom_polygon(data = uk, aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3)


#This region name is not recognised 
counties <- map_data("county", region = 'UK')


#changing map outline and colours 
ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = NA, color = "red") + 
  coord_fixed(1.3)


#Shading the inside of the map 
gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "blue") + 
  coord_fixed(1.3)


points <- data.frame(
  long = c(-1.605549405, -9.8976),
  lat = c(60.44977781, 52.1049),
  names = c("one", "two"),
  stringsAsFactors = FALSE
)  

gg1 +
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 2) +
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 2)


#Working code 
#############################################################################

#Map with Ireland as well (yay)
uk <- map_data("world", regions = c('UK', 'Ireland'))
uk <- map_data("uk")
ggplot() + geom_polygon(data = uk, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)


#changing map outline and colours - just for fun 
ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = NA, color = "red") +


  geom_polygon(data = ukcounty, aes(x = long, y = lat, group = group), fill = NA, color = "black")
  coord_fixed(1.3)


#Plotting counties with GADM - for United Kingdom 
gadm <- readRDS("/Users/ellencoombs/Desktop/Projects/Strandings/data/GBR_adm2 (1).rds")
plot(gadm)


#Plotting counties - southern Ireland
gadmireland <- readRDS("/Users/ellencoombs/Desktop/Projects/Strandings/data/IRL_adm1.rds")
plot(gadmireland)



#This chunk of code plots the UK, southern Ireland, shades the map and also adds county lines for 
#The UK and Ireland 

gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  geom_polygon(data = gadm, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  geom_polygon(data = gadmireland, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) 
  


#Adds long and lat points 
points <- data.frame(
  long = nhmcsip$Longitude,
  lat = nhmcsip$Latitude)  

#Plotting lats and longs - last line is a zoom in to Scotland - remove if wanting to see the whole dataset
gg1 +
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9)) +
  coord_fixed(xlim = c(-7.409381252, -2.116296036),  ylim = c(57.15599797, 59.1839841), ratio = 1.200)

#197 points not plotted as they are NAs - need to fix these 

gg1

#Playing around with the county data
#Changing the border and fill color 
plot(gadm, col = 'NA', border = 'black')
#Selecting Northumberland (object ID = 71)
wholemap <- rep("NA", 192)
wholemap[71] <- "red"
plot(gadm, col = wholemap, border = 'black')

#To see what the differet object IDs are 
View(gadm)
View(gadmireland)


filter(nhmcsip, Longitude > 150)
#Need to change this - should be 57.7522 (Highlands)
#Name.Current.Sci          Name.Common Latitude Longitude   County Year       Date
# lagenorhynchus albirostris white-beaked dolphin 44.50139  157.7522 Highland 2015 2015-08-06


#Need to change NAs and also 0,0 cordinates 

#London - an example of how to zoom in on sections 
gg1 +
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9)) +
  coord_fixed(xlim = c(-0.1278),  ylim = c(51.5074), ratio = 1.200) 
 

#Time bins 

#Need to split data into specific bins using select 
nhmcsip
View(nhmcsip)
names(nhmcsip)
#Removing the 'X' variable again - not sure where this comes from???
nhmcsip <- select(nhmcsip, Name.Current.Sci, Name.Common, Latitude, Longitude, County, Year, Date)




#1913 - 1923 ############################################################################
#Selecting 1913 - 1923 

nhmcsip1913 <- filter(nhmcsip, Year >= 1913 & Year <= 1923)
#Did use: nhmcsip1913 <- slice(nhmcsip, 1:390)

gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  coord_fixed(1.3) 


points1913 <- data.frame(
  long = nhmcsip1913$Longitude,
  lat = nhmcsip1913$Latitude)  

gg1 +
  geom_point(data = points1913, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points1913, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))

#########################################################################################
#1924-1934 

nhmcsip1924 <- filter(nhmcsip, Year >= 1924 & Year <= 1934)
#I did use: nhmcsip1924 <- slice(nhmcsip, 391:1167)
View(nhmcsip1924)

gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  coord_fixed(1.3) 


points1924 <- data.frame(
  long = nhmcsip1924$Longitude,
  lat = nhmcsip1924$Latitude)  

gg1 +
  geom_point(data = points1924, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points1924, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))


#####################################################################################
#1935-1945

nhmcsip1935 <- filter(nhmcsip, Year >= 1935 & Year <= 1945)
#Did use this: nhmcsip1935 <- slice(nhmcsip, 1168:1649)
View(nhmcsip1935)

gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  coord_fixed(1.3) 


points1935 <- data.frame(
  long = nhmcsip1935$Longitude,
  lat = nhmcsip1935$Latitude)  


gg1 +
  geom_point(data = points1935, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points1935, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))


########################################################################################
#1946 - 1956
nhmcsip1946 <- filter(nhmcsip, Year >= 1946 & Year <= 1956)

#Did use this: nhmcsip1946 <- slice(nhmcsip, 1650:2282)
View(nhmcsip1946)


gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  coord_fixed(1.3) 


points1946 <- data.frame(
  long = nhmcsip1946$Longitude,
  lat = nhmcsip1946$Latitude)  

gg1 +
  geom_point(data = points1946, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points1946, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))


######################################################################################
#1957 - 1967 

nhmcsip1957 <- filter(nhmcsip, Year >= 1957 & Year <= 1967)
#Did use this: nhmcsip1957 <- slice(nhmcsip, 2283:2754)
View(nhmcsip1957)


gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  coord_fixed(1.3) 


points1957 <- data.frame(
  long = nhmcsip1957$Longitude,
  lat = nhmcsip1957$Latitude)  

gg1 +
  geom_point(data = points1957, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points1957, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))



##########################################################################################
#1968 - 1978 

nhmcsip1968 <- filter(nhmcsip, Year >= 1968 & Year <= 1978)
#I did use this 
#nhmcsip1968 <- slice(nhmcsip, 2755:3204)

View(nhmcsip1968)


gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  coord_fixed(1.3) 


points1968 <- data.frame(
  long = nhmcsip1968$Longitude,
  lat = nhmcsip1968$Latitude) 

gg1 +
  geom_point(data = points1968, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points1968, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))



##########################################################################################
#1979 - 1989

#Slicing years 1979-1989
nhmcsip1979 <- filter(nhmcsip, Year >= 1979 & Year <= 1989)
#I did use: nhmcsip1979 <- slice(nhmcsip, 3205:4311)
View(nhmcsip1979)

#Plotting the UK map 
gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  coord_fixed(1.3) 


#Latitude and long points to the UK map
points1979 <- data.frame(
  long = nhmcsip1979$Longitude,
  lat = nhmcsip1979$Latitude)  

gg1 +
  geom_point(data = points1979, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points1979, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))

#########################################################################################
#1990 - 2000

nhmcsip1990 <- filter(nhmcsip, Year >= 1990 & Year <= 2000)
#I did use: nhmcsip1990 <- slice(nhmcsip, 4312:8112)
View(nhmcsip1990)


gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  coord_fixed(1.3) 


points1990 <- data.frame(
  long = nhmcsip1990$Longitude,
  lat = nhmcsip1990$Latitude)

gg1 +
  geom_point(data = points1990, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points1990, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))


#########################################################################################
#2001 - 2010

nhmcsip2001 <- filter(nhmcsip, Year >= 2001 & Year <= 2010)
#Did use: nhmcsip2001 <- slice(nhmcsip, 8113:14381)
View(nhmcsip2001)


gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  coord_fixed(1.3) 


points2001 <- data.frame(
  long = nhmcsip2001$Longitude,
  lat = nhmcsip2001$Latitude)  

gg1 +
  geom_point(data = points2001, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points2001, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))


##########################################################################################
#2011 - 2015 
nhmcsip2011 <- filter(nhmcsip, Year >= 2011 & Year <= 2015)
#I did use: nhmcsip2011 <- slice(nhmcsip, 14382:17402)
View(nhmcsip2011)


gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "violet", color = "black") + 
  coord_fixed(1.3) 


points2011 <- data.frame(
  long = nhmcsip2011$Longitude,
  lat = nhmcsip2011$Latitude) 

gg1 +
  geom_point(data = points2011, aes(x = long, y = lat), color = "black", size = 0.5) +
  geom_point(data = points2011, aes(x = long, y = lat), color = "black", size = 0.5) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))


######################################################################################



#counties added to the map 
install.packages("rgdal")
library(rgdal)
install.packages("spdplyr")
library(spdplyr)

ukcounty <- read.csv("infuse_cnty_lyr_2011.csv")


#Plotting counties with GADM
gadm <- readRDS("/Users/ellencoombs/Desktop/Projects/Strandings/data/GBR_adm2 (1).rds")
plot(gadm)
#Changing the border and fill color 
plot(gadm, col = 'NA', border = 'black')
#Selecting Northumberland (object ID = 71)
wholemap <- rep("NA", 192)
wholemap[71] <- "red"
plot(gadm, col = wholemap, border = 'black')




###########################################################################################
#Use geom density for heat maps???
#Use animate for a decade by decade animation 

############################################################################################
## Some code to make maps for each decade and plot in the same
# plotting window
# Load libraries 
library(ggplot2)
library(tidyverse)
library(mapdata)
#Had to install this to get it running - no idea why - was running fine before without 
install.packages("mapproj")
library(mapproj)
# Read in the strandings data
nhmcsip <- read_csv("cleaned.data.300517.csv")
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

map.list

#Had to load gridExtra to get the code to run (couldn't find 'grid.arrange'was the error message)
library(gridExtra)
install.packages("gridExtra")

#################################################################################
#Want to try and animate the code....
#installing gganimate 
devtools::install_github("dgrtwo/gganimate")
library(gganimate)
install.packages("animation")
library(animation)
install.ffmpeg()

library(animate)




########################################################################################
#Heat map of all strandings 
install.packages("viridis")
library(viridis)
library(ggplot2)
library(dplyr)
install.packages("RColorBrewer")
library(RColorBrewer)

#Playing with colour palettes 
mypalette<-brewer.pal(8,"Dark2")

counts <- nhmcsip %>% count(Latitude, Longitude) %>% ungroup() %>% arrange(desc(n))
rename(counts, count = n)


gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x=long, y = lat, group = group), fill = "white", color = "black") + 
  #If you want to add the county lines then uncomment the next two lines 
  geom_polygon(data = gadm, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  geom_polygon(data = gadmireland, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) 

#Adds long and lat points 
points <- data.frame(
  long = counts$Longitude,
  lat = counts$Latitude,
  count = counts$n)

png("myplot.png")
gg1 +
  geom_point(data = points, aes(x = long, y = lat, colour = count), size = 0.5, alpha = 0.3) +
  geom_tile(colour = "blue") +
  #scale_fill_gradient(low = "white", high = "red",
  #                    breaks = seq(0, 200, by = 30)) + 
  scale_colour_gradientn(colours = terrain.colors(10),
                         breaks = seq(0, 300, by = 10)) +
  coord_map(xlim=c(-11,3), ylim=c(49,60.9)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank() 
  ) 

#cleaning the plot window - don;t know why the plots are opening like this 
 dev.off() 
?scale_fill_gradient

hist(rnorm(10))


################
#Heat maps 
gg1+
geom_point(data = points, aes(x = long, y = lat, colour = count), size = 0.5, alpha = 0.5) +
  stat_density2d(aes(colour = count), alpha=0.5, geom = "polygon") +
  geom_point(colour = "red") +
  geom_path(data = points,aes(x = long, y = lat,group=group), colour ="grey50")+
  scale_fill_gradientn(colours = rev(brewer.pal(7,"Spectral")), guide = guide_legend(nrow=1))+
  coord_map(xlim = c(-11,3), ylim = c(49,60.9))
  

install.packages("rgdal")
library(rgdal)         # for readOGR(...)

sample <- data.frame(Longitude=c(-1+rnorm(50,0,.5),-2+rnorm(50,0,0.5),-4.5+rnorm(50,0,.5)),
                     Latitude =c(52+rnorm(50,0,.5),54+rnorm(50,0,0.5),56+rnorm(50,0,.5)))
UKmap <- readRDS("/Users/ellencoombs/Desktop/Projects/Strandings/data/GBR_adm2 (1).rds")
IRmap <- readRDS("/Users/ellencoombs/Desktop/Projects/Strandings/data/IRL_adm1.rds")
map.df <- fortify(UKmap)
ir.df <- fortify(IRmap)

ggplot(points, aes(x = long, y = lat)) + 
  stat_density2d(aes(fill = ..level..), alpha = 1, geom ="polygon")+
  geom_point(colour = "black", size = 0.5)+
  geom_path(data = map.df, aes(x = long, y = lat, group = group), colour ="grey50")+
  geom_path(data = ir.df, aes(x = long, y = lat, group = group), colour ="grey50")+
  scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", space = "Lab", 
  na.value = "grey50", guide = "colourbar") +
  xlim(-10,+2.5) +
  coord_map(xlim = c(-11,3), ylim = c(49,60.9))

#Different colour palettes and experimentation (can't see points/density easily)
ggplot(points, aes(x = long, y = lat)) + 
  stat_density2d(aes(fill = ..level..), alpha=0.5, geom ="polygon")+
  geom_point(colour = "black", size = 0.5)+
  geom_path(data = map.df, aes(x = long, y = lat,group = group), colour ="grey50")+
  scale_fill_gradientn(colours = rev(brewer.pal(7,"Spectral")), breaks = seq(0,200, by = 5) +
                                       xlim(-10, +2.5) +
                                       coord_map(xlim = c(-11,3), ylim = c(49,60.9))


