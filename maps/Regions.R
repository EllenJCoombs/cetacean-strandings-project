
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
install.packages("tools")
library(tools)


#Use the above map info for the below map 

#Creat polygon specifics with lat longs  
library(ggmap)
Longitude1<-c(-10.458984, -2.460938, 1.669922, -10.458984, -10.458984)
Latitude1<-c(58.5, 58.5, 61.891755, 61.891755, 58.5)
mydata<-as.data.frame(cbind(Longitude1,Latitude1))
mydata
UK<-get_map("United Kingdom",zoom=5)
gg2 +
  geom_polygon(data=mydata,aes(x=Longitude1,y=Latitude1),alpha=0.3,colour="red",fill="red")+
  geom_path(data=mydata,aes(x=Longitude1,y=Latitude1),colour="white",alpha=0.7,size=0.5)


#Put county data in 

#Provide the function fortify.shape(), which puts the shapefile data in the object class data.frame, 
#so that it can be used by ggplot2
fortify.shape <- function(x){
x@data$id <- rownames(x@data)
x.f <- fortify(x, region = "id")
x.join <- inner_join(x.f, x@data, by = "id")
}

#Provide the function subset.shape(), which we will use to extract portions of the data (from the fortified data.frame object) for a smaller domain,
#since shapefiles often contain global data.
subset.shape <- function(x, domain){
  x.subset <- filter(x, long > domain[1] & 
                       long < domain[2] & 
                       lat > domain[3] & 
                       lat < domain[4])
  x.subset
}


path.ne.coast <- ("/Users/ellencoombs/Desktop/ne_10m_coastline")
fnam.ne.coast <- "ne_10m_coastline.shp"
dat.coast <- readOGR(dsn = path.ne.coast, 
                     layer = file_path_sans_ext(fnam.ne.coast))

#Fortify the shapefile data using `fortify.shape()`:
#Specify the area I want 
dat.coast <- fortify.shape(dat.coast)
domain <- c(-11, 3, 49, 61)

#Extract the coastline data for the desired domain using `subset.shape()`:
dat.coast.uk <- subset.shape(dat.coast, domain)


#Specify the spatial extent for our map (i.e., our study area; notice that its
#dimensions are different from the domain for which we extracted the
#coastline):
xlims <- c(-11, 3)
ylims <- c(49, 61)

#Generate plot 
p0 <- ggplot() + 
  geom_path(data = dat.coast.uk, aes(x = long, y = lat, group = group), 
            color = "black", size = 0.25) + 
  coord_map(projection = "mercator") + 
  scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
  scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
  labs(list(title = "", x = "Longitude", y = "Latitude"))

p0


p1 <- p0 + 
  geom_point(data = ds, 
             aes(x = Longitude, y = Latitude), 
             shape = 16, size = 0.5, color = "red") 
p1


path.eez.world.v9 <- ("/Users/ellencoombs/Desktop/World_EEZ_v9_20161021_LR")
fnam.eez.world.v9 <- "eez_lr.shp"

eez.world.v9 <- readOGR(dsn = path.eez.world.v9, 
                        layer = file_path_sans_ext(fnam.eez.world.v9))

# Extract the EEZ for the UK
dat.eez.uk <- eez.world.v9[eez.world.v9@data$Territory1 == "United Kingdom", ]
dat.eez.uk <- fortify.shape(dat.eez.uk)

#Plotting the polygon on the UK map 
str(dat.eez.uk)


#The below values came from: dat.eez.uk$piece and dat.eez.uk$PolygonID
p.eez.vliz <- p1 + 
  geom_path(data = filter(dat.eez.uk, piece == 1 & PolygonID == 228), 
            aes(x = long, y = lat, group = group), 
            colour = "blue", size = 0.75)
p.eez.vliz

#Extracting data into a dataframe for use 
dat.eez.atlantic <- droplevels(filter(dat.eez.uk, piece == 1 & PolygonID == 228))

#Exporting a polygon
#Export this polygon (long, lat vectors) as a tab-separated text file so that we don’t have to read
#and extract it every time we want to use it
dat.eez.exp <- dat.eez.atlantic %>% dplyr::select(long, lat) 
path.eez.exp <- ("/Users/ellencoombs/desktop/World_EEZ_v9_20161021_LR")
fnam.eez.exp <- "eez_uk_pac_lr_v9.txt"
write_tsv(dat.eez.exp, path = paste(path.eez.exp, fnam.eez.exp, sep = "/"))

#Use function point.in.polygon() in the sp package to determine if a simulated whale observation is inside the EEZ
#(0 is outside, 1 is inside)and extract those observations in a separate dataframe:

inside.eez <- point.in.polygon(ds$Longitude, ds$Latitude, 
                              dat.eez.atlantic$long, dat.eez.atlantic$lat) 


#Add a column to sim.obs with this information:
ds$eez <- inside.eez

# Extract the observations in sim.obs that occur inside the EEZ:
sim.obs.eez <- ds %>% 
  dplyr::filter(eez == 1)  %>% 
  dplyr::select(-eez) 


p2 <- p0 + 
  geom_point(data = ds, aes(x = Longitude, y = Latitude), 
             colour = "gray75", shape = 16, size = 1) + 
  geom_point(data = sim.obs.eez, 
             aes(x = Longitude, y = Latitude), 
             shape = 16, size = 1) + 
  scale_colour_brewer(palette = "YlOrBr", 
                      guide_legend(title = "Number \nof whales")) + 
  geom_path(data = dat.eez.atlantic, aes(x = long, y = lat, group = group), 
            colour = "blue", size = 0.75)
p2

str(sim.obs.eez)

#Heat maps - (putting it all in one place)

p3 <- p0 + 
  stat_density_2d(data = sim.obs.eez, geom = "polygon", 
                  aes(x = Longitude, y = Latitude, fill = ..level..)) + 
  scale_fill_distiller(palette = "Spectral", 
                       guide_legend(title = "Probability \ndensity")) + 
  geom_point(data = ds, aes(x = Latitude, y = Longitude), 
             colour = "gray75", shape = 16, size = 1) + 
  geom_point(data = sim.obs.eez, aes(x = Longitude, y = Latitude), 
             colour = "black", shape = 16, size = 1) + 
  geom_path(data = dat.eez.atlantic, aes(x = Longitude, y = Latitude, group = group), 
            colour = "blue", size = 0.75) + 
  labs(list(title = "Cetacean presence within the EEZ", 
            x = "Longitude", y = "Latitude"))
p3




