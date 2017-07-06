

#Attempting to split UK map into 1km x 1km squares on a raster 
library(ggplot2)
library(raster)
library(sp)

#My usual map 
#Using cleann
gg1 <- ggplot() + 
  geom_polygon(data = uk, aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  #Add in counties if needed 
  geom_polygon(data = gadm, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  geom_polygon(data = gadmireland, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) 


#Adds long and lat points 
points <- data.frame(
  long = cleaneddata$Longitude,
  lat = cleaneddata$Latitude,
  Name.Current.Sci = cleaneddata$Name.Current.Sci
)  

#Plotting lats and longs
gg1 +
  geom_point(data = longlat, aes(x = points, y = lat), color = "red", size = 0.5) +
  geom_point(data = longlat, aes(x = points, y = lat), color = "red", size = 0.5) +
  #over(geom_polygon(data = UKmap_utm, aes(x=long, y=lat, group=group), fill=NA, color="black")) +
  geom_path(sq_grid, border = "orange", add = TRUE) + 
  coord_map(xlim=c(-11,3), ylim=c(49,60.9))
  



###########
library(dplyr)

#This just plots a part of Scotland....
study_area <- Britain %>% 
  disaggregate %>% 
  geometry
study_area <- sapply(study_area@polygons, slot, "area") %>% 
{which(. == max(.))} %>% 
  study_area[.]
plot(study_area, col = "grey50", bg = "light blue",xlim=c(-11,3), ylim=c(49,60.9))
text(11, 60.9, "Study Area:\nUnited Kingdom")


Britain <- c(UKmap, IRmap)

#Plots hexagons of 0.5
size <- 0.3
hex_points <- spsample(UKmap, type = "hexagonal", cellsize = size) 
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
plot(UKmap, col = "grey50", bg = "light blue", axes = TRUE) 
plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
plot(hex_grid, border = "orange", add = T)


#Function for making maps with square or hexagonal grids as required 
make_grid <- function(x, type, cell_width, cell_area, clip = FALSE) {
  if (!type %in% c("square", "hexagonal")) {
    stop("Type must be either 'square' or 'hexagonal'")
  }

if (missing(cell_width)) {
  if (missing(cell_area)) {
    stop("Must provide cell_width or cell_area")
  } else {
    if (type == "square") {
      cell_width <- sqrt(cell_area)
    } else if (type == "hexagonal") {
      cell_width <- sqrt(2 * cell_area / sqrt(3))
    }
  }
}
# buffered extent of study area to define cells over
ext <- as(extent(x) + cell_width, "SpatialPolygons")
projection(ext) <- projection(x)
# generate grid
if (type == "square") {
  g <- raster(ext, resolution = cell_width)
  g <- as(g, "SpatialPolygons")
} else if (type == "hexagonal") {
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_width, offset = c(0, 0))
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_width)
}

# clip to boundary of study area
if (clip) {
  g <- gIntersection(g, x, byid = TRUE)
} else {
  g <- g[x, ]
}
# clean up feature IDs
row.names(g) <- as.character(1:length(g))
return(g)
}


#Square maps 
install.packages("rgeos")
library(rgeos)

UKmap_utm <- CRS("+proj=utm +zone=44 +datum=WGS84 +units=km +no_defs") %>% 
  spTransform(UKmap, .)
IRmap_utm <- CRS("+proj=utm +zone=44 +datum=WGS84 +units=km +no_defs") %>% 
  spTransform(IRmap, .)

points_utm <- CRS("+proj=utm +zone=44 +datum=WGS84 +units=km +no_defs") %>% 
  spTransform(points, .)


#Square cells 
sq_grid <- make_grid(UKmap_utm, type = "square", cell_area = 625, clip = FALSE) 
plot(UKmap_utm, col = "grey50", bg = "light blue", axes = FALSE)
plot(sq_grid, border = "orange", add = TRUE) 
overlay(points)
box() 

geom_point(longlat) 


#################################
library(rgdal)
library(sp)
sp_poly <- CRS("+proj=longlat +zone=44 +datum=WGS84 +units=km +no_defs")
# set coordinate reference system with SpatialPolygons(..., proj4string=CRS(...))
# e.g. CRS("+proj=longlat +datum=WGS84")
sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
writeOGR(sp_poly_df, "points", layer="points", driver="ESRI Shapefile")



install.packages("config")
library(config)



GB <- getData(name = "GADM", country = "GB", level = 0) %>% 
  disaggregate %>% 
  geometry
# exclude gapalapos
GB <- sapply(GB@polygons, slot, "area") %>% 
{which(. == max(.))} %>% 
  GB[.]
# albers equal area for Great Britain 
GB <- spTransform(GB, CRS(
  paste("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60",
        "+x_0=0 +y_0=0 +ellps=aust_SA +units=km +no_defs")))
hex_gb <- make_grid(GB, type = "hexagonal", cell_area = 2500, clip = FALSE)


#families = name 
#family = species 
#bird_families = categories 

fill_missing <- expand.grid(id = row.names(hex_gb), 
                            family = points, stringsAsFactors = FALSE)
point_density <- overlay(hex_gb, longlat, returnList = TRUE) %>% 
  plyr::ldply(.fun = function(x) x, .id = "id") %>%
  mutate(id = as.character(id)) %>% 
  count(id, points) %>% 
  left_join(fill_missing, ., by = c("id", "family")) %>%
  # log transform
  mutate(n = ifelse(is.na(n), -1, log10(n))) %>% 
  spread(points, n, fill = -1) %>% 
  SpatialPolygonsDataFrame(hex_gb, .)

spplot(point_density, points,
       main = "Ecuador eBird Sightings by Family",
       col.regions = c("grey20", viridis(255)),
       colorkey = list(
         space = "bottom",
         at = c(-0.1, seq(0, log10(1200), length.out = 255)),
         labels = list(
           at = c(-0.1, log10(c(1, 5, 25, 75, 250, 1200))),
           labels = c(0, 1, 5, 25, 75, 250, 1200)
         )
       ),
       xlim = bbexpand(bbox(point_density)[1, ], 0.04), 
       ylim = bbexpand(bbox(point_density)[2, ], 0.04),
       par.strip.text = list(col = "white"),
       par.settings = list(
         strip.background = list(col = "grey40"))
)




       