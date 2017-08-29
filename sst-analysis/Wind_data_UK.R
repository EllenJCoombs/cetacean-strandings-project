#Mean windspeed data from the Met office 
#Met Office - average monthly windspeeds 

library(rgdal)
library(dplyr)
wind1969_1980 <- read.csv("MeanWindSpeed_1969-1980.csv")

# Variables for holding the coordinate system types (see:
# http://www.epsg.org/ for details)
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

# Remove those points with missing Eastings or
# Northings
wind1969_1980 <- subset(wind1969_1980, Easting != "" | Northing != "")
# Create a unique ID for each GP
wind1969_1980$wind1969_1980_ID <- 1:nrow(wind1969_1980)
# Create coordinates variable
coords <- cbind(Easting = as.numeric(as.character(wind1969_1980$Easting)),
                Northing = as.numeric(as.character(wind1969_1980$Northing)))

#All data 
wind1969_1980_all <- wind1969_1980 %>%
  select(Jan.69:Feb.80)

# Create the SpatialPointsDataFrame
wind1969_1980_SP <- SpatialPointsDataFrame(coords, data = data.frame(wind1969_1980_all,
                                                          wind1969_1980$wind1969_1980_ID), proj4string = CRS("+init=epsg:27700"))
#wind_SP is now a spatial data frame. We can do a quick plot(wind_SP) to see what this looks like.
# Show the results
plot(wind1969_1980_SP)

# Convert from Eastings and Northings to Latitude and Longitude
wind_SP_LL <- spTransform(wind1969_1980_SP, CRS(latlong))
# we also need to rename the columns
colnames(wind_SP_LL@coords)[colnames(wind_SP_LL@coords) == "Easting"] <- "Longitude"
colnames(wind_SP_LL@coords)[colnames(wind_SP_LL@coords) == "Northing"] <- "Latitude"

head(wind_SP_LL@coords)
plot(wind_SP_LL)
