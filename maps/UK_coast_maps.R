

#Plotting UK coastal bathymetry to see if this may be biasing cetacean strandings 


install.packages("marmap")
library(marmap)
ukbathy <- getNOAA.bathy(lon1 = -13, lon2 = 4,
                        lat1 = 46, lat2 = 63, resolution = 10)   


summary.bathy(ukbathy)
plot(ukbathy)

#adding scale 
plot(ukbathy, image = TRUE)
scaleBathy(ukbathy, deg = 2, x = "bottomleft", inset = 5)

#Create a heat map 
blues <- colorRampPalette(c("red","purple","blue",
                            "cadetblue1","white"))
plot(ukbathy, image = TRUE, bpal = blues(100))


#Changing the colours to make it a little less crowded 
plot(ukbathy, image = TRUE, bpal = blues(100),
     deep = c(-9000, -3000, 0),
     shallow = c(-10, -5, 0),
     step = c(100, 100, 0),
     lwd = c(0.8, 0.8, 1), lty = c(1, 1, 1),
     col = c("lightgrey", "darkgrey", "black"),
     drawlabel = c(FALSE, FALSE, FALSE))


# Creating a custom palette of blues
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
# Plotting the bathymetry with different colors for land and sea
plot(ukbathy, image = TRUE, land = TRUE, lwd = 0.1,
     bpal = list(c(0, max(ukbathy), "grey"),
                 c(min(ukbathy),0,blues)))
# Making the coastline more visible
plot(ukbathy, deep = 0, shallow = 0, step = 0,
     lwd = 0.4, add = TRUE)


#####################################################################
#Ocean wind speeds - could bias where whales strand 
install.packages("utils")
install.packages("colorRamps")
install.packages("RNetCDF")
install.packages("sp")
require(utils)
require(colorRamps)
require(RNetCDF)





