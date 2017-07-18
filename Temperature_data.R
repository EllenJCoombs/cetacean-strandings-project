#Loading climate data into R 

#Required packages 
install.packages("chron")
library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)

#required download 
install.packages("ncdf4", repos = NULL, type="source")
ncname <- "cru10min30_tmp" 

#Reading in .nc data 
ncin1 <- nc_open("UK_left.switch.EN4.2.nc")
ncin2 <- nc_open("UK_right.EN4.2.nc")

print(ncin1)
print(ncin2)

# get longitude and latitude
long1 <- ncvar_get(ncin1,"lon")
long2 <- ncvar_get(ncin2, "lon")
nlon1 <- dim(long1)
nlon2 <- dim(long2)
head(long1)
head(long2)



