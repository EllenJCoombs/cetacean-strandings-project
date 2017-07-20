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
ncin1 <- nc_open("UK_left.switch.EN.4.2.nc")
ncin2 <- nc_open("UK_right.EN.4.2.nc")

#Having a look at what's in the files 
attributes(ncin1)$names
attributes(ncin2)$names

#Variables 
attributes(ncin1$var)$names
attributes(ncin2$var)$names

#They have: 
#[1] "depth_bnds"                      "salinity"                       
#[3] "salinity_observation_weights"    "salinity_uncertainty"           
#[5] "temperature"                     "temperature_observation_weights"
#[7] "temperature_uncertainty"         "time_bnds"          


print(ncin1)
print(ncin2)


ncin1_data <- ncvar_get(ncin1, attributes(ncin1$var)$names[1])
print(ncin1_data)


#Global attributes 
# get global attributes
title <- ncatt_get(ncin1,0,"title")
institution <- ncatt_get(ncin1,0,"institution")
datasource <- ncatt_get(ncin1,0,"source")
references <- ncatt_get(ncin1,0,"references")
history <- ncatt_get(ncin1,0,"history")
Conventions <- ncatt_get(ncin1,0,"Conventions")



# get longitude and latitude
long1 <- ncvar_get(ncin1,"lon")
long2 <- ncvar_get(ncin2, "lon")
nlon1 <- dim(long1)
nlon2 <- dim(long2)
head(long1)
head(long2)

lat1 <- ncvar_get(ncin1,"lat")
lat2 <-ncvar_get(ncin2, "lat")
nlat1 <- dim(lat1)
nlat2 <- dim(lat2)
head(lat1)
head(lat2)

print(c(nlon1,nlat1))
print(c(nlon2,nlat2))

#Time variable from the dataset 
# get time
time1 <- ncvar_get(ncin1,"time")
time2 <- ncvar_get(ncin2, "time")
time1
time2

#Variables 
depth1 <- ncvar_get(ncin1,"depth")
depth2 <- ncvar_get(ncin2,"depth")
temp1 <- ncvar_get(ncin1,"temperature")
temp2 <- ncvar_get(ncin2,"temperature")
temp_uncertainty1 <- ncvar_get(ncin1,"temperature_uncertainty")
temp_uncertainty2 <- ncvar_get(ncin2, "temperature_uncertainty")

# replace netCDF fill values with NA's - no fill values found 
temp1[temp1==fillvalue$value] <- NA
temp2[temp2==fillvalue$value] <- NA

#How much of the data are land points? 
length(na.omit(as.vector(temp1[,,1])))
length(na.omit(as.vector(temp2[,,1])))

# get a single slice or layer (January)
m <- 1
tmp_slice <- temp1[,,m]

#Using the time slice above to look at January temps 
image(long1,lat1,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

#All temps 
# levelplot of the slice - doesn't look right 
grid <- expand.grid(lon=long1, lat=lat1)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tmp_slice ~ long1 * lat1, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))


# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(long1,lat1))
dim(lonlat)

# vector of `tmp` values
tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)

ncin1

tustr <- strsplit(tunits$value," ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))

save(ncin1, file = "UK_left.switch.EN4.2.RData")
save(ncin2, file = "UK_right.EN4.2.RData")

save(ncin1, file = "UK.left.switch.en4.2.csv")

#Create a dataframe 
# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat1 <- as.matrix(expand.grid(long1,lat1))
dim(lonlat1)

lonlat2 <- as.matrix(expand.grid(long2,lat2))
dim(lonlat2)

#Temperatures 
# vector of `tmp` values
# replace netCDF fill values with NA's
tmp_array[tmp_array==fillvalue$value] <- NA


tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)



