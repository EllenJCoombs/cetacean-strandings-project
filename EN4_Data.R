#Loading climate data into R 

#Required packages 
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

ncin1

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

temp1

# replace netCDF fill values with NA's - no fill values found 
temp1[temp1==fillvalue$value] <- NA
temp2[temp2==fillvalue$value] <- NA

#How much of the data are land points? 
length(na.omit(as.vector(temp1[,,1])))
length(na.omit(as.vector(temp2[,,1])))

# get a single slice or layer (January)
#Changed to temp1[m]
m <- 1
tmp_slice <- temp1[,,m]
dim(tmp_slice)

#Using the time slice above to look at January temps 
image(long1,lat1,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

#All temps 
# levelplot of the slice - doesn't look right 
grid <- expand.grid(lon=long1, lat=lat1)
cutpts <- c(-20,-10,0,10,20,30)
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

tmp_df01 <- data.frame(cbind(lonlat1,tmp_vec))
names(tmp_df01) <- c("lon1","lat1",paste(temp1,as.character(m), sep="_"))
head(na.omit(tmp_df01), 10)




install.packages("ncdf4.helpers")
library(ncdf4.helpers)
install.packages("PCICt")
library(PCICt)

#This use of non-standard calendars messes up the calculations of as.Date, 
#so you must use a more specialized function to read in the time variable of the
#climate model output and map it to standard calendar dates. 
#You can do this with the nc.get.time.series function from the ncdf4.helpers package
#(you should also load the PCICt package to use this function):

tmp_time <- nc.get.time.series(ncin1, v = "sst",
                               time.dim.name = "time")
sst_time[c(1:3, length(sst_time) - 2:0)]

#Getting sst data 
sst <- ncvar_get(ncin, "sst")
dim(sst)
length(sst_time)
summary(sst_time)

#Selecting a specific time point and place 
#Answers are in celcius? CHECK


#Selecting a specific lat/long section 
lon_index <- which(ncin$dim$lon$vals > -11 | ncin$dim$lon$vals < 3)
lat_index <-  which(ncin$dim$lat$vals > 49 & ncin$dim$lat$vals < 60.9)


#Selecting only one point 
lon_index <- which.min(abs(lon - 0.3))
lat_index <- which.min(abs(lat - 54.2))
time_index <- which(format(sst_time, "%Y-%m-%d") == "2011-03-16")
sst[lon_index, lat_index, time_index]


#Picking out specific of data - this is one lat/long from 1870-2017
#Note that this code uses format and as.Date to convert the PCICt object 
#to a date object, to allow use of a date axis when plotting with ggplot2.
lon_index <- which.min(abs(lon - 0.3))
lat_index <- which.min(abs(lat - 54.2))
sst <- nc.get.var.subset.by.axes(ncin, "sst",
                                 axis.indices = list(X = lon_index,
                                                     Y = lat_index))
data_frame(time = sst_time, 
           sst = as.vector(sst)) %>%
  mutate(time = as.Date(format(time, "%Y-%m-%d"))) %>%
  ggplot(aes(x = time, y = sst)) + 
  geom_line() + 
  xlab("Date") + ylab("Temperature (C)") + 
  ggtitle("Daily measured sea-surface-temperature, 1870 - 2017",
          subtitle = "At model grid point nearest Scarborough, United Kingdom") + 
  theme_classic()


#Modelling temperature for a single day - need to change the code as this currently 
#gives a global picture 
library(ggmap)
library(viridis)
install.packages("weathermetrics")
library(weathermetrics)

time_index <- which(format(sst_time, "%Y-%m-%d") == "2016-07-16")
sst <- nc.get.var.subset.by.axes(ncin, "sst",
                                 axis.indices = list(T = time_index))
expand.grid(lon, lat) %>%
  rename(lon = Var1, lat = Var2) %>%
  mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
         sst = as.vector(sst)) %>% 
  #mutate(sst = convert_temperature(sst, "k", "c")) %>%
  ggplot() + 
  geom_point(aes(x = lon, y = lat, color = sst),
             size = 0.8) + 
  borders("world", colour="black", fill=NA) + 
  scale_color_viridis(name = "Temperature (C)") + 
  theme_void() + 
  coord_quickmap() + 
  ggtitle("Measured temperature on 1977-07-16",
          subtitle = "HadISST1") 

