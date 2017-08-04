#Loading climate data into R 

#Required packages 
library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#required download 
install.packages("ncdf4", repos = NULL, type="source")
ncname <- "cru10min30_tmp" 

#Set path and filename 
ncpath <- "Users/ellencoombs/Desktop/HadISST_sst.nc"
ncname <- "HadISST_sst.nc"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tmp"  # note: tmp means temperature (not temporary)

#Reading in .nc data 
ncin <- nc_open("HadISST_sst.nc")


#Having a look at what's in the files 
attributes(ncin)$names
#[1] "filename"    "writable"    "id"          "safemode"    "format"      "is_GMT"     
#[7] "groups"      "fqgn2Rindex" "ndims"       "natts"       "dim"         "unlimdimid" 
#[13] "nvars"       "var"        


#Variables 
attributes(ncin$var)$names

#[1] "time_bnds" "sst" 

print(ncin)
ncin

#Taking a look at the data 
ncin_data <- ncvar_get(ncin, attributes(ncin$var)$names[1])
print(ncin_data)


#Global attributes 
# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")



# get longitude and latitude
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
head(lon)


lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

#Time variable from the dataset 
# get time
time <- ncvar_get(ncin,"time")
time


tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt
tunits 

#Get the the variable (tmp) and its attributes, and verify the size of the array.
# get temperature

tmp <- ncvar_get(ncin,"sst")
st <- dim(tmp)

tmp_array <- ncvar_get(ncin, "sst")
dlname <- ncatt_get(ncin, "sst","long_name")
dunits <- ncatt_get(ncin,"sst","units")
fillvalue <- ncatt_get(ncin, "sst","_FillValue")
dim(tmp_array)


# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
new_time <- chron(time,origin=c(tmonth, tday, tyear))

new_time

# replace netCDF fill values with NA's - no fill values found 
tmp_array[tmp_array==fillvalue$value] <- NA

#How much of the data are land points? 
length(na.omit(as.vector(tmp_array[,,1])))


# get a single slice or layer (January)
#Changed to temp1[m]
m <- 1
tmp_slice <- tmp[,,m]
dim(tmp_slice)

#Using the time slice above to look at January temps 
image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

#All temps 
# levelplot of the slice - doesn't look right 
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

###########################################################
#Unsure why this doesn't work 
LonIdx <- which( ncin$dim$lon$vals > -11 | ncin$dim$lon$vals < 3)
LatIdx <- which( ncin$dim$lat$vals > 49 & ncin$dim$lat$vals < 60.9)
MyVariable <- ncvar_get(ncin, "sst_time")[ LonIdx, LatIdx]


ncin$dim$time$units

install.packages("ncdf4.helpers")
library(ncdf4.helpers)
install.packages("PCICt")
library(PCICt)

#This use of non-standard calendars messes up the calculations of as.Date, 
#so you must use a more specialized function to read in the time variable of the
#climate model output and map it to standard calendar dates. 
#You can do this with the nc.get.time.series function from the ncdf4.helpers package
#(you should also load the PCICt package to use this function):

sst_time <- nc.get.time.series(ncin, v = "sst",
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

ts.bounds <- nc.make.time.bounds(ts, unit="month")


#sst[lon_index, lat_index, time_index]

#Selecting only one point 
lon_index <- which.min(abs(lon - 0.3))
lat_index <- which.min(abs(lat - 54.2))
time_index <- as.PCICt(c("1913-01-01", "2016-01-01"), cal="360")
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
  mutate(time = as.Date(format(sst_time, "%Y-%m-%d"))) %>%
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

#This code maps the temp data for the whole planet 
time_index <- which(format(sst_time, "%Y-%m-%d") == "2016-07-16")
sst <- nc.get.var.subset.by.axes(ncin, "sst",
                                 axis.indices = list(T = time_index))
expand.grid(lon, lat) %>%
  rename(lon = Var1, lat = Var2) %>%
  mutate(lon = ifelse(lon > 3, -(11 - lon), lon),
         sst = as.vector(sst)) %>% 
  mutate(sst = convert_temperature(sst, "c", "k")) %>%
  ggplot() + 
  geom_point(aes(x = lon, y = lat, color = sst),
             size = 0.8) + 
  borders("world", colour="black", fill=NA) + 
  scale_color_viridis(name = "Temperature (C)") + 
  theme_void() + 
  coord_quickmap() + 
  ggtitle("Measured temperature on 1977-07-16",
          subtitle = "HadISST1") 


ts <- as.PCICt(c("1913-01-01", "2016-01-01"), cal="360")
ts.bounds <- nc.make.time.bounds(ts, unit="month")

ncin

###################################################
#Creating a dataframe 
LonStartIdx <- which(ncin$dim$lon$vals == -3)
LatStartIdx <- which( ncFile$dim$lat$vals == 49)

LonIdx <- which( ncin$dim$lon$vals > -3 | ncin$dim$lon$vals < 11)
LatIdx <- which( ncin$dim$lat$vals > 49 & ncin$dim$lat$vals < 60.9) 
#Don't know why this doesn't work 
MyVariable <- ncvar_get(ncin, sst)[ LonIdx, LatIdx]


#Made the nc file a csv file
sst_csv <- read.csv("sst_tmp_1.csv")

library(dplyr)
library(tidyverse)

tmp_df01 <- data.frame(cbind(lon, lat,tmp, new_time))
names(tmp_df01) <- c("lon","lat", "tmp" , "time", paste(dname,as.character(m), sep="_"))
head(na.omit(tmp_df01), 10)

write.csv(tmp_df01, file = "tmp_sst.csv")
sst_csv <- read.csv("tmp_sst.csv")

write.csv(new_time, file = "new_time.csv")
new_time <- read.csv("new_time.csv")

sst_csv <- sst_csv %>%
rename(lat = Var2) %>%
  rename(lon = Var1) 

sst_csv %>%
  filter(lat > 49.0) %>%
  filter(lat < 62.0) %>%
  filter(lon > -3) %>%
  filter(lon < 11)

sst_uk_csv
sst_csv

#Stripping out UK data 
write.csv(tmp_df01, file = "tmp_df01.csv")
test <- read.csv("tmp_df01.csv")
