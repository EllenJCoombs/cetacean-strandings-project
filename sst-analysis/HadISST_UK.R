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
summary(lon)


lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)
head(lat)
summary(lat)

print(c(nlon,nlat))

#Time variable from the dataset 
# get time

#Time since when? 
ncin$dim$time$units
#Looking at the time units 
time <- ncvar_get(ncin,"time")
time


tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
#How many time units there are 
nt
#Days since...
tunits 

#What kind of calendar? 
ncin$dim$time$calendar
#Gregorian

#Get the the variable (tmp) and its attributes, and verify the size of the array.
# get temperature

tmp <- ncvar_get(ncin,"sst")
st <- dim(tmp)

tmp_array <- ncvar_get(ncin, "sst")

#This stuff was what I was playing around with 
#dlname <- ncatt_get(ncin, "sst","long_name")
#dunits <- ncatt_get(ncin,"sst","units")
#fillvalue <- ncatt_get(ncin, "sst","_FillValue")
#dim(tmp_array)


# convert time -- split the time units string into fields
#tustr <- strsplit(tunits$value, " ")
#tdstr <- strsplit(unlist(tustr)[3], "-")
#tmonth <- as.integer(unlist(tdstr)[2])
#tday <- as.integer(unlist(tdstr)[3])
#tyear <- as.integer(unlist(tdstr)[1])
#new_time <- chron(time,origin=c(tmonth, tday, tyear))

#new_time

# replace netCDF fill values with NA's - no fill values found 
#tmp_array[tmp_array==fillvalue$value] <- NA

#How much of the data are land points? 
#length(na.omit(as.vector(tmp_array[,,1])))


# get a single slice or layer (January)
#Changed to temp1[m]
#m <- 1
#tmp_slice <- tmp[,,m]
#dim(tmp_slice)

#Using the time slice above to look at January temps 
#image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

#All temps 
# levelplot of the slice - doesn't look right 
#grid <- expand.grid(lon=lon, lat = lat)
#cutpts <- c(-20,-10,-5,-2,0,2,5,10,20,30)
#levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          #col.regions=(rev(brewer.pal(10,"RdBu"))))

###########################################################
#Unsure why this doesn't work 
#LonIdx <- which( ncin$dim$lon$vals > -11 | ncin$dim$lon$vals < 3)
#LatIdx <- which( ncin$dim$lat$vals > 49 & ncin$dim$lat$vals < 60.9)
#MyVariable <- ncvar_get(ncin, "sst_time")[ LonIdx, LatIdx]


ncin$dim$time$units

install.packages("ncdf4.helpers")
library(ncdf4.helpers)
install.packages("PCICt")
library(PCICt)
library(dplyr)

#This use of non-standard calendars messes up the calculations of as.Date, 
#so you must use a more specialized function to read in the time variable of the
#climate model output and map it to standard calendar dates. 
#You can do this with the nc.get.time.series function from the ncdf4.helpers package
#(you should also load the PCICt package to use this function):

#This sets the date to a standard: YYYYMMDD and time as well
sst_time <- nc.get.time.series(ncin, v = "sst",
                               time.dim.name = "time")
sst_time[c(1:3, length(sst_time) - 2:0)]

#Getting sst data 
sst <- ncvar_get(ncin, "sst")

#This variable is in a 3-dimensional array, with dimensions ordered as first longitude, then latitude, then time:
#Checking on SST data 
dim(sst)


length(sst_time)
summary(sst_time)

#Selecting a specific time point and place 
#Answers are in celcius? CHECK
#Looking at specific lat/long in September 2011 
lon_index <- which.min(abs(lon - -5.72))
lat_index <- which.min(abs(lat - 50.06))
time_index <- which(format(sst_time, "%Y-%m-%d") == "2011-09-16")
sst[lon_index, lat_index, time_index]

#Selecting a specific lat/long section 
#lon_index <- which(ncin$dim$lon$vals > -11 | ncin$dim$lon$vals < 3)
#lat_index <-  which(ncin$dim$lat$vals > 49 & ncin$dim$lat$vals < 60.9)

ts.bounds <- nc.make.time.bounds(ts, unit="month")
#sst[lon_index, lat_index, time_index]

#Selecting only one location (lat/long)
lon_index <- which.min(abs(lon - 1.75))
lat_index <- which.min(abs(lat - 52.48))
time_index <- as.PCICt(c("1913-01-01", "2016-01-01"), cal="360")
sst[lon_index, lat_index, time_index]


#Picking out specifics of data - this is one lat/long from 1870-2017
#Note that this code uses format and as.Date to convert the PCICt object 
#to a date object, to allow use of a date axis when plotting with ggplot2.
lon_index <- which.min(abs(lon - - 6.04)) 
lat_index <- which.min(abs(lat - 52.98))

sst <- nc.get.var.subset.by.axes(ncin, "sst",
                                 axis.indices = list(X = lon_index,
                                                     Y = lat_index))

data_frame(time = sst_time,
           sst = as.vector(sst)) %>% 
  mutate(time = as.Date(format(sst_time, "%Y-%m-%d"))) %>%
  #Write csv is for extracting csv data and compare each site 
  #write.csv(file = "Wicklow_temp.csv")
#read csv 
#Ireland_Wicklow <- read.csv("Wicklow_temp.csv")


#Add the below for graphing 
  ggplot(aes(x = time, y = sst)) + 
  geom_line() + 
  xlab("Date") + ylab("Temperature (C)") + 
  ggtitle("Daily measured sea-surface-temperature, 1870 - 2017",
          subtitle = "At model grid point nearest Dover, UK") + 
  theme_classic()


########################################################################################
#Pulling together all of the data 

#Ireland data binding and cleaning 
Ireland <- bind_cols(Ireland_BrowHead, Ireland_Cleggan, Ireland_Wicklow)
  
Ireland <- Ireland %>%
    dplyr::rename(sst_Browhead_IR = sst) %>%
    dplyr::rename(sst_Cleggan_IR = sst1) %>%
    dplyr::rename(sst_Wicklow_IR = sst2) 

Ireland <- Ireland %>%
select(time, sst_Browhead_IR, sst_Cleggan_IR, sst_Wicklow_IR)
  

#UK data binding and cleaning 
UK <- bind_cols(UK_Ballyhillin, UK_Barrow, UK_Dover, UK_GoreCliff, UK_Holyhead, UK_JohnOGroats, 
                UK_LandsEnd, UK_Lindisfarne, UK_Lowerstoft, UK_Mull, UK_Shetland)

  
  UK <- UK %>%
    dplyr::rename(sst_Ballyhillin_UK = sst) %>%
    dplyr::rename(sst_Barrow_UK = sst1) %>%
    dplyr::rename(sst_Dover_UK = sst2) %>%
    dplyr::rename(sst_GoreCliff_UK = sst3) %>%
    dplyr::rename(sst_Holyhead_UK = sst4) %>%
    dplyr::rename(sst_JohnOGroats_UK = sst5) %>%
    dplyr::rename(sst_LandsEnd_UK = sst6) %>%
    dplyr::rename(sst_Lindisfarne_UK = sst7) %>%
    dplyr::rename(sst_Lowerstoft_UK = sst8) %>%
    dplyr::rename(sst_Mull_UK = sst9) %>%
    dplyr::rename(sst_Shetland_UK = sst10) 
  
  UK <- UK %>%
    select(time, sst_Ballyhillin_UK, sst_Barrow_UK, sst_Dover_UK, sst_GoreCliff_UK, sst_Holyhead_UK,
           sst_JohnOGroats_UK, sst_LandsEnd_UK, sst_Lindisfarne_UK, sst_Lowerstoft_UK, sst_Mull_UK, 
           sst_Shetland_UK)
  
#Binding the UK and Ireland 
UK_Ireland <- bind_cols(UK, Ireland)
#Removing the second time variable (from the Ireland dataset)
UK_Ireland$time1 <- NULL

write.csv(UK_Ireland, file = "UK_Ireland_SST.csv")
#Have added the mean

UK_Ireland_SST <- read.csv("UK_Ireland_SST.csv")


#Plotting the temperature data 

plot(UK_Ireland_SST)
#group = 1 tells R that the points should be connected for one group 
#group = g no lines drawn
#group = 2 lines are drawn for two seperate groups 

#This code plots Ireland 
ggplot() + 
  geom_line(data = Ireland_BrowHead, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = Ireland_Cleggan, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = Ireland_Wicklow, aes(x = time, y = sst, group = 1)) + 
  labs(y = "SST",
        x = "time") +
  ggtitle("Daily measured sea-surface-temperature, 1870 - 2017",   
          subtitle = "At model grid point nearest several Irish locations") + 
  theme_classic() 


#Plots of the UK and Ireland
ggplot() + 
  geom_line(data = UK_Ballyhillin, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = UK_Barrow, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = UK_Dover, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = UK_GoreCliff, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = UK_Holyhead, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = UK_JohnOGroats, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = UK_LandsEnd, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = UK_Lindisfarne, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = UK_Lowerstoft, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = UK_Mull, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = UK_Shetland, aes(x = time, y = sst, group = 1)) +
  geom_line(data = Ireland_BrowHead, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = Ireland_Cleggan, aes(x = time, y = sst, group = 1)) + 
  geom_line(data = Ireland_Wicklow, aes(x = time, y = sst, group = 1)) + 
  geom_smooth(span = 0.3) +
  labs(y = "SST",
       x = "time") +
  ggtitle("Daily measured sea-surface-temperature, 1870 - 2017",   
          subtitle = "At model grid points nearest to several UK and Ireland locations") + 
  theme_classic() 


#Plot the mean SST
UK_mean_SST <- UK_Ireland_SST %>%
  dplyr::select(time, UK_mean) 

#Plotting UK mean temperature 
bb1 <- ggplot() + 
  geom_point(data = UK_mean_SST, aes(x = time, y = UK_mean, group = 1)) + 
  labs(y = "SST",
       x = "time") +
  ggtitle("Monthly mean sea-surface-temperature, 1870 - 2017",   
          subtitle = "UK mean") + 
  theme_classic()

#Trying to add a trend line - not sure how to get this to work 
#bb1<- bb1+ geom_smooth(span = 0.5) + 
  #scale_colour_gradient(low='yellow', high='#de2d26') 

bb1



##########################################################################################
#Extracting yearly max SST data 

#Renaming uk_mean_SST columns 

UK_mean_SST <- UK_mean_SST %>%
  dplyr::rename(Date = time)

#Had to make it a as.Date first 
UK_mean_SST <- mutate(UK_mean_SST, Date = as.Date(Date))

#Splitting SST into day, month, year 
df <- data.frame(date = UK_mean_SST$Date,
                 year = as.numeric(format(UK_mean_SST$Date, format = "%Y")),
                 month = as.numeric(format(UK_mean_SST$Date, format = "%m")),
                 day = as.numeric(format(UK_mean_SST$Date, format = "%d")))

#Binding SST and dates/months/years and removing the extra 'date' 
SST_day_month_year <- bind_cols(df, UK_mean_SST) 
SST_day_month_year$Date <- NULL

#Average SST for each of the months from 1912 - 2016
aggregate(SST_day_month_year$UK_mean, by = list(SST_day_month_year$year), max)
SST_yearly_max <- aggregate(UK_mean ~ year, data = SST_day_month_year, max)

#Renaming the max_year temp and also selcting from 1913:2015 only (as my data starts
#1912 and ends halfway through 2016)
SST_yearly_max <- SST_yearly_max %>%
  dplyr::rename(year_max = UK_mean) %>%
  filter(year %in% c(1913:2015))

ggplot(data = SST_yearly_max, aes(x = year, y = year_max)) +
  geom_line() + 
  xlab("Year") + ylab("Maximum recorded SST ("~degree~"C)") + 
  ggtitle("Yearly maximum UK sea-surface-temperature (SST) (1913 - 2015)",
          subtitle = "Average taken from 14 sites closest to chosen model grid points") + 
  theme_classic()


write.csv(SST_yearly_max, file = "SST_yearly_max.csv")


#####################################################
#Extracting monthly maximum.... 
#Need to go back to the original dataset as the above uses the mean 

#CAN'T GET THIS TO WORK 
#UK_Ireland_SST <- UK_Ireland_SST %>%
  #dplyr::rename(Date = time)

#UK_Ireland_SST <- mutate(UK_Ireland_SST, Date = ymd(Date))

#ef <- data.frame(date = UK_Ireland_SST$Date,
                 #year = as.numeric(format(UK_Ireland_SST$Date, format = "%Y")),
                 #month = as.numeric(format(UK_Ireland_SST$Date, format = "%m")),
                 #day = as.numeric(format(UK_Ireland_SST$Date, format = "%d")))


#max(UK_Ireland_SST_dates[1,5:19])
#UK_Ireland_SST$max <- apply(UK_Ireland_SST[,5:19], max)
#head(UK_Ireland_SST, 10)


UK_Ireland_SST_max <- read.csv("UK_Ireland_SST.csv")





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
  mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
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

