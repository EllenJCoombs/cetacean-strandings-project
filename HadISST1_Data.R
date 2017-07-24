#Loading climate data into R 

#Required packages 
library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)

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
nlat2 <- dim(lat2)
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
chron(time,origin=c(tmonth, tday, tyear))


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

################################################################################
# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

# vector of `tmp` values
tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)

# create dataframe and add names
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste("sst",as.character(m), sep="_"))
head(na.omit(tmp_df01), 10)

write.csv(tmp_df01, file = "sst_tmp_1.csv")

#Save everything as a csv 
#set path and filename
csvpath <- "Users/ellencoombs/Desktop/sst_tmp_1.csv"
csvname <- "sst_tmp_1.csv"
csvfile <- paste(csvpath, csvname, sep="")
write.table(na.omit(csvname),csvfile, row.names=FALSE, sep=",")

# reshape the array into vector
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)


