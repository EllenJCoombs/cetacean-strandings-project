#GAMs
#Models for lat and SST

library(mgcv)
library(broom)
#+s(Storms, k = 4)
#+s(Year) +s(Max_SST_temp) 
#+s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations)

#From the modelling folder 
Model_data_wlat <- read.csv("Model_data_wlat.csv")
Model_data_wlat$X <- NULL 
#What would my offset be?
All_lat <- gam(Maximum_latitude ~ +s(Max_SST) +s(Year), data = Model_data_wlat, 
              method = "REML", family=gaussian())

#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(All_lat)
plot(All_lat)

par(mfrow=c(2,2))
gam.check(All_lat)
#AIC (model comparison)
AIC(All_ra)
#Visualisation 
vis.gam(All_ra)

vis.gam(All_ra, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")



#Add an factor smooth interaction to these GAMs
#Here - add a northsouth column

#Adding new columns to the above with NA at first 
Model_data_wlat["Northsouth"] <- "NA"
#Make the data in that column 
Model_data_wlat$Northsouth <- Model_data_wlat$Maximum_latitude
#Changing the data in the model from max lat to north south 
Model_data_wlat$Northsouth[Model_data_wlat$Northsouth > 55.5] <- "North"
Model_data_wlat$Northsouth[Model_data_wlat$Northsouth < 55.5] <- "South"
Model_data_wlat$Northsouth[Model_data_wlat$Northsouth == 55.5] <- "South"

#Need to make the character a factor (as before) - was getting error messages 
Model_data_wlat$Northsouth <- as.factor(Model_data_wlat$Northsouth)

#Run this as a factor smooth in the GAM 
All_lat <- gam(Maximum_latitude ~ +s(NAO_index, Northsouth, bs="fs") +s(Year), data = Model_data_wlat, 
               method = "REML", family=gaussian())

#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(All_lat)
plot(All_lat)

par(mfrow=c(2,2))
gam.check(All_lat)
#AIC (model comparison)
AIC(All_ra)


