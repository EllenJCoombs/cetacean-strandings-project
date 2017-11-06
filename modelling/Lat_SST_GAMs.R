#GAMs
#Models for lat and SST

library(mgcv)
library(broom)
library(ggplot2)
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


#Species specifics ============================================================================
#Strip out species from Model_data_wlat 

#Striped dolphins first 

S_coeruleoalba_wlat <- Model_data_wlat %>%
  filter(Species == "Stenella coeruleoalba") 

#Model the above
SC_lat <- gam(Maximum_latitude ~ +s(Year, Northsouth, bs="fs"), data = S_coeruleoalba_wlat, 
               method = "REML", family=gaussian())

summary(SC_lat)
plot(SC_lat)

par(mfrow=c(2,2))
gam.check(SC_lat)

#Plot of max Striped latitude 
ggplot() + 
  geom_point(data = S_coeruleoalba_wlat, aes(x = Year, y = Maximum_latitude)) + 
  theme_bw()


#White beaked dolphins 
L_albirostris_wlat <- Model_data_wlat %>%
  filter(Species == "Lagenorhynchus albirostris")

#Model the above 
L_Alb_lat <- gam(Maximum_latitude ~ +s(Year, Northsouth, bs="fs"), data = L_albirostris_wlat, 
              method = "REML", family=gaussian())


L_albirostris <- UK_IRL_stranding_events %>%
  filter(Name.Current.Sci == "Lagenorhynchus albirostris")

L_albirostris_north <- L_albirostris %>%
  filter(Latitude > 55.5)

L_albirostris_south <- L_albirostris %>%
  filter(Latitude < 55.5)

ggplot() + 
  geom_point(data = L_albirostris_north, aes(x = Year, y = Latitude, colour = "blue")) + 
  geom_point(data = L_albirostris_south, aes(x = Year, y = Latitude, colour = "red")) + 
  theme_bw()


summary(L_Alb_lat)
plot(L_Alb_lat)

par(mfrow=c(2,2))
gam.check(L_Alb_lat)

#Plot of max White beaked dolphin latitude 
ggplot() + 
  geom_point(data = L_albirostris_wlat , aes(x = Year, y = Maximum_latitude)) + 
  theme_bw()


#Fin whale 
B_physalus_wlat <- Model_data_wlat %>%
  filter(Species == "Balaenoptera physalus")

#Model the above 
BP_lat <- gam(Maximum_latitude ~ +s(Year, Northsouth, bs="fs"), data = B_physalus_wlat, 
                 method = "REML", family=gaussian())


summary(BP_lat)
plot(BP_lat)

par(mfrow=c(2,2))
gam.check(BP_lat)

#Plot of max Fin latitude 
ggplot() + 
  geom_point(data = B_physalus_wlat, aes(x = Year, y = Maximum_latitude)) + 
  theme_bw()


#Common dolphins 
D_delphis_wlat <- Model_data_wlat %>%
  filter(Species == "Delphinus delphis")

#Model the above 
DD_lat <- gam(Maximum_latitude ~ +s(Year, Northsouth, bs="fs"), data = D_delphis_wlat, 
              method = "REML", family=gaussian())


summary(DD_lat)
plot(DD_lat)

par(mfrow=c(2,2))
gam.check(DD_lat)

#Plot of max Fin latitude 
ggplot() + 
  geom_point(data = D_delphis_wlat, aes(x = Year, y = Maximum_latitude)) + 
  theme_bw()


