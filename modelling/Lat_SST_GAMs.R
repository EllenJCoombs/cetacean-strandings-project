#GAMs
#Models for lat and SST

library(mgcv)
library(broom)
#+s(Storms, k = 4)
#+s(Year) +s(Max_SST_temp) 
#+s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations)

Model_data_wlat <- read.csv("Model_data_wlat.csv")
Model_data_wlat$X <- NULL 

All_lat <- gam(Maximum_latitude ~ offset(log(Population)) +s(Max_SST) +s(Year), data = Model_data_wlat, 
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
