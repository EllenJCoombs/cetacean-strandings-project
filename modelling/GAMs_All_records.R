#GAMs
#Models for all strandings 

library(mgcv)
library(broom)
#+s(Storms, k = 4)
#+s(Year) +s(Max_SST_temp) 
#+s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations)

All_model <- read.csv("Model_data.csv")
All_model$X <- NULL
All_model$X.1 <- NULL

All_ra <- gam(Total_richness ~ offset(log(Population)) +s(Year), data=All_model, 
                method = "REML", family=tw(a=1.2))

#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(All_ra)
plot(All_ra)

par(mfrow=c(2,2))
gam.check(All_ra)
#AIC (model comparison)
AIC(All_ra)
#Visualisation 
vis.gam(All_ra)

vis.gam(All_ra, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")


#All records stranding count 
All_ed <- gam(Total_events ~ offset(log(Population)) +s(Year, k = 80) +s(Max_SST) +s(Storms, k = 7) +s(Max_K_index, k=5), data= All_model, method = "REML",
              family=tw(a=1.2))


#+s(Storms, k = 4) +s(Max_K_index, k=4)

summary(All_ed)
plot(All_ed)

par(mfrow=c(2,2))
gam.check(All_ed)

#AIC (model comparison)
AIC(All_ed)

#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM 

#Tidy multiple models at once 
All_tidy <- list(All_ra = All_ra, All_rb = All_rb, All_rc = All_rc, All_rd = All_rd,
                 All_ea = All_ea, All_eb = All_eb, All_ec = All_ec, All_ed = All_ed) 

#Saving the tidy and glance datasets 
All_coefs_tidy <- plyr::ldply(All_tidy, tidy, .id = "model")
All_coefs_glance <- plyr::ldply(All_tidy, glance, .id = "model")

write.csv(All_coefs_tidy, file = "All_tidy.csv")
write.csv(All_coefs_glance, file = "All_glance.csv")


#check what type of models these are 
sapply(Small_tidy, class)


















#GAMs 

library(mgcv)
library(broom)
#+s(Storms, k = 4)
#+s(Year) +s(Max_SST_temp) 
#+s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations)

#Small body size 
#Small body size richness 
Small_model <- read.csv("Small_model.csv")
Small_model$X <- NULL

Small_ra <- gam(Small_richness ~ offset(log(Population)) +s(Year), data=Small_model, 
                method = "REML", family=tw(a=1.2))

#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(Small_ra)
plot(Small_ra)

par(mfrow=c(2,2))
gam.check(Small_ra)
#AIC (model comparison)
AIC(Small_rd)
#Visualisation 
vis.gam(Small_rb)

vis.gam(Small_d, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")

#Small body size stranding count 
Small_ed <- gam(Small_events ~ offset(log(Population)) +s(Year, k = 50) +s(Max_SST) +s(Storms, k = 7) 
                +s(Max_K_index, k=4), 
                data=Small_model, method = "REML",
                family=tw(a=1.2))
                  

#+s(Storms, k = 4) +s(Max_K_index, k=4)

summary(Small_ed)
plot(Small_ed)

par(mfrow=c(2,2))
gam.check(Small_ed)

#AIC (model comparison)
AIC(Small_ed)

#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM 

#Tidy multiple models at once 
Small_tidy <- list(Small_ra = Small_ra, Small_rb = Small_rb, Small_rc = Small_rc, Small_rd = Small_rd,
               Small_ea = Small_ea, Small_eb = Small_eb, Small_ec = Small_ec, Small_ed = Small_ed) 

#Saving the tidy and glance datasets 
Small_coefs_tidy <- plyr::ldply(Small_tidy, tidy, .id = "model")
Small_coefs_glance <- plyr::ldply(Small_tidy, glance, .id = "model")

write.csv(Small_coefs_tidy, file = "Small_tidy.csv")
write.csv(Small_coefs_glance, file = "Small_glance.csv")


#check what type of models these are 
sapply(Small_tidy, class)
