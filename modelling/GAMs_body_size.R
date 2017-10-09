
#GAMs 
#Body size 

#+s(Storms, k = 4)
#+s(Year) +s(Max_SST_temp) 
#+s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations)

#Big body size 
#Big body size richness 
Big_model <- read.csv("Big_model.csv")
Big_model$X <- NULL

Big_rd <- gam(Big_richness ~ offset(log(Population)) +s(Year) +s(Max_SST) +s(Storms, k = 7) +s(Max_K_index, k= 5), data=Big_model, 
                method = "REML", family=tw(a=1.2))


#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(Big_rd)
plot(Big_rd)

par(mfrow=c(2,2))
gam.check(Big_rd)
#AIC (model comparison)
AIC(Big_rc)
#Visualisation 
vis.gam(Big_rc)

vis.gam(Big_rb, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")


#Big body size stranding count 
Big_ed <- gam(Big_events ~ offset(log(Population)) +s(Year) +s(Max_SST) +s(Storms, k = 7) +s(Max_K_index, k=5), 
                data=Big_model, method = "REML",
                family=tw(a=1.2))


#+s(Storms, k = 4) +s(Max_K_index, k=4)
#+s(Max_SST) +s(Storms, k = 4)

summary(Big_ed)
plot(Big_ed)

par(mfrow=c(2,2))
gam.check(Big_ed)

#AIC (model comparison)
AIC(Big_ed)

#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM

#Tidy multiple models at once 
Big_tidy <- list(Big_ra = Big_ra, Big_rb = Big_rb, Big_rc = Big_rc, Big_rd = Big_rd,
                   Big_ea = Big_ea, Big_eb = Big_eb, Big_ec = Big_ec, Big_ed = Big_ed) 

#Saving the tidy and glance datasets 
Big_coefs_tidy <- plyr::ldply(Big_tidy, tidy, .id = "model")
Big_coefs_glance <- plyr::ldply(Big_tidy, glance, .id = "model")

write.csv(Big_coefs_tidy, file = "Big_tidy.csv")
write.csv(Big_coefs_glance, file = "Big_glance.csv")


#check what type of models these are 
sapply(Big_tidy, class)


#######################
#Medium body size 

#Medium body size 
#Medium body size richness 
Medium_model <- read.csv("Medium_model.csv")
Medium_model$X <- NULL

Medium_rd <- gam(Medium_richness ~ offset(log(Population)) +s(Year, k = 70) +s(Max_SST) +s(Storms, k = 7) +s(Max_K_index, k= 4), data=Medium_model, 
              method = "REML", family=tw(a=1.2))


#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(Medium_rd)
plot(Medium_rd)

par(mfrow=c(2,2))
gam.check(Medium_rd)
#AIC (model comparison)
AIC(Medium_rd)
#Visualisation 
vis.gam(Medium_ra)

vis.gam(Medium_ra, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")


#Big body size stranding count 
Medium_ed <- gam(Medium_events ~ offset(log(Population)) +s(Year) +s(Max_SST) +s(Storms, k = 7) +s(Max_K_index, k=5), 
              data=Medium_model, method = "REML",
              family=tw(a=1.2))


#+s(Storms, k = 4) +s(Max_K_index, k=4)
#+s(Max_SST) +s(Storms, k = 4)

summary(Medium_ed)
plot(Medium_ed)

par(mfrow=c(2,2))
gam.check(Medium_ed)

#AIC (model comparison)
AIC(Medium_ed)

#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM

#Tidy multiple models at once 
Big_tidy <- list(Big_ra = Big_ra, Big_rb = Big_rb, Big_rc = Big_rc, Big_rd = Big_rd,
                 Big_ea = Big_ea, Big_eb = Big_eb, Big_ec = Big_ec, Big_ed = Big_ed) 

#Saving the tidy and glance datasets 
Big_coefs_tidy <- plyr::ldply(Big_tidy, tidy, .id = "model")
Big_coefs_glance <- plyr::ldply(Big_tidy, glance, .id = "model")

write.csv(Big_coefs_tidy, file = "Big_tidy.csv")
write.csv(Big_coefs_glance, file = "Big_glance.csv")




























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
