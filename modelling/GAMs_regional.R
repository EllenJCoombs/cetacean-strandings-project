
#GAMs for North/South UK devide 

North_model <- read.csv("North_model.csv")

library(mgcv)
library(broom)
#+s(Storms, k = 4)
#+s(Year) +s(Max_SST_temp) 
#+s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations)

#Small body size 
#Small body size richness 
North_model <- read.csv("North_model.csv")
Small_model$X <- NULL

North_ra <- gam(North_richness ~ offset(log(Population)) +s(Year, k = 100),
                data=North_model, 
                method = "REML", 
                family=tw(a=1.2))

#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(North_ra)
plot(North_ra)

par(mfrow=c(2,2))
gam.check(North_ra)
#AIC (model comparison)
AIC(North_ra)
#Visualisation 
vis.gam(North_rc)

vis.gam(North_rc, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")

################
#North stranding events 
North_ed <- gam(North_events ~ offset(log(Population)) +s(Year, k = 50) +s(Max_SST) +s(Storms, k = 7) +s(Max_K_index, k= 5), 
                data=North_model, method = "REML",
                family=tw(a=1.2))

#+s(Storms, k = 4) +s(Max_K_index, k=4)

summary(North_ed)
plot(North_ed)

par(mfrow=c(2,2))
gam.check(North_ed)

#AIC (model comparison)
AIC(North_ed)


#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM

#Tidy multiple models at once 
North_tidy <- list(North_ra = North_ra, North_rb = North_rb, North_rc = North_rc, North_rd = North_rd,
                   North_ea = North_ea, North_eb = North_eb, North_ec = North_ec, North_ed = North_ed) 

#Saving the tidy and glance datasets 
North_coefs_tidy <- plyr::ldply(North_tidy, tidy, .id = "model")
North_coefs_glance <- plyr::ldply(North_tidy, glance, .id = "model")

write.csv(North_coefs_tidy, file = "North_tidy.csv")
write.csv(North_coefs_glance, file = "North_glance.csv")


#check what type of models these are 
sapply(North_tidy, class)


