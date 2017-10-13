#Parorder GAMs

library(mgcv)
library(broom)

#+s(Storms, k = 4)
#+s(Year) +s(Max_SST_temp) 
#+s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations)

#Big body size 
#Big body size richness 
Mysticete_model <- read.csv("Mysticete_model.csv")
Mysticete_model$X <- NULL

Mysticete_rd <- gam(Mysticete_richness ~ offset(log(Population)) +s(Year) +s(Max_SST) +s(Storms, k = 7) +s(Max_K_index, k= 5), data=Mysticete_model, 
              method = "REML", family=tw(a=1.2))


#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(Mysticete_rd)
plot(Mysticete_rd)

par(mfrow=c(2,2))
gam.check(Mysticete_rd)
#AIC (model comparison)
AIC(Mysticete_rd)
#Visualisation 
vis.gam(Mysticete_rd)

vis.gam(Mysticete_ra, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")


#Mysticete stranding count 
Mysticete_ec <- gam(Mysticete_events ~ offset(log(Population)) +s(Year) +s(Max_SST) +s(Storms, k = 7), 
              data=Mysticete_model, method = "REML",
              family=tw(a=1.2))


#+s(Storms, k = 4) +s(Max_K_index, k=4)
#+s(Max_SST) +s(Storms, k = 4)

summary(Mysticete_ec)
plot(Mysticete_ec)

par(mfrow=c(2,2))
gam.check(Mysticete_ec)

#AIC (model comparison)
AIC(Mysticete_ec)

#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM

#Tidy multiple models at once 
Mysticete_tidy <- list(Mysticete_ra = Mysticete_ra, Mysticete_rb = Mysticete_rb, Mysticete_rc = Mysticete_rc, Mysticete_rd = Mysticete_rd,
                       Mysticete_ea = Mysticete_ea, Mysticete_eb = Mysticete_eb, Mysticete_ec = Mysticete_ec, Mysticete_ed = Mysticete_ed) 

#Saving the tidy and glance datasets 
Mysticete_coefs_tidy <- plyr::ldply(Mysticete_tidy, tidy, .id = "model")
Mysticete_coefs_glance <- plyr::ldply(Mysticete_tidy, glance, .id = "model")

write.csv(Mysticete_coefs_tidy, file = "Mysticete_tidy.csv")
write.csv(Mysticete_coefs_glance, file = "Mysticete_glance.csv")


#check what type of models these are 
sapply(Mysticete_tidy, class)



################
#Odontocete 

library(mgcv)
library(broom)

#+s(Storms, k = 4)
#+s(Year) +s(Max_SST_temp) 
#+s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations)

#Odontocete 
Odontocete_model <- read.csv("Odontocete_model.csv")
Odontocete_model$X <- NULL

Odontocete_ra <- gam(Odontocete_richness ~ offset(log(Population)) +s(Year) +s(Max_SST) +s(Storms, k = 7), data=Odontocete_model, 
                    method = "REML",  family=tw(a=1.2))


#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(Odontocete_rc)
plot(Odontocete_rc)

par(mfrow=c(2,2))
gam.check(Odontocete_rc)
#AIC (model comparison)
AIC(Odontocete_rc)
#Visualisation 
vis.gam(Odontocete_rd)

vis.gam(Odontocete_rd, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")


#Mysticete stranding count 
Odontocete_ed <- gam(Odontocete_events ~ offset(log(Population)) +s(Year, k = 50) +s(Max_SST) +s(Storms, k = 7) +s(Max_K_index, k=5), 
                    data=Odontocete_model, method = "REML",
                    family=tw(a=1.2))


#+s(Storms, k = 4) +s(Max_K_index, k=4)
#+s(Max_SST) +s(Storms, k = 4)

summary(Odontocete_ed)
plot(Odontocete_ed)

par(mfrow=c(2,2))
gam.check(Odontocete_ed)

#AIC (model comparison)
AIC(Odontocete_eb)

#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM

#Tidy multiple models at once 
Odontocete_tidy <- list(Odontocete_ra = Odontocete_ra, Odontocete_rb = Odontocete_rb, Odontocete_rc = Odontocete_rc, Odontocete_rd = Odontocete_rd,
                        Odontocete_ea = Odontocete_ea, Odontocete_eb = Odontocete_eb, Odontocete_ec = Odontocete_ec, Odontocete_ed = Odontocete_ed) 

#Saving the tidy and glance datasets 
Odontocete_coefs_tidy <- plyr::ldply(Odontocete_tidy, tidy, .id = "model")
Odontocete_coefs_glance <- plyr::ldply(Odontocete_tidy, glance, .id = "model")

write.csv(Odontocete_coefs_tidy, file = "Odontocete_tidy.csv")
write.csv(Odontocete_coefs_glance, file = "Odontocete_glance.csv")


#check what type of models these are 
sapply(Mysticete_tidy, class)




