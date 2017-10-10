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
Mysticete_ed <- gam(Mysticete_events ~ offset(log(Population)) +s(Year) +s(Max_SST) +s(Storms, k = 7) +s(Max_K_index, k = 5), 
              data=Mysticete_model, method = "REML",
              family=tw(a=1.2))


#+s(Storms, k = 4) +s(Max_K_index, k=4)
#+s(Max_SST) +s(Storms, k = 4)

summary(Mysticete_ed)
plot(Mysticete_ed)

par(mfrow=c(2,2))
gam.check(Mysticete_ed)

#AIC (model comparison)
AIC(Mysticete_ed)

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
