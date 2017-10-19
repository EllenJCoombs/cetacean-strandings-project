
#GAM's pre and post CSIP
#Need to cut everything to 77 years (1913-1989)

library(mgcv)
library(broom)
#+s(Storms, k = 4)
#+s(Year) +s(Max_SST_temp) 
#+s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations)

#Pre CSIP 
Pre_CSIP_model <- read.csv("Pre_CSIP_model.csv")

PreCSIP_rd <- gam(Pre_CSIP_richness ~ offset(log(Population)) +s(Max_SST) +s(Storms, k = 5) +s(Max_K_index, k= 5),
                data=Pre_CSIP_model, method = "REML", family=tw(a=1.2))

#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(PreCSIP_rd)
plot(PreCSIP_rd)

par(mfrow=c(2,2))
gam.check(PreCSIP_rd)
#AIC (model comparison)
AIC(PreCSIP_rd)
#Visualisation 
vis.gam(PreCSIP_rc)

vis.gam(PreCSIP_ra, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")



#Stranding events 
PreCSIP_ed <- gam(Pre_CSIP_events ~ offset(log(Population)) +s(Year, k= 50) +s(Max_SST) +s(Storms, k = 6) +s(Max_K_index, k= 5),
                  data=Pre_CSIP_model, method = "REML", family=tw(a=1.2))
#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(PreCSIP_ed)
plot(PreCSIP_ed)

par(mfrow=c(2,2))
gam.check(PreCSIP_ed)
#AIC (model comparison)
AIC(PreCSIP_ed)
#Visualisation 
vis.gam(PreCSIP_ed)

vis.gam(PreCSIP_ra, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")



#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM

#Tidy multiple models at once 
Pre_CSIP_tidy <- list(PreCSIP_ra = PreCSIP_ra, PreCSIP_rb = PreCSIP_rb, PreCSIP_rc = PreCSIP_rc, PreCSIP_rd = PreCSIP_rd,
                      PreCSIP_ea = PreCSIP_ea, PreCSIP_eb = PreCSIP_eb, PreCSIP_ec = PreCSIP_ec, PreCSIP_ed = PreCSIP_ed) 

#Saving the tidy and glance datasets 
Pre_CSIP_coefs_tidy <- plyr::ldply(Pre_CSIP_tidy, tidy, .id = "model")
Pre_CSIP_coefs_glance <- plyr::ldply(Pre_CSIP_tidy, glance, .id = "model")

write.csv(Pre_CSIP_coefs_tidy, file = "Pre_CSIP_tidy.csv")
write.csv(Pre_CSIP_coefs_glance, file = "Pre_CSIP_glance.csv")


#check what type of models these are 
#sapply(Pre_CSIP_tidy, class)

#############
#Post CSIP 
#1990-2015 

#+s(Storms, k = 4)
#+s(Year) +s(Max_SST_temp) 
#+s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations)

#Pre CSIP 
Post_CSIP_model <- read.csv("Post_CSIP_model.csv")

PostCSIP_rd <- gam(Post_CSIP_richness ~ offset(log(Population)) +s(Year) +s(Max_SST) +s(Storms, k = 4) +s(Max_K_index, k= 4),
                  data=Post_CSIP_model, method = "REML", family=tw(a=1.2))

#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(PostCSIP_rd)
plot(PostCSIP_rd)

par(mfrow=c(2,2))
gam.check(PostCSIP_rd)
#AIC (model comparison)
AIC(PreCSIP_rc)
#Visualisation 
vis.gam(PostCSIP_rb)

vis.gam(PostCSIP_ra, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")


#####
#Stranding events 
PostCSIP_ed <- gam(Post_CSIP_events ~ offset(log(Population)) +s(Year) +s(Max_SST) +s(Storms, k = 4) +s(Max_K_index, k = 5),
                  data=Post_CSIP_model, method = "REML", family=tw(a=1.2))

#+s(Max_SST) +s(Storms, k = 4)
#+s(Max_K_index, k= 4)

summary(PostCSIP_ed)
plot(PostCSIP_ed)

par(mfrow=c(2,2))
gam.check(PostCSIP_ed)
#AIC (model comparison)
AIC(PostCSIP_ed)
#Visualisation 
vis.gam(PostCSIP_ec)

vis.gam(PostCSIP_ra, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")


#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM

#Tidy multiple models at once 
Post_CSIP_tidy <- list(PostCSIP_ra = PostCSIP_ra, PostCSIP_rb = PostCSIP_rb, PostCSIP_rc = PostCSIP_rc, PostCSIP_rd = PostCSIP_rd,
                      PostCSIP_ea = PostCSIP_ea, PostCSIP_eb = PostCSIP_eb, PostCSIP_ec = PostCSIP_ec, PostCSIP_ed = PostCSIP_ed) 

#Saving the tidy and glance datasets 
Post_CSIP_coefs_tidy <- plyr::ldply(Post_CSIP_tidy, tidy, .id = "model")
Post_CSIP_coefs_glance <- plyr::ldply(Post_CSIP_tidy, glance, .id = "model")

write.csv(Post_CSIP_coefs_tidy, file = "Post_CSIP_tidy.csv")
write.csv(Post_CSIP_coefs_glance, file = "Post_CSIP_glance.csv")


