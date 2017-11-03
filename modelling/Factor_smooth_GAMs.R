#Factor smooth splines 

#One for north and one for south? That you used to fit the current model
#Create a new column in each called northsouth and then let all the north ones 
#have the value "north" and south have "south". 
#Stack them on top of each other (rbind()). 
#SST geomag later 


#Read in the north and south data frames (from the modelling folder)
#Stranding events and richness 

library(mgcv)
library(plyr)
library(dplyr)

#The models are currently in the modelling file 
North_model <- read.csv("North_model.csv")
North_model$X <- NULL
South_model <- read.csv("South_model.csv")
South_model$X <- NULL

#Adding a new column to both the north and south data 
North_model["Northsouth"] <- "North"
South_model["Northsouth"] <- "South"

#Change the variable names so they will bind 
North_model <- North_model %>% 
  dplyr::rename(Richness = North_richness) %>%
  dplyr::rename(Stranding_events = North_events)

South_model <- South_model %>% 
  dplyr::rename(Richness = South_richness) %>%
  dplyr::rename(Stranding_events = South_events)


#Bind the two datasets on top of one another 
North_South_model <- bind_rows(North_model, South_model)
#Need to make the character a factor - was getting error messages 
North_South_model$Northsouth <- as.factor(North_South_model$Northsouth)

#All records Richness - GAM
All_ns <- gam(Richness ~ offset(log(Population)) +s(Year, Northsouth, bs="fs"), data= North_South_model, method = "REML",
              family=tw(a=1.2))
#Check 
sapply(North_South_model, class)

write.csv(North_South_model, file = "North_South_model.csv")

#The following covariates can be inserted 
#+s(Storms, k = 4) +s(Max_K_index, k=4)

summary(All_ns)
plot(All_ns)

par(mfrow=c(2,2))
gam.check(All_ns)

#AIC (model comparison)
AIC(All_ns)

##################################################################################################
#Body size 
#Body size models are in the modelling folder 
Big_model <- read.csv("Big_model.csv")
Big_model$X <- NULL 

#Medium 
Medium_model <- read.csv("Medium_model.csv")
Medium_model$X <- NULL 

#Small 
Small_model <- read.csv("Small_model.csv")
Small_model$X <- NULL 

#Adding new columns to the above 
Big_model["Bodysize"] <- "Big"
Medium_model["Bodysize"] <- "Medium"
Small_model["Bodysize"] <- "Small"


#Changing the names of the columns so that they can be bound 
Big_model <- Big_model %>%
  dplyr::rename(Richness = Big_richness) %>%
  dplyr::rename(Stranding_events = Big_events)

#Medium 
Medium_model <- Medium_model %>%
  dplyr::rename(Richness = Medium_richness) %>%
  dplyr::rename(Stranding_events = Medium_events)

#Small 
Small_model <- Small_model %>%
  dplyr::rename(Richness = Small_richness) %>%
  dplyr::rename(Stranding_events = Small_events)


Body_size_model <- bind_rows(Big_model, Medium_model, Small_model)
#Turn body size into a factor - keep getting error messages as it was a character before 
Body_size_model$Bodysize <- as.factor(Body_size_model$Bodysize)
#check
sapply(Body_size_model, class)

write.csv(North_South_model, file = "North_South_model.csv")


#GAMs
#All records Richness - GAM
All_bs <- gam(Richness ~ offset(log(Population)) +s(Storms, k = 5, Bodysize, bs="fs"), data= Body_size_model, method = "REML",
              family=tw(a=1.2))

#The following covariates can be inserted 
#+s(Storms, k = 4) +s(Max_K_index, k=4)

summary(All_bs)
plot(All_bs)

par(mfrow=c(2,2))
gam.check(All_bs)

#AIC (model comparison)
AIC(All_bs)


#Sometimes it is helpful to see how the linear predictor or expected response would 
#vary with 2 predictors, if all the others were held fixed at some value. vis.gam allows this.

vis.gam(All_bs)

vis.gam(All_bs, n.grid = 50, theta = 35, phi = 32, zlab = "additional",
        ticktype = "detailed", color = "topo")



