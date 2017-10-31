#Factor smooth splines 

#One for north and one for south? That you used to fit the current model
#Create a new column in each called northsouth and then let all the north ones 
#have the value "north" and south have "south". 
#Stack them on top of each other (rbind()). 
#SST geomag later 


#Read in the north and south data frames (from the modelling folder)
#Stranding events and richness 

North_model <- read.csv("North_model.csv")
North_model$X <- NULL
South_model <- read.csv("South_model.csv")
South_model$X <- NULL

#Adding a new column to both the north and south data 
North_model["Northsouth"] <- "North"
South_model["Northsouth"] <- "South"

#Change the variable names so they will bind 
North_model <- North_model %>% 
  rename(Richness = North_richness) %>%
  rename(Stranding_events = North_events)

South_model <- South_model %>% 
  rename(Richness = South_richness) %>%
  rename(Stranding_events = South_events)


#Bind the two datasets on top of one another 
North_South_model <- bind_rows(North_model, South_model)


#All records Richness 
All_ns <- gam(Richness ~ offset(log(Population)) +s(Year, Northsouth, bs="fs"), data= North_South_model, method = "REML",
              family=tw(a=1.2))

sapply(North_South_model, class)

View(North_South_model$Northsouth)
View(data.frame(u = North_South_model$Northsouth))

write.csv(North_South_model, file = "North_South_model.csv")

#+s(Storms, k = 4) +s(Max_K_index, k=4)

summary(All_ed)
plot(All_ed)

par(mfrow=c(2,2))
gam.check(All_ed)

#AIC (model comparison)
AIC(All_ed)
s(year, northsouth, bs="fs")
