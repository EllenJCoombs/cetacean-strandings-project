
library(mgcv)

Model_data <- read.csv("Model_data.csv")
Model_data$X <- NULL 


b_t <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Storms, k=4), data=Model_data, method = "REML",
           family=poisson())

summary(b_t)
plot(b_t)


# not enough unique values of storms? How many are there?
unique(Model_data$Storms)


#Running my own models 
#Be aware of the k value - the model might be over complicating the data (e.g. assuming more values
#when we only have 4 different categories)

#Here this model is looking at Richness, offset by population and the smooths are "Year" and "Storms"
b_t <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Storms, k=4), data=Model_data, method = "REML",
           family=poisson())

unique(Model_data$Max_K_index)
#Just looking at geogmagnetic data and year 
#It has 5 so using a K value of 4 
b_t <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Max_K_index, k=4), data=Model_data, method = "REML",
           family=poisson())

summary(b_t)
plot(b_t)

#Almost identical to storm data 

#Modelling all variables - magnets, storms, year 
b_t <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Storms, k = 4) +s(Max_K_index, k=4), data=Model_data, method = "REML",
           family=poisson())

summary(b_t)
