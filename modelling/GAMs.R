
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
plot(b_t)

# get the AIC
AIC(b_t)

par(mfrow=c(2,2))
# compare k' column to edf if close, double k
gam.check(b_t)


# residuals look wacky (quantile-quantile plot not matching red line)
# increasing variance in resids. vs. linear pred. plot
# try quasi poisson?
b_t <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Storms, k = 4) +s(Max_K_index, k=4), data=Model_data, method = "REML",
           family=quasipoisson())

summary(b_t)
plot(b_t)

par(mfrow=c(2,2))
gam.check(b_t)


# lets be fancy!
# Tweedie
# set a=1.2 because things get weird if less than 1.2
b_t <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Storms, k = 4) +s(Max_K_index, k=4), data=Model_data, method = "REML",
           family=tw(a=1.2))

summary(b_t)
plot(b_t)

par(mfrow=c(2,2))
gam.check(b_t)

# make predictions - useful if I have loads of missing data 
b_t <- gam(Richness ~ offset(log(Population)) + s(Year), data=Model_data, method = "REML",
           family=tw(a=1.2))

plot_data <- Model_data
predict_Richness <- predict(b_t, newdata=Model_data, type="response", se.fit=TRUE)
plot_data$predict_Richness <- predict_Richness$fit
plot_data$upper_Richness <- predict_Richness$fit + 2*predict_Richness$se.fit
plot_data$lower_Richness <- predict_Richness$fit - 2*predict_Richness$se.fit

plot(plot_data[,c("Year", "Richness")])
# a bit jagged
lines(plot_data[,c("Year", "predict_Richness")])
lines(plot_data[,c("Year", "upper_Richness")], lty=2)
lines(plot_data[,c("Year", "lower_Richness")], lty=2)
rug(plot_data$Year)


