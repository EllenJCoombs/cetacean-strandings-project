
library(mgcv)

Model_data <- read.csv("Model_data.csv")
Model_data$X <- NULL 


b_a <- gam(Richness ~ offset(log(Population)) + s(Year), data=Model_data, method = "REML",
           family=poisson())

summary(b_a)
plot(b_a)


# not enough unique values of storms? How many are there? 
#This is useful when deciding the K value 
unique(Model_data$Storms)

unique(Model_data$Organisations)

unique(Model_data$Max_K_index)

#Running my own models 
#Be aware of the k value - the model might be over complicating the data 
#(e.g. assuming more values
#when we only have 4 different categories)

#Here this model is looking at Richness, offset by population and the smooths are "Year" and "Storms"
b_b <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Storms, k=4), data=Model_data, method = "REML",
           family=poisson())

summary(b_b)

#Just looking at geogmagnetic data and year 
unique(Model_data$Max_K_index)
plot(b_b)

#Geomagnetic data 
#It has 5 categories so using a K value of 4 
b_c <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Max_K_index, k=4), data=Model_data, method = "REML",
           family=poisson())

summary(b_c)
plot(b_c)

#Almost identical to storm data 

#Modelling all variables - magnets, storms, year 
b_d <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Storms, k = 4) +s(Max_K_index, k=4), data=Model_data, method = "REML",
           family=poisson())

summary(b_d)
plot(b_d)

# get the AIC - the lower the better - this gives you an idea of whether extra parametres added 
# to a model are worth it for the added analysis/complexity, or whether the simpler model with fewer 
# parametres is better 

AIC(b_d)

par(mfrow=c(2,2))
# compare k' column to edf if close, double k
gam.check(b_d)


# residuals look wacky (quantile-quantile plot not matching red line)
# increasing variance in resids. vs. linear pred. plot
# try quasi poisson?
b_e <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Storms, k = 4) +s(Max_K_index, k=4), data=Model_data, method = "REML",
           family=quasipoisson())

summary(b_e)
plot(b_e)

par(mfrow=c(2,2))
gam.check(b_e)


# lets be fancy!
# Tweedie
# set a=1.2 because things get weird if less than 1.2
b_f <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Storms, k = 4) +s(Max_K_index, k=4), data=Model_data, method = "REML",
           family=tw(a=1.2))

summary(b_f)
plot(b_f)

par(mfrow=c(2,2))
gam.check(b_f)

# make predictions - useful if I have loads of missing data 
b_g <- gam(Richness ~ offset(log(Population)) + s(Year), data=Model_data, method = "REML",
           family=tw(a=1.2))

plot_data <- Model_data
predict_Richness <- predict(b_g, newdata=Model_data, type="response", se.fit=TRUE)
plot_data$predict_Richness <- predict_Richness$fit
plot_data$upper_Richness <- predict_Richness$fit + 2*predict_Richness$se.fit
plot_data$lower_Richness <- predict_Richness$fit - 2*predict_Richness$se.fit

plot(plot_data[,c("Year", "Richness")])
# a bit jagged
lines(plot_data[,c("Year", "predict_Richness")])
lines(plot_data[,c("Year", "upper_Richness")], lty=2)
lines(plot_data[,c("Year", "lower_Richness")], lty=2)
rug(plot_data$Year)

#Can't get this to work....
#Playing around with data that has missing values (here richness during and around the war years)
test_data <- read.csv("test_data.csv")

b_h <- gam(Richness ~ offset(log(Population)) + s(Year), data=Model_data, method = "REML",
           family=tw(a=1.2))

plot_data <- test_data
predict_Richness <- predict(b_t, newdata=test_data, type="response", se.fit=TRUE)
predict_Richness

plot_data$predict_Richness <- predict_Richness$fit
plot_data$upper_Richness <- predict_Richness$fit + 2*predict_Richness$se.fit
plot_data$lower_Richness <- predict_Richness$fit - 2*predict_Richness$se.fit

plot(plot_data[,c("Year", "Richness")])
# a bit jagged
lines(plot_data[,c("Year", "predict_Richness")])
lines(plot_data[,c("Year", "upper_Richness")], lty=2)
lines(plot_data[,c("Year", "lower_Richness")], lty=2)
#Shows you which years you have data for 
rug(plot_data$Year)


#Here this model is looking at Richness, offset by population and the smooths 
#are "Year" and "Organisations"

b_i <- gam(Richness ~ offset(log(Population)) + s(Year) +s(Organisations), data=Model_data, method = "REML",
           family=tw(a=1.2))

summary(b_i)
plot(b_i)

par(mfrow=c(2,2))
gam.check(b_i)

AIC(b_i)

#Shoving everything into the model 
b_j <- gam(Richness ~ offset(log(Population)) +s(Year) +s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations), data=Model_data, method = "REML",
           family=tw(a=1.2))

summary(b_j)
plot(b_j)

par(mfrow=c(2,2))
gam.check(b_j)

#This gives an AIC score 3 points higher - is the model complexity really worth it? 
#Model only explains .3 more of the variation 
AIC(b_i, b_j)
anova(b_i)
anova(b_j)

#SST - tweedie seems to work best 
unique(Model_data$Max_SST_temp)

b_k <- gam(Richness ~ offset(log(Population)) +s(Max_SST_temp), data=Model_data, method = "REML",
           family=tw(a=1.2))

summary(b_k)
plot(b_k)

par(mfrow=c(2,2))
gam.check(b_k)
AIC(b_k)


b_l <- gam(Richness ~ offset(log(Population)) +s(Year) +s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations) +s(Max_SST_temp), data=Model_data, method = "REML",
           family=tw(a=1.2))

summary(b_l)
plot(b_l)

par(mfrow=c(2,2))
gam.check(b_l)
AIC(b_l)



#Stranding events 
b_m <- gam(Stranding_count ~ offset(log(Population)) + s(Year) +s(Max_SST_temp), data=Model_data, method = "REML",
           family=tw(a=1.2))

summary(b_m)
plot(b_m)

par(mfrow=c(2,2))
gam.check(b_m)
AIC(b_m)


#+s(Year) +s(Storms, k = 4) +s(Max_K_index, k=4) +s(Organisations) +s(Max_SST_temp)
#+s(Storms, k =4) +s(Max_K_index, k=4) +s(Organisations)



install.packages("broom")
library(broom)

#Tidy gives the neat model output - summarises the model statistical findings 
tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
glance(b_m)

#Note: You can't use 'augment' on a GAM 

#Tidy multiple models at once 
#Create a list of the models (use mget(paste0("b", 1"14))) or whatever to 'get' all models 
models <- list(b_m = b_m, b_l = b_l, b_i = b_i) 

#Saving the tidy and glance datasets 
all_coefs_tidy <- plyr::ldply(models, tidy, .id = "model")
all_coefs_glance <- plyr::ldply(models, glance, .id = "model")

sapply(models, class)


############################################################################################
#GAMs stranding events 
b_m <- gam(Stranding_count ~ offset(log(Population)) + s(Max_K_index, k=4), data=Model_data, method = "REML",
           family=tw(a=1.2))

# +s(Max_SST_temp),

summary(b_m)
plot(b_m)

par(mfrow=c(2,2))
gam.check(b_m)
AIC(b_m)
vis.gam(b_m)


vis.gam(b_m, n.grid = 50, theta = 35, phi = 32, zlab = "",
        ticktype = "detailed", color = "topo")
