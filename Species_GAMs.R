
library(dplyr)
library(mgcv)

#Species GAMs 
#This code runs the GAMs for each of the indiviual species 

#First, code to pick out each of the seperate species 

#Now we need to take each species from the all_strandings dataset 
#Creating a new data frame to play with
sep_species <- all_strandings

#Make new dataframe by splitting into seperate species (these are tibbles)
lDf <- split(sep_species, sep_species$Species)

#Example of calling up sei whale data 
lDf$`Balaenoptera borealis`

#Splitting the tibble into data frames for each species 
Y <- lapply(seq_along(lDf), function(x) as.data.frame(lDf[[x]])[, 1:8]) 

BA <- Y[[1]] #minke
BB <- Y[[2]] #sei 
BM <- Y[[3]] #blue
BP <- Y[[4]] #fin
DD <- Y[[5]] #common
GM <- Y[[6]] #globicephala
GG <- Y[[7]] #griseus
HA <- Y[[8]] #northern 
KB <- Y[[9]] #pygmy
LA <- Y[[10]] #white sided 
LAl <- Y[[11]] #white beaked 
MA <- Y[[12]] #humpback
MB <- Y[[13]] #Sowerby's
MM <- Y[[14]] #True's 
OO <- Y[[15]] #orca 
PP <- Y[[16]] #harbour 
PM <- Y[[17]] #sperm
PC <- Y[[18]] #false 
SC <- Y[[19]] #striped 
TT <- Y[[20]] #bottlenose
ZC <- Y[[21]] #Cuvier's


#Now the GAM's for each species 
#Doing this one at a time - but I'm sure there is a way o do this and then use broom....

BA_GAM <- gam(Total_strandings ~ offset(log(Population)) +
                        s(Year, bs="ts") +
                        s(Storms, k=5, bs="ts") +
                        s(Max_K_index, k=4, bs="ts") +
                        s(Max_SST, bs="ts") +
                        s(NAO_index, bs="ts"), 
                      data= BA, 
                      method= "REML",
                      family=nb)


summary(BA_GAM)
par(mfrow = c(2,2))
plot(BA_GAM)

#Gam.check
par(mfrow=c(2,2))
gam.check(BA_GAM)




#Running code for each of the seperate species 
#There are 21 species, it might be easiest to just plug each of the species dataframes (above) into the 
#following code 

