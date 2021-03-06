
#Running additional analysis
#This code is for running additional analysis on the correlates 
#of strandings, running the model for each species in turn. 
#Here the same correlates from the main analysis are used in 
#the generalised additive models.


library(dplyr)
library(mgcv) #for GAMs
library(broom) #for pulling all of the code together 

#Species GAMs 
#This code runs the GAMs for each of the indiviual species 
#The same correlates are used - see supplementary material 

#First, code to pick out each of the seperate species 
#Now we need to take each species from the all_strandings dataset 
#Creating a new data frame to play with
sep_species <- All_strandings

sep_species <- sep_species %>%
  dplyr::rename(Species = Name.Current.Sci)

#Make new dataframe by splitting into seperate species (these are tibbles)
lDf <- split(sep_species, sep_species$Species)

#Example of calling up sei whale data 
lDf$`Balaenoptera borealis`

#Splitting the tibble into data frames for each species 
#Be sure to capture the columns you want (1:10)
Y <- lapply(seq_along(lDf), function(x) as.data.frame(lDf[[x]])[, 1:10]) 

#new name <- assigned number (random) #common name/ID
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
MN <- Y[[12]] #humpback
MB <- Y[[13]] #Sowerby's
MM <- Y[[14]] #True's 
OO <- Y[[15]] #orca 
PP <- Y[[16]] #harbour 
PM <- Y[[17]] #sperm
PC <- Y[[18]] #false 
SC <- Y[[19]] #striped 
TT <- Y[[20]] #bottlenose
ZC <- Y[[21]] #Cuvier's

#Count the species per year as normal
speciesyearcount <- count(GM, Species, Year) %>%
  na.omit()

#Adding a 0 to each year without a record 
#Min is now 1990
speciesyearcount <- speciesyearcount %>% 
  complete(Year = seq(min(1913), max(2015), 1L))
#NAs -> 0 
speciesyearcount[is.na(speciesyearcount)] <- 0 
#NAs to species name
speciesyearcount[is.na(speciesyearcount)] <- "Globicephala melas"

#Changing the name of the dataset 
species_strandings <- speciesyearcount 
#Rename n to "Total_strandings"
species_strandings <- species_strandings %>% 
  dplyr::rename(Total_strandings = n)


Species_model <- full_join(species_strandings, All_model, by = "Year")

#Now the GAM's for each species 
#Use the above acronyms e.g., 'TT' - this needs to be done for all 21 species (above)
#Doing this one at a time - but I'm sure there is a way o do this and then use broom....

Species_GAM <- gam(Total_strandings ~ offset(log(Population)) +
                     s(Year, bs="ts") +
                     s(Storms, k=5, bs="ts") +
                     s(Max_K_index, k=4, bs="ts") +
                     s(Max_SST, bs="ts") +
                     s(NAO_index, bs="ts") + 
                     s(Fish_catch, bs="ts"),
                   data= Species_model, 
                   method= "REML",
                   family=nb())


summary(Species_GAM)
par(mfrow = c(2,2))
plot(Species_GAM) 

#Gam.check
par(mfrow=c(2,2))
gam.check(Species_GAM)


#Using broom to tidy up all of the results together 
#paackage 'broom'
#Tidy multiple models at once 
Mysticete_GAMs <- list(BA_GAM = BA_GAM, BB_GAM = BB_GAM, BM_GAM = BM_GAM, BP_GAM = BP_GAM,
                       MN_GAM = MN_GAM) 

#Tidy and glance datasets 
Mysticete_tidy <- plyr::ldply(Mysticete_GAMs, tidy, .id = "model")
Mysticete_glance <- plyr::ldply(Mysticete_GAMs, glance, .id = "model")

#Save to csv if required 
#write.csv(Mysticete_tidy, file = "Mysticete_tidy.csv")
#write.csv(Mysticete_glance, file = "Mysticete_glance.csv")


Odontocete_GAMs <- list(DD_GAM = DD_GAM, GM_GAM = GM_GAM, GG_GAM = GG_GAM, HA_GAM = HA_GAM,
                        KB_GAM = KB_GAM, LA_GAM = LA_GAM, LAl_GAM = LAl_GAM, MB_GAM = MB_GAM, 
                        MM_GAN = MM_GAM, OO_GAM = OO_GAM, PP_GAM = PP_GAM, PM_GAM = PM_GAM, 
                        PC_GAM = PC_GAM, SC_GAM = SC_GAM, TT_GAM = TT_GAM, ZC_GAM = ZC_GAM) 


#Tidy and glance datasets 
Odontocete_tidy <- plyr::ldply(Odontocete_GAMs, tidy, .id = "model")
Odontocete_glance <- plyr::ldply(Odontocete_GAMs, glance, .id = "model")

#Save to csv if required 
#write.csv(Odontocete_tidy, file = "Odontocete_tidy.csv")
#write.csv(Odontocete_glance, file = "Odontocete_glance.csv")
library(dplyr)
library(mgcv) #for GAMs
library(broom) #for pulling all of the code together 

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
MN <- Y[[12]] #humpback
MB <- Y[[13]] #Sowerby's
MM <- Y[[14]] #True's 
OO <- Y[[15]] #orca 
PP <- Y[[16]] #harbour 
PM <- Y[[17]] #sperm
PC <- Y[[18]] #false 
SC <- Y[[19]] #striped 
TT <- Y[[20]] #bottlenose
ZC <- Y[[21]] #Cuvier's

#Count the species 
GM <- count(Species, Year) %>%
  na.omit()

GM <- GM %>% 
  complete(Year = seq(min(1913), max(2015), 1L), Code = unique(sitecount$Code))
#NAs -> 0 
GM[is.na(GM)] <- 0


#Now the GAM's for each species 
#Use the above acronyms e.g., 'TT' 
#Doing this one at a time - but I'm sure there is a way o do this and then use broom....

MN_GAM <- gam(Total_strandings ~ offset(log(Population)) +
                        s(Year, bs="ts") +
                        s(Storms, k=5, bs="ts") +
                        s(Max_K_index, k=4, bs="ts") +
                        s(Max_SST, bs="ts") +
                        s(NAO_index, bs="ts") + 
                        s(Fish_catch, bs = "ts")
                      data= MN, 
                      method= "REML",
                      family=nb)


summary(MN_GAM)
par(mfrow = c(2,2))
plot(MN_GAM) 

#Gam.check
par(mfrow=c(2,2))
gam.check(MN_GAM)


#Using broom to tidy up all of the results together 
#paackage 'broom'
#Tidy multiple models at once 
Mysticete_GAMs <- list(BA_GAM = BA_GAM, BB_GAM = BB_GAM, BM_GAM = BM_GAM, BP_GAM = BP_GAM,
                        MN_GAM = MN_GAM) 

#Tidy and glance datasets 
Mysticete_tidy <- plyr::ldply(Mysticete_GAMs, tidy, .id = "model")
Mysticete_glance <- plyr::ldply(Mysticete_GAMs, glance, .id = "model")

#Save to csv if required 
#write.csv(Mysticete_tidy, file = "Mysticete_tidy.csv")
#write.csv(Mysticete_glance, file = "Mysticete_glance.csv")


Odontocete_GAMs <- list(DD_GAM = DD_GAM, GM_GAM = GM_GAM, GG_GAM = GG_GAM, HA_GAM = HA_GAM,
                       KB_GAM = KB_GAM, LA_GAM = LA_GAM, LAl_GAM = LAl_GAM, MB_GAM = MB_GAM, 
                       MM_GAN = MM_GAM, OO_GAM = OO_GAM, PP_GAM = PP_GAM, PM_GAM = PM_GAM, 
                       PC_GAM = PC_GAM, SC_GAM = SC_GAM, TT_GAM = TT_GAM, ZC_GAM = ZC_GAM) 


#Tidy and glance datasets 
Odontocete_tidy <- plyr::ldply(Odontocete_GAMs, tidy, .id = "model")
Odontocete_glance <- plyr::ldply(Odontocete_GAMs, glance, .id = "model")

#Save to csv if required 
#write.csv(Odontocete_tidy, file = "Odontocete_tidy.csv")
#write.csv(Odontocete_glance, file = "Odontocete_glance.csv")
