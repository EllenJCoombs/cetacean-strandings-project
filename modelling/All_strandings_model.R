

#Species year count (just done 'speciesyearcount')

library(dplyr)
library(mgcv)

#Read in model data for adding to the GAM later on this script 
Model_data <- read.csv("Model_data.csv")
#Read in the raw data 
#Or load "Uk_and_Irish_sp.csv" (just wanted to make sure this was all right)
UK_and_Irish <- read.csv("UK_and_Irish_strandings.csv")

#Remove unknowns for species year count 
UK_and_Irish_known <- UK_and_Irish %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))

#Removing species with only one or two records 
#This has been done earlier for richness, but 
UK_and_Irish_sp <- UK_and_Irish_known %>%
  filter(!(Name.Current.Sci %in% c("Monodon monoceros", "Peponocephala electra", "Delphinapterus leucas", "Kogia sima",
                                   "Mesoplodon densirostris", "Mesoplodon europaeus", "Lagenodelphis hosei")))


#'Speciesyearcount' cleaned data: a count of current scientific name and year 
speciesyearcount <- dplyr::count(UK_and_Irish_sp, Name.Current.Sci, Year) %>%
  na.omit()

#This is making sure unknowns aren't factors in the data 
speciesyearcount$Name.Current.Sci <- droplevels(speciesyearcount$Name.Current.Sci)

#Adding a 0 to each year without a record 
speciesyearcount <- speciesyearcount %>% 
  complete(Year = seq(min(1913), max(2015), 1L), Name.Current.Sci = unique(speciesyearcount$Name.Current.Sci))

#NAs -> 0 
speciesyearcount[is.na(speciesyearcount)] <- 0

#join the two datasets
all_strandings <- full_join(speciesyearcount, Model_data, by = "Year")
all_strandings$X <- NULL

#Rename 
all_strandings <- all_strandings %>% 
  rename(Total_strandings = n)

all_strandings <- all_strandings %>%
  rename(Species = Name.Current.Sci)

#Adding factor smooth 
#Adding the factor smooth (there is a much better way of doing this...)
#All strandings BIG 

#Big 
bphysalus <- filter(all_strandings, Species ==  "Balaenoptera physalus") 
bborealis <- filter(all_strandings, Species ==  "Balaenoptera borealis")
bmusculus <- filter(all_strandings, Species ==  "Balaenoptera musculus") 
mnovaeangliae <- filter(all_strandings, Species == "Megaptera novaeangliae") 
pmacrocephalus <- all_strandings %>%
  filter(Species %in% c("Physeter macrocephalus", "Physeter macrocephalus "))  


Big_bs <- rbind(bphysalus, bborealis, bmusculus, mnovaeangliae, pmacrocephalus)
Big_bs$X.1 <- NULL
Big_bs$X <- NULL

#Medium body size 
oorca <- all_strandings %>% 
  filter(Species %in% c("Orcinus orca", "Orcinus orca "))
hampullatus <- filter(all_strandings, Species ==  "Hyperoodon ampullatus")
bacutorostrata <- filter(all_strandings, Species ==  "Balaenoptera acutorostrata") 

Medium_bs <- rbind(oorca, hampullatus, bacutorostrata)
Medium_bs$X.1 <- NULL
Medium_bs$X <- NULL


#Small - need to remove all of the uknowns as well 
Small_bs_clean <- all_strandings[ !(all_strandings$Species %in% Big_bs$Species), ] 
Small_bs_clean2 <- Small_bs_clean[ !(Small_bs_clean$Species %in% Medium_bs$Species), ] 

#No unknowns 
Small_bs <- Small_bs_clean2

Small_bs$X.1 <- NULL
Small_bs$X <- NULL 


#Adding a new column for bodysize to each 
#Adding new columns to the above 
Big_bs["Body_size"] <- "Big"
Medium_bs["Body_size"] <- "Medium"
Small_bs["Body_size"] <- "Small"

#Combine the three datasets 
#This now has indivisual strandings, stranding richness and all the covariates as well as 
#bodysize 

all_strandings <- bind_rows(Big_bs, Medium_bs, Small_bs)
#Turn body size into a factor - keep getting error messages as it was a character before 
all_strandings$Body_size <- as.factor(all_strandings$Body_size)

#save as all_strandings 

write.csv(all_strandings, file = "all_strandings.csv")


#GAM for the above with Species as the factor smooth - added factor smooths in later models 
All_strandc <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                s(Storms, k=5, bs="ts") +
                s(Max_K_index, k=4, bs= "ts") +
                s(Max_SST, bs= "ts") +
                s(NAO_index, bs="ts"), 
              data= all_strandings, method = "REML",
              family=nb())


summary(All_strandc)
par(mfrow = c(2,2))
plot(All_strandc)


#Gam.check
par(mfrow=c(2,2))
gam.check(All_strandc)


library(broom)
#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM 

#Tidy multiple models at once 
Tidy1_9 <- list(All_strand1 = All_strand1, All_strand2 = All_strand2, All_strand3 = All_strand3, All_strand4 = All_strand4,
                All_strand5 = All_strand5, All_strand6 = All_strand6, All_strand7 = All_strand7, All_strand8 = All_strand8, 
                All_strand9 = All_strand9) 

#Tidy multiple models at once 
#This is for 1,6,7,8,9 (no backwards selection)
Tidy1567 <- list(All_strand1 = All_strand1, All_strand6 = All_strand6, All_strand7 = All_strand7, All_strand8 = All_strand8,
                All_strand9 = All_strand9) 

#Saving the tidy and glance datasets 
All_coefs_tidy1_9 <- plyr::ldply(Tidy1_9, tidy, .id = "model")
All_coefs_glance1_9 <- plyr::ldply(Tidy1_9, glance, .id = "model")

write.csv(All_coefs_tidy1567, file = "All_tidy1567.csv")
write.csv(All_coefs_glance1567, file = "All_glance1567.csv")


par(mfrow=c(1,1))
fitted_A <- fitted(All_strand1)
response_A <-  All_strand1$y
plot(fitted_A[response_A<50], response_A[response_A<50], pch=19, cex=0.2, asp=1)
abline(a=0,b=1)

#Using identify
identify(fitted_A[response_A<50], response_A[response_A<50])
#What are the specific points? 
all_strandings[response_A<50,][2202,]#Lagenorhynchus acutus   
all_strandings[response_A<50,][995,]#Lagenorhynchus acutus
all_strandings[response_A<50,][1025,]#Lagenorhynchus acutus
all_strandings[response_A<50,][1069,]#Lagenorhynchus acutus 
all_strandings[response_A<50,][1094,]#Delphinus delphis
all_strandings[response_A<50,][1408,]#Globicephala melas 
all_strandings[response_A<50,][1423,]#Globicephala melas                
all_strandings[response_A<50,][1894,]#Phocoena phocoena 
all_strandings[response_A<50,][1900,]#Globicephala melas
all_strandings[response_A<50,][1954,]#Phocoena phocoena 
all_strandings[response_A<50,][2001,]#Globicephala melas 
all_strandings[response_A<50,][2040,]#Delphinus delphis  
all_strandings[response_A<50,][2054,]#Delphinus delphis
all_strandings[response_A<50,][2202,]#Lagenorhynchus acutus 
all_strandings[response_A<50,][1364,]#Globicephala melas
all_strandings[response_A<50,][1393,]#Globicephala melas
all_strandings[response_A<50,][1885,]#Globicephala melas 
all_strandings[response_A<50,][2211,]#Globicephala melas 
all_strandings[response_A<50,][2224,]#Globicephala melas 

#Highlighting phocoena as possible outliers 
plot(fitted_A, response_A, pch=19, cex=0.2, asp=1)
points(fitted_A[all_strandings$Species=="Phocoena phocoena"], 
      response_A[all_strandings$Species=="Phocoena phocoena"], pch=19, cex=0.5, col="red")
abline(a=0,b=1)


#Melas
plot(fitted_A, response_A, pch=19, cex=0.2, asp=1)
points(fitted_A[all_strandings$Species=="Globicephala melas"], 
       response_A[all_strandings$Species=="Globicephala melas"], pch=19, cex=0.5, col="green")
abline(a=0,b=1)

#Lagenorynchus acutus 
plot(fitted_A, response_A, pch=19, cex=0.2, asp=1)
points(fitted_A[all_strandings$Species=="Lagenorhynchus acutus"], 
       response_A[all_strandings$Species=="Lagenorhynchus acutus"], pch=19, cex=0.5, col="green")
abline(a=0,b=1)

#Delphius 
plot(fitted_A, response_A, pch=19, cex=0.2, asp=1)
points(fitted_A[all_strandings$Species=="Delphinus delphis"], 
       response_A[all_strandings$Species=="Delphinus delphis"], pch=19, cex=0.5, col="green")
abline(a=0,b=1)

#Removing Harbour porpoise from the dataset 
No_phocoena <- all_strandings %>%
  filter(Species != "Phocoena phocoena")


No_phocoena_a9 <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                     s(Storms, k=5, bs="ts") +
                     s(Max_K_index, k=4, bs="ts") +
                     s(Max_SST, Species, bs="fs") +
                     s(NAO_index, bs="ts"), 
                   data= No_phocoena, 
                   method= "REML",
                   family=poisson())

summary(No_phocoena_a9)
par(mfrow = c(2,2))
plot(No_phocoena_a9)

#Gam.check
par(mfrow=c(2,2))
gam.check(No_phocoena_a9)


#Tidy multiple models at once 
Tidy_no_phocoena <- list(No_phocoena1 = No_phocoena1, No_phocoena2 = No_phocoena2, No_phocoena3 = No_phocoena3, No_phocoena4 = No_phocoena4,
                         No_phocoena5 = No_phocoena5) 

#Saving the tidy and glance datasets 
All_coefs_No_phocoena <- plyr::ldply(Tidy_no_phocoena, tidy, .id = "model")
All_coefs_glance_No_phocoena <- plyr::ldply(Tidy_no_phocoena, glance, .id = "model")

write.csv(All_coefs_No_phocoena, file = "All_tidy_no_phocoena.csv")
write.csv(All_coefs_glance_No_phocoena, file = "All_glance_no_phocoena.csv")


#Tidy and AICs for different model fits 
#All_strand1 = Tweedie 
#All_stranda = Poisson 
#All_strandb = Quassipoisson 
#All_strandc = Negative binomial 

Tidy_abc <- list(All_strand1 = All_strand1, All_stranda = All_stranda, All_strandb = All_strandb, All_strandc = All_strandc)

#Saving the tidy and glance datasets 
All_coefs_tidy_abc <- plyr::ldply(Tidy_abc, tidy, .id = "model")
All_coefs_glance_tidyabc <- plyr::ldply(Tidy_abc, glance, .id = "model")

write.csv(All_coefs_tidy_abc, file = "All_tidy_abc.csv")
write.csv(All_coefs_glance_tidyabc, file = "All_glance_abc.csv")

#Tidy for poisson models 
Tidy_poisson <- list(All_stranda1 = All_stranda1, All_stranda6 = All_stranda6, All_stranda7 = All_stranda7, All_stranda8 = All_stranda8,
                     All_stranda9 = All_stranda9)

All_coefs_tidy_poisson <- plyr::ldply(Tidy_poisson, tidy, .id = "model")
All_coefs_glance_poisson <- plyr::ldply(Tidy_poisson, glance, .id = "model")


#Tidy for poisson models with no phocoena 










save(All_strand, all_strandings, file = "Model_for_Dave.Rdata")
