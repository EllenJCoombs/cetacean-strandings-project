

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
                                   "Mesoplodon densirostris")))


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
#sapply(all_strandings, class)

#MODEL 1: No additional smooths (just Year, Species as standard)
#GAM for the above with Species as the factor smooth 
All_strand9 <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                s(Storms, k=5) +
                s(Max_K_index, k=4) +
                s(Max_SST) +
                s(NAO_index, bs="ts"), 
              data= all_strandings, method = "REML",
              family=tw(a=1.2))

summary(All_strand9)
par(mfrow = c(2,2))
plot(All_strand9)

#Gam.check
par(mfrow=c(2,2))
gam.check(All_strand9)


library(broom)
#Broom to tidy model outputs 

#Tidy gives the neat model output - summarises the model statistical findings 
#e.g. tidy(b_m)
#construct a concise one-row summary of the model. This typically contains values such as R^2, 
#adjusted R^2, and residual standard error that are computed once for the entire model.
#e.g. glance(b_m)

#Note: You can't use 'augment' on a GAM 

#Tidy multiple models at once 
All_tidy <- list(All_ra = All_ra, All_rb = All_rb, All_rc = All_rc, All_rd = All_rd,
                 All_ea = All_ea, All_eb = All_eb, All_ec = All_ec, All_ed = All_ed) 

#Saving the tidy and glance datasets 
All_coefs_tidy <- plyr::ldply(All_tidy, tidy, .id = "model")
All_coefs_glance <- plyr::ldply(All_tidy, glance, .id = "model")

write.csv(All_coefs_tidy, file = "All_tidy.csv")
write.csv(All_coefs_glance, file = "All_glance.csv")







par(mfrow=c(1,1))
fitted_A <- fitted(All_strand1)
response_A <-  All_strand1$y
plot(fitted_A[response_A<50], response_A[response_A<50], pch=19, cex=0.2, asp=1)
abline(a=0,b=1)

#Using identify
identify(fitted_A[response_A<50], response_A[response_A<50])
#What are the specific points? 
all_strandings[response_A<50,][377,]



plot(fitted_A, response_A, pch=19, cex=0.2, asp=1)
points(fitted_A[all_strandings$Species=="Phocoena phocoena"], 
      response_A[all_strandings$Species=="Phocoena phocoena"], pch=19, cex=0.5, col="red")


#Removing Harbour porpoise from the dataset 
No_porpoise <- all_strandings %>%
  filter(Species != "Phocoena phocoena")



save(All_strand, all_strandings, file = "Model_for_Dave.Rdata")
