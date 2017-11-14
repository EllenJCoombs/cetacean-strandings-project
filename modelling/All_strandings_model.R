

#Species year count (just done 'speciesyearcount')

library(dplyr)
library(mgcv)

#Read in model data for adding to the GAM later on this script 
Model_data <- read.csv("Model_data.csv")

#Remove unknowns for species year count 
UK_and_Irish_known <- UK_and_Irish %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))

#'Speciesyearcount' cleaned data: a count of current scientific name and year 
speciesyearcount <- dplyr::count(UK_and_Irish_known, Name.Current.Sci, Year) %>%
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


#Removing Harbour porpoise from the dataset 
No_porpoise <- all_strandings %>%
  filter(Species != "Phocoena phocoena")

#GAM for the above with Species as the factor smooth 
All_strand <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                s(Storms, k=5, bs="ts") +
                s(Max_K_index, k=4, bs="ts") +
                s(Max_SST, bs="ts") +
                s(NAO_index, bs="ts"), 
              data= No_porpoise, method = "REML",
              family=tw(a=1.2))

summary(All_strand)
plot(All_strand)


par(mfrow=c(2,2))
gam.check(All_strand)


par(mfrow=c(1,1))
fitted_A <- fitted(All_strand)
response_A <-  All_strand$y
plot(fitted_A[response_A<50], response_A[response_A<50], pch=19, cex=0.2, asp=1)
abline(a=0,b=1)

plot(fitted_A, response_A, pch=19, cex=0.2, asp=1)
#points(fitted_A[all_strandings$Species=="Phocoena phocoena"], 
#       response_A[all_strandings$Species=="Phocoena phocoena"], pch=19, cex=0.5, col="red")


save(All_strand, all_strandings, file = "Model_for_Dave.Rdata")
