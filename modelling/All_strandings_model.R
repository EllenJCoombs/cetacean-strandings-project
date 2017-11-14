

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
