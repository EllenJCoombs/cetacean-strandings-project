

#Species year count (just done 'speciesyearcount')

library(dplyr)
library(mgcv)


Model_data <- read.csv("Model_data.csv")

#Remove unknowns for species year count 
Strandings_known_IRL_UK <- UK_and_Irish %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))


#'Speciesyearcount' cleaned data: a count of current scientific name and year 
speciesyearcount <- dplyr::count(Strandings_known_IRL_UK, Name.Current.Sci, Year) %>%
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


#GAM for the above with Species as the factor smooth 
All_strand <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                s(Storms, k=5) +
                s(Max_K_index, k=4) +
                s(Max_SST) +
                s(NAO_index), 
              data= all_strandings, method = "REML",
              family=tw(a=1.2))

summary(All_strand)
plot(All_strand)


par(mfrow=c(2,2))
gam.check(All_strand)


save(All_strand, all_strandings, file = "Model_for_Dave.Rdata")
