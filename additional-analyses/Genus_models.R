
#Genus instead of species models 
#Need to split species column 

#For whole datset 
#Then for 1990s onwards 

library(dplyr)
#Seperate
UK_and_Irish_genus <- UK_and_Irish_sp %>% 
  separate(Name.Current.Sci, c('Genus', 'Species'), sep=" ")

#Remove species column 
UK_and_Irish_genus$X.1 <- NULL
UK_and_Irish_genus$X <- NULL 
UK_and_Irish_genus$Species <- NULL 