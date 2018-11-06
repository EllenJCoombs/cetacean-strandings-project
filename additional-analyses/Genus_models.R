
#Genus instead of species models 
#Need to split species column 

#For whole datset 
#Then for 1990s onwards 

library(dplyr)
#Seperate
UK_and_Irish_genus <- UK_and_Irish_sp %>% 
  separate(Name.Current.Sci, c('Species', 'Genus'), sep=" ")





install.packages('reshape')
library(reshape)
df = transform(UK_and_Irish_sp, Name.Current.Sci = colsplit(Name.Current.Sci, split = "\\|", names = c('Species', 'Genus')))