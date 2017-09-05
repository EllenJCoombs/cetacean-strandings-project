#Bringing all the model parameters together 

library(dplyr)

#1 - Storm data 

storms <- read.csv("Storm_data.csv")
#Select data 
storms <- storms %>% 
  select(Year, Count)

#Sum counts and rename 
storms <- (count(storms, Year))
storms <- storms %>%
  dplyr::rename(Count = n)
