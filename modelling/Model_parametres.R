#Bringing all the model parameters together 

library(dplyr)
library(tidyr)

#1 - Storm data 

storms <- read.csv("Storm_data.csv")
#Select data 
storms <- storms %>% 
  select(Year, Count)

#Counting up and keeping 0 as 0 
storms <- storms %>% 
  complete(Year, fill = list(Count = 0)) %>% 
  group_by(Year) %>% 
  summarise(count = sum(Count))

###################################################################################################
#Population data 
#This is a data file that is combined with yearly strandings 

