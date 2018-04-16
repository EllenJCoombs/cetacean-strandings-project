

#Habitat GAMs 
#Running GAMs with information on whether species are pelagic, coastal or both 
#Information was taken from Reid et al., 2003 

#Pelagic = most time spent in waters >200m 
#Coastal = most time spent in waters  ≤ 200m 
#Both - where it was stated the species spent time in both habitats equally, or where it was unclear

#Read in the data from the cleaned data folder 
all_strandings <- read.csv("all_strandings.csv")

#Split the data as in the Suborder GAMs code 
#Pelagic, coastal, both

