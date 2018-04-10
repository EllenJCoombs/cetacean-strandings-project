

#Species GAMs 
#This code runs the GAMs for each of the indiviual species 

#First, code to pick out each of the seperate species 

#Now we need to take each species from the all_strandings dataset 
#Creating a new data frame to play with
sep_species <- all_strandings

#Make new dataframe by splitting into seperate species (these are tibbles)
lDf <- split(sep_species, sep_species$Species)

#Example of calling up sei whale data 
lDf$`Balaenoptera borealis`

#Splitting the tibble into data frames for each species 
Y <- lapply(seq_along(lDf), function(x) as.data.frame(lDf[[x]])[, 1:8]) 

BA <- Y[[1]] #minke
BB <- Y[[2]] #sei 
BM <- Y[[3]] #blue
BP <- Y[[4]] #fin
E <- Y[[5]]




#Running code for each of the seperate species 
#There are 21 species, it might be easiest to just plug each of the species dataframes (above) into the 
#following code 

