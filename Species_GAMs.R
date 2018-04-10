

#Species GAMs 
#This code runs the GAMs for each of the indiviual species 

#First, code to pick out each of the seperate species 



#Now we need to take each species, give it a count per year (including 0s for years it wasn't 
#recorded) and add it to the model data 

#Make new dataframe by splitting into seperate species (these are tibbles)
lDf <- split(sep_species, sep_species$Species)

#Example of calling up sei whale data 
lDf$`Balaenoptera borealis`

#Splitting the tibble into data frames for each species 
Y <- lapply(seq_along(lDf), function(x) as.data.frame(lDf[[x]])[, 2:8]) 

bacutorostrata <- Y[[1]]
B <- Y[[2]]
C <- Y[[3]]
D <- Y[[4]]
E <- Y[[5]]




#Running code for each of the seperate species 
#There are 21 species, it might be easiest to just plug each of the species dataframes (above) into the 
#following code 

