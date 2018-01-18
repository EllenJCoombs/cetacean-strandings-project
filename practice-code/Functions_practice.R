
#Functions and foreloops 
library(dplyr)

#Getting something from a subfolder: 
ds<-read.csv("data/mydata.csv")

#Leading to your own functions 
source("functions/myfunctions1.R")
source("functions/myfunctions2.R")

#etc etc 
#Loops 
#Fake data with species snout vent length (SVL) and body mass
mydata <- data.frame(
  species = c(rep(LETTERS[1:3], 4)),
  SVL = abs(rnorm(12)),
  mass = sample(c(10, 15, 50), 12, replace = TRUE)
)

mydata

#for i (so "hello world"), run 10 times
for(i in 1:10){
  print(i)
  print(paste("Hello world", i))
}


#Creating an empty dataframe to put outputs in (from loop below)
myoutput <- array(dim = c(length(unique(mydata$species)), 3)) #Because we don't know how many species
                          #there are 
myoutput <-data.frame(myoutput) #outputting to a data frame 
names(myoutput) <- c("Spp", "meanSVL", "meanMass") #Changing the column names 

#loop for the number of unique species in the data set 
#Finding the mean of each species 
for(i in 1:length(unique(mydata$species))){  
  mydata2 <- filter(mydata, species == unique(mydata$species)[i]) 
  myoutput$Spp[i] <- unique(mydata$species)[i]
  myoutput$meanSVL[i] <- mean(mydata2$SVL)
  myoutput$meanMass[i] <- mean(mydata2$mass)
}

myoutput

#Functions 

#The basic add_numbers function 
add_numbers <- function(number1, number2 =5){
  number1 + number2
}

add_numbers <- function(number1, number2){
  mysum <- number1 + number2 
  x <- number1 * 2 
  return(list(mysum, x)) #returns a list when you've asked it to do multiple things 
}

add_numbers(7,7)

#New function 
mult <- function(number1, number2, number3){ #add the number part to tell the function where it's
  #getting data from
  add_numbers(number1, number2) * number3
}

mult(2,3,4) 

















