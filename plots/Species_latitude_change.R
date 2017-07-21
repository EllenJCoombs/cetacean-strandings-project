#Want to make decadal plots using Natalie's code 
#With strandings per lat and long - to look at range change 

library(dplyr)
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(magrittr)

cleaneddata <- read.csv("cleandatesnames.csv")

species_lat <- dplyr::count(cleaneddata, Latitude, Name.Current.Sci, Year) 

#removing the 0.0000 lats and 
species_lat <- species_lat %>%
  filter(Latitude > 49.00000) %>%
  filter(Latitude < 62.00000)


#This bit is for playing around with specific species 
B_acutorostrata_lat <- species_lat %>% 
  filter(Name.Current.Sci == "Balaenoptera acutorostrata")

# Create a list for the start years of each decade
decades <- c(seq(from = 1913, to = 2003, by = 10), 2013)
# Make an empty list that you're going to put all your
# ggplot plots into
plot.list <- list()
# Loop through each of the starting years in "decades"
for(i in seq_along(decades)){
  # Define the start and end year of the decade
  start.year <- decades[i]
  end.year <- decades[i] + 9
  # Add a short if statement so that you don't go into the future!
  if(end.year > 2017){
    end.year <- 2017
  }
  
 # Use filter to select just the records for that decade
  one.decade <- filter(B_acutorostrata_lat, Year >= start.year & Year <= end.year)
  
  
  # Add the points to a plot 
  # Create a different plot for each decade with a different name
  # placed into map.list
  plot.list[[i]] <- 
    ggplot(one.decade, aes(x = Latitude)) +
    geom_histogram(binwidth = 0.1) + 
    labs(title = paste(start.year, "-", end.year))
    
} # end loop
# Plot all the plots in map.list
do.call(grid.arrange, plot.list)

plot.list

################
#All species 



