

library(ggplot2)
library(viridis)
library(dplyr)
library(maps)
library(hexbin)
library(ggmap)
library(gridExtra)
library(tidyverse)
library(Hmisc) #Not sure why ths had to be run on later versions 

#Extract UK map
#Added Ireland
uk <- map_data("world", regions = c('UK', 'Ireland', 'Guernsey', 'Jersey', 'Isle of Man'))

#Create base map
gg1 <-
  ggplot() +
  geom_polygon(data = uk, aes(x = long, y = lat, group = group),
               fill = "white", color = "grey70") +
  coord_fixed(1.3)

#Read in the strandings data
#This is the cleaned data with unknowns and rare species removed
ds <- read.csv("UK_and_Irish_sp.csv")

#This dataset comes from "make_megadata.R"
load("allparv.RData")

ds <- allparv

# capitalize the labels
ds$whatareyou <- Hmisc::capitalize(ds$whatareyou)


#function to build the hexmaps
make_data <- function(ds, years, labs=FALSE){
  # Remove NAs from coordinates
  # And restrict to things in UK waters
  #Filter species here too·
  ds <- ds %>%
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    filter(Latitude < 62 & Latitude > 45) %>%
    filter(Longitude < 3 & Latitude > -11)
  
  
  ds <- ds %>%
    filter(Year>=years[1] & Year<years[2])
  
  #Basic plot using viridis colour scheme
  #Note that you can change bins and transparency
  
  pp <- gg1+
    geom_hex(data = ds, aes(y = Latitude, x= Longitude), bins = 50, alpha = 0.75) +
    theme_minimal() +
    facet_wrap(~whatareyou, ncol=1, strip.position="left") +
    labs(x="", y="") +
    scale_fill_viridis(trans = "log", na.value="white",
                       limits = c(1, 501),
                       breaks = c(1, 10, 20, 50, 100, 250, 500),
                       labels = c(1, 10, 20, 50, 100, 250, 500)) +
    ggtitle(paste0(years[1], "-", years[2]))

  
  if(labs){
    pp <- pp +
      theme(axis.text=element_blank(),
            strip.text=element_text(),
            plot.title=element_text(hjust=0.5, size=8))

    
    
  }else{
    pp <- pp +
      theme(legend.position="none",
            strip.text=element_text(colour="white"),
            axis.text=element_blank(),
            plot.title=element_text(hjust=0.5, size=8))
  }
  
  return(pp)
}


  #put it all together
  gr <- grid.arrange(pp, 
                     make_data(ds, years=c(1913, 1925)),
                     make_data(ds, years=c(1926, 1950)),
                     make_data(ds, years=c(1951, 1975)),
                     make_data(ds, years=c(1976, 2000)),
                     make_data(ds, years=c(2001, 2016)),
                     layout_matrix = matrix(c(1,2,1,3,1,4,1,5,1,6), 2, 5)) 
                  
                    
