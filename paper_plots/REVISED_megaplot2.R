
#This code is for the maps only part of the original megaplot.
#Ellen still to do: make maps bigger, and closer together
library(ggplot2)
library(viridis)
library(dplyr)
library(maps)
library(hexbin)
library(ggmap)
library(gridExtra)
library(Hmisc) 

#Extract UK map
#Added Ireland
# Note that package purrr masks the function map so detach this, or don't load it, or
# the tidyverse bundle, in the first place. 
uk <- map_data("world", regions = c('UK', 'Ireland', 'Guernsey', 'Jersey', 'Isle of Man'))

#Create base map
gg1 <-
  ggplot() +
  geom_polygon(data = uk, aes(x = long, y = lat, group = group),
               fill = "white", color = "grey70") +
  coord_fixed(1.3)

#Read in the strandings data - this isn't needed here - read in the allparv.RData
#This is the cleaned data with unknowns and rare species removed
#ds <- read.csv("UK_and_Irish_sp.csv")

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


  #put the maps together 
# To fix the spacing use plot.margin. Doing it here means you can have different
# margins for each on if needed.
# Top, right, bottom, left is the order
# You may wish to modify. 
  gr <- grid.arrange(make_data(ds, years=c(1913, 1925)) +
                     theme(plot.margin=unit(c(5.5, 5.5, 5.5, -10), "points"),
                           strip.text=element_text(colour="black")), # For y labels
                     make_data(ds, years=c(1926, 1950)) +
                     theme(plot.margin=unit(c(5.5, 5.5, 5.5, -10), "points")),
                     make_data(ds, years=c(1951, 1975)) +
                     theme(plot.margin=unit(c(5.5, 5.5, 5.5, -10), "points")),
                     make_data(ds, years=c(1976, 2000)) +
                     theme(plot.margin=unit(c(5.5, 5.5, 5.5, -10), "points")),
                     make_data(ds, years=c(2001, 2016)) +
                     theme(plot.margin=unit(c(5.5, 5.5, 5.5, -10), "points")),
                     ncol = 5) 
  
# Save to pdf - may want to change size/location/name
  ggsave(gr, file = "megaplot_maps.pdf", height = 5) 
  