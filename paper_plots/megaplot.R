# here is the code for the megaplot

#Ellen - have changed the code for a slightly different dataset!

library(ggplot2)
library(viridis)
library(dplyr)
library(maps)
library(hexbin)
library(ggmap)
library(gridExtra)
library(tidyverse)

# Extract UK map
#Added Ireland·
uk <- map_data("world", regions = c('UK', 'Ireland', 'Guernsey', 'Jersey', 'Isle of Man'))

# Create base map
gg1 <-
  ggplot() +
  geom_polygon(data = uk, aes(x = long, y = lat, group = group),
               fill = "white", color = "grey70") +
  coord_fixed(1.3)

# Read in the strandings data
#This is the cleaned data with unknowns and rare species removed
ds <- read.csv("UK_and_Irish_sp.csv")

#This dataset comes from "make_megadata.R"
load("allparv.RData")

ds <- allparv

# capitalize the labels
ds$whatareyou <- Hmisc::capitalize(ds$whatareyou)


# function to build the hexmaps
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

  # Basic plot using viridis colour scheme
  # Note that you can change bins and transparency
  
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
      theme(legend.position= c(13.2,1.25),
            axis.text=element_blank(),
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

## make the time series line

# get the data
linesdat <- ds %>%
  group_by(Year, whatareyou) %>%
  summarize(total=n())


# make the timeseries plot
plines <- ggplot(linesdat) +
  scale_x_continuous(labels=seq(1900, 2025, by=25),
                     breaks=seq(1900, 2025, by=25),
                     expand=c(0, 0)) +
  geom_line(aes(x=Year, y=total, group=whatareyou, colour=whatareyou)) +
  #scale_fill_viridis() +
  scale_colour_manual(values=c("#D55E00", "#0072B2", "#73BFB8")) +
  #position = "jitter" + 
  theme_minimal() +
  theme(legend.position= c("right")) +
  labs(x="", y="Total stranded individuals", colour="Suborder") 
  


#Adding events to the plot 
plines <- plines + annotate("rect", xmin=1914, xmax=1918, ymin=0, ymax=800, alpha=.1, fill="gray64") +
  geom_text(
    aes(x = 1916, y = 780, label = "WWI"), size = 3, colour = "gray38")+ 
  annotate("rect", xmin=1939, xmax=1945, ymin=0, ymax=800, alpha=.1, fill="gray64") +
  geom_text(
    aes(x = 1942, y = 780, label = "WWII"), size = 3, colour = "gray38")+ 
  annotate("segment", x =1985, xend=1985, y=50, yend=725, colour = "gray38", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) +
  geom_text(
    aes(x = 1988, y = 780, label = "1985/1986 season:
        Moratorium on whaling comes into effect"), size = 3, colour = "gray38") +
  annotate("segment", x = 1990, xend = 1990, y = 50, yend = 565, colour="gray38", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1995, y = 620, label = "1990: CSIP and IWDG
   programmes start"), size = 3, colour = "gray38") +
  annotate("segment", x = 1945, xend = 1945, y = 50, yend = 540, colour="gray38", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1945, y = 620, label = "1945: Increase in 
        post-war fishing & 
        whaling effort"), size = 3, colour = "gray38")
  


# put it all together
gr <- grid.arrange(plines,
                   make_data(ds, years=c(1913, 1925), TRUE),
                   make_data(ds, years=c(1926, 1950)),
                   make_data(ds, years=c(1951, 1975)),
                   make_data(ds, years=c(1976, 2000)),
                   make_data(ds, years=c(2001, 2016)),
                   layout_matrix = matrix(c(1,2,1,3,1,4,1,5,1,6), 2, 5))
ggsave(gr, file="megaplot.pdf", height=13, width=10)


