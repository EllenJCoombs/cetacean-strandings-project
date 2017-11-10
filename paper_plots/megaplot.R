# here is the code for the megaplot

library(ggplot2)
library(viridis)
library(dplyr)
library(maps)
library(hexbin)
library(ggmap)
library(gridExtra)

# Extract UK map
#Added Ireland·
uk <- map_data("world", regions = c('UK', 'Ireland', 'Guernsey', 'Jersey', 'Isle of Man'))

# Create base map
gg1 <-
  ggplot() +
  geom_polygon(data = uk, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  coord_fixed(1.3)

# Read in the strandings data
#ds <- read.csv("UK_and_Irish_strandings.csv")

load("allparv.RData")

ds <- allparv



make_data <- function(ds, years){
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
    facet_wrap(~whatareyou, ncol=1) +
    labs(x="", y="") +
    theme(legend.position="none",
          strip.text=element_blank()) +
    scale_fill_viridis(trans = "log", na.value="white",
                       limits = c(1, 501),
                       breaks = c(1, 10, 20, 50, 100, 250, 500),
                       labels = c(1, 10, 20, 50, 100, 250, 500))

  return(pp)
}

linesdat <- ds %>%
  group_by(Year, whatareyou) %>%
  summarize(total=n())

plines <- ggplot(linesdat) +
  theme_minimal() +
  geom_line(aes(x=Year, y=total, group=whatareyou))

grid.arrange(plines,
             make_data(ds, years=c(1913, 1950)),
             make_data(ds, years=c(1951, 2000)),
             make_data(ds, years=c(2000, 2016)),
             layout_matrix = matrix(c(1,2,1,3,1,4), 2, 3))




