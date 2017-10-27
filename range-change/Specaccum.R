#Accumulation curves for different locations and different time slots 

#Split into north/south as before 

library(dplyr)
#Species richness using the vegan package 
library(vegan)
library(ggplot2)
#for altering my data into presence/absence
library(picante)


UK_and_Irish <- read.csv("UK_and_Irish_strandings.csv")
UK_and_Irish$X.1 <- NULL
UK_and_Irish$X <- NULL


North_strandings <- UK_and_Irish %>%
  filter(Latitude > 55.5)


#South_Strandings 
South_strandings <- UK_and_Irish %>%
  filter(Latitude < 55.5) %>%
  filter(Longitude < 4)

South_strandings$X.1 <- NULL
South_strandings$X <-NULL


#Turn these data sets into matices 
#South strandings 
#Need to reorder the data
south_speciesyearcount <- dplyr::count(South_strandings, Name.Current.Sci, Year) %>%
  na.omit()

#Remove unknowns 
south_known <-south_speciesyearcount %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))

south_known <- south_known %>%
  dplyr::rename(Species = Name.Current.Sci)

#Data need to be reordered for putting into matrix 
south_reordering <- south_known[c("Year", "n", "Species")]
south.matrix <- sample2matrix(south_reordering)

#Number of species per year 
specnumber(south.matrix)
#Species accumulation curves 
south.curve <- specaccum(south.matrix, method = "random", permutations = 1000)
south.curve
#Plot 
plot(south.curve, ci.type = "poly", col = "blue", ci.col = "lightblue", 
     lwd = 2, ci.lty = 0, xlab = "Years: 1913 - 2015", 
     ylab = "Cumulative number of whale species: Southern UK and Ireland")



#Turn these data sets into matices 
#North strandings 
#Need to reorder the data
north_speciesyearcount <- dplyr::count(North_strandings, Name.Current.Sci, Year) %>%
  na.omit()

#Remove unknowns 
north_known <-north_speciesyearcount %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))

north_known <- north_known %>%
  dplyr::rename(Species = Name.Current.Sci)

#Data need to be reordered for putting into matrix 
north_reordering <- north_known[c("Year", "n", "Species")]
north.matrix <- sample2matrix(north_reordering)

#Number of species per year 
specnumber(north.matrix)
#Species accumulation curves 
north.curve <- specaccum(north.matrix, method = "random", permutations = 1000)
north.curve
#Plot 
plot(north.curve, ci.type = "poly", col = "blue", ci.col = "lightblue", 
     lwd = 2, ci.lty = 0, xlab = "Years: 1913 - 2015", 
     ylab = "Cumulative number of whale species: Northern UK and Ireland")

#Attempting to plot two specaccum on the same plot 
l <- list(south.curve, north.curve) 

# Apply a plotting function over the indices of the list
sapply(seq_along(l), function(i) {
  if (i==1) { # If it's the first list element, use plot()
    with(l[[i]], {
      plot(sites, richness, type='l', ylim= c(0,30), 
           xlab='Years', ylab='Random', las=1)
      segments(seq_len(max(sites)), y0=richness - 2*sd, 
               y1=richness + 2*sd)
    })    
  } else {
    with(l[[i]], { # for subsequent elements, use lines()
      lines(sites, richness, col=i)
      segments(seq_len(max(sites)), y0=richness - 2*sd, 
               y1=richness + 2*sd, col=i)
    })     
  }
})

legend('bottomright', c('South', 'North'), col=1:2, lty=1, 
       bty='n', inset=0.025)



#North and South plots of all strandings (events?)

#Stranding events - North and South #######################################

library(tidyverse)
#North_strandings = unknowns removed 

duplicated(North_strandings$S.W.No.)
#Or you can use unique 
unique(North_strandings$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
North_events <- North_strandings[!duplicated(North_strandings$S.W.No.), ]

#Removing duplicates from SW (CSIP data)
North_events$S.W.No. <- (sub("\\.\\d+$","", North_events$S.W.No.))
North_events <- North_events[!duplicated(North_events$S.W.No.), ]

#Removing duplicates 
North_events <- North_events[!duplicated(North_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]

#for all species 
ggplot(North_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

#Count for GAMs
North_events_count <- dplyr::count(North_events, Year)
North_events_count <- North_events_count %>% 
  dplyr::rename(North_events = n)

#Adds an extra column with all the years in 
North_events_count <- North_events_count %>% 
  complete(Year = seq(min(1913), max(2015), 1L))

#NAs -> 0 
North_events_count[is.na(North_events_count)] <- 0



####South events 
duplicated(South_strandings$S.W.No.)
#Or you can use unique 
unique(South_strandings$S.W.No.)

#This works to get rid of e.g. 1932/14, 1932/14 
South_events <- South_strandings[!duplicated(South_strandings$S.W.No.), ]


#Removing duplicates from SW (CSIP data)
South_events$S.W.No. <- (sub("\\.\\d+$","", South_events$S.W.No.))
South_events <- South_events[!duplicated(South_events$S.W.No.), ]

#Removing duplicates 
South_events <- South_events[!duplicated(South_events[c("Name.Current.Sci", "Latitude", "Longitude", "Date")]), ]

ggplot(South_events, aes(x = Year, fill = Name.Current.Sci)) +
  geom_histogram(binwidth = 0.5)

South_events_count <- count(South_events, Year) 
South_events_count <- South_events_count %>%
  rename(South_events = n)


#Plotting both, north and south events counts per year 

aa <- ggplot() + 
geom_line(data = South_events_count, aes(x = Year, y = South_events, colour = "South_events")) +
  geom_line(data = North_events_count, aes(x = Year, y = North_events, colour = "North_events")) + 
                          theme_bw()
  
#Adding some annotation for when species first arrive in the records 
#This code adds arrows to the plot 
aa <- aa + annotate("segment", x = 1988, xend = 1988, y = 20, yend = -50, colour="red", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1988, y = -70, label = "First northern Striped dolphin record"), size = 2, colour = "red")

aa <- aa + annotate("segment", x = 1934, xend = 1934, y = 50, yend = 90, colour="turquoise", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1934, y = 110, label = "First southern Striped dolphin record"), size = 2, colour = "turquoise")

aa <- aa + annotate("segment", x = 1996, xend = 1996, y = 120, yend = 50, colour="red", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1999, y = 30, label = "First northern Pygmy 
        sperm whale record"), size = 2, colour = "red")

aa <- aa + annotate("segment", x = 1966, xend = 1966, y = 30, yend = 80, colour="turquoise", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1966, y = 100, label = "First southern Pygmy 
        sperm whale record"), size = 2, colour = "turquoise")

aa







