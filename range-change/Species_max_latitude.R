#Max latitude per species for stranding events 
library(readr)
library(ggplot2) 
library(gridExtra)
library(lazyeval)

# Plot whale occurrences through time
# Note that I hardcoded the Year variable due to some issues I had with ggplot
# You'd need to change this if you changed the name of that variable.
# species.name = name of the species if looking at only one
# binwidth = width of bins in histogram
# start.date and end.date - allows you to change the plotting window years
# You might want to do other things like control the y axis limits, or labels etc.


UK_IRL_stranding_events <- read.csv("UK_IRL_stranding_events.csv")
UK_and_Irish_stranding_events$X <- NULL

Lat_list <- UK_IRL_stranding_events %>%
  select(Name.Current.Sci, Latitude, Year)

Lat_list <- Lat_list %>%
  arrange(Name.Current.Sci)


Species_lat <- Lat_list %>%
  filter(Name.Current.Sci == "Balaenoptera acutorostrata")

Species_lat <- aggregate(Species_lat$Latitude, by = list(Species_lat$Year), max)




geomag_yearly_max <- aggregate(geomag$k, by = list(geomag$Year), max)




Species_lat <- function(Lat_list, Name.Current.Sci, Latitude) {
  Lat_list %>%
    group_by_(Name.Current.Sci) %>%
    aggregate(Lat_list$Latitude, by = list(Lat_list$Year), max)
}

Species_Lat_max <- aggregate(Lat_list$Latitude, by = list(Lat_list$Latitude), max)

# Choose one species from dataset
# Input name of the column with the species data
# and the name of the species (both in quotation marks)
choose_species <- function(data, species.col.no, species.name){
  data[which(data[, species.col.no] == species.name), ]
}

# Select a list of all whale species in the strandings dataset
# Currently this deletes NAs but does not fix "unknowns" etc.
# Replace unknowns with NA when cleaning data
whale_list <- function(data, species.col){
  # Extract column number of species column
  species.col.no <- which(names(data) == species.col)
  # Get list of whale species
  whales <- unique(data[, species.col.no])
  # Unlist so this is a list of species not a dataframe
  whales <- unlist(whales, use.names = FALSE)
  # Remove NAs
  whales[!is.na(whales)]
}

# Plot all the plots for all the whales
# year = name of date variable, not in quotation marks
# species.name = name of the species if looking at only one
# binwidth = width of bins in histogram
# start.date and end.date - allows you to change the plotting window years
# species.col = name of the column with the species data in quotation marks
# whales = a vector of names of the whales you want to plot
# requires library(gridExtra)
# requires library(ggplot2)
plot_all_whale_years <- function(data, species.col, whales,
                                 binwidth = 0.5, start.date = 1913, end.date = 2017){
  
  # Extract column number of species column
  species.col.no <- which(names(data) == species.col)
  
  # Make an empty list to put graphs into
  whale.graph.list <- list()
  
  # Loop through each of the whales in the list
  for(i in seq_along(whales)){
    
    # Select one whale
    one.whale.name <- whales[i]
    one.whale <- choose_species(data, species.col.no, one.whale.name)
    
    # Plot the graph
    whale.graph.list[[i]] <- 
      whale_years_plot(one.whale, species.name = one.whale.name, binwidth, 
                       start.date, end.date)
  } # end loop
  
  # Plot all the plots in whale.graph.list
  do.call(grid.arrange, whale.graph.list)
  
}

# EXAMPLE
library(ggplot2)
library(gridExtra)

# Read in the data
ds <- read_csv("cleandatesnames.csv")
ds$X1 <- NULL
ds$X <- NULL

# This would get a full list of the whale species
# But for now we just want a couple as an example
# whales <- whale_list(ds, "Name.Current.Sci")

#If I want to look at all whales 
whales <- whale_list(ds, "Name.Current.Sci")

#If I want to look at just these species 
whales <- c("Delphinus delphis", "Orcinus orca", "Balaenoptera acutorostrata", "Physeter macrocephalus") 


# Plot graphs
plot_all_whale_years(ds, species.col = "Name.Current.Sci", whales,
                     binwidth = 0.5, start.date = 1913, end.date = 2017)

