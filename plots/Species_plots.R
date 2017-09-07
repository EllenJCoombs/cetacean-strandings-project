
library(readr)
library(ggplot2) 
library(gridExtra)

# Plot whale occurrences through time
# Note that I hardcoded the Year variable due to some issues I had with ggplot
# You'd need to change this if you changed the name of that variable.
# species.name = name of the species if looking at only one
# binwidth = width of bins in histogram
# start.date and end.date - allows you to change the plotting window years
# You might want to do other things like control the y axis limits, or labels etc.
whale_years_plot <- function(data, species.name = NULL, binwidth = 0.5, 
                             start.date = 1913, end.date = 2017){
  
  ggplot(data, aes(x = Year)) +
    geom_histogram(binwidth = binwidth) +
    xlim(start.date, end.date) +
    #theme_bw() +
    labs(title = species.name)
}

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

# This would get a full list of the whale species
# But for now we just want a couple as an example
# whales <- whale_list(ds, "Name.Current.Sci")

#If I want to look at all whales 
whales <- whale_list(ds, "Name.Current.Sci")

#If I want to look at just these species 
whales <- c("Delphinus delphis", "Orcinus orca") 
            
          #, "phocoena phocoena", "kogia sima", "orcinus orca")

# Plot graphs
plot_all_whale_years(ds, species.col = "Name.Current.Sci", whales,
                     binwidth = 0.5, start.date = 1913, end.date = 2015)



#Mysitcetes ###########################################################################
mysticetesplot <- c("balaenoptera acutorostrata", "balaenoptera borealis", "balaenoptera musculus", "balaenoptera physalus", 
                    "megaptera novaeangliae", "un. mystitcete", "unknown balaenoptera", "unknown balaenopterid",
                    "unknown mysticete") 
                  
plot_all_whale_years(ds, species.col = "Name.Current.Sci", mysticetesplot,
                     binwidth = 0.5, start.date = 1913, end.date = 2017)

#unknowns #############################################################################
unknowns <- c("unknown odontocete", "unknown mysticete", "unknown", "unknown balaenoptera", "unknown delphinidae")
plot_all_whale_years(ds, species.col = "Name.Current.Sci", unknowns,
                     binwidth = 0.5, start.date = 1913, end.date = 2017)

#Beaked and bottlenose whales ########################################################
beakers <- c("hyperoodon ampullatus", "mesoplodon densirostris", "mesoplodon europaeus", "mesoplodon mirus")
plot_all_whale_years(ds, species.col = "Name.Current.Sci", beakers,
                     binwidth = 0.5, start.date = 1913, end.date = 2017) 

#Bycatch candidates 
bycatch_candidates <- c("phocoena phocoena", "delphinus delphis", "tursiops truncatus")
plot_all_whale_years(ds, species.col = "Name.Current.Sci", bycatch_candidates,
                     binwidth = 0.5, start.date = 1913, end.date = 2017) 


#Plot without Phocoena Phocoena
#Need to strip out PP
allphocoena <- cleaneddata %>%
  filter(Name.Current.Sci == "Phocoena phocoena")

nophocoena <- cleaneddata[ !(cleaneddata$Name.Current.Sci %in% allphocoena$Name.Current.Sci), ]
ggplot(nophocoena, aes(x = Year)) +
  stat_count(width = 0.5) 
  #Facet_wrap if splitting by species 
  #facet_wrap(~ Name.Current.Sci)

#Removing Phocoena from the odontocetes (not just from all whales as above as odontocetes make up
#most strandings 
nophocoena_odonts <- odontocetes[ !(odontocetes$Name.Current.Sci %in% allphocoena$Name.Current.Sci), ]
ggplot(nophocoena_odonts, aes(x = Year)) +
  stat_count(width = 0.5) +
  facet_wrap(~ Name.Current.Sci)



D_delpis <- ds %>%
  filter(Name.Current.Sci == "Delphinus delphis")

ab <- ggplot(D_delpis, aes(x = Year, colour = Name.Current.Sci)) + 
  stat_count(width = 0.5) +
  theme_bw()
  
ab + coord_cartesian(xlim=c(1913:2015))



