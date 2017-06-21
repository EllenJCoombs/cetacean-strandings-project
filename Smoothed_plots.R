#Using Natalie's species code 


library(readr)
library(ggplot2) 


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
    theme_bw() +
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
ds <- read_csv("cleaned.data.300517.csv")

# This would get a full list of the whale species
# But for now we just want a couple as an example
# whales <- whale_list(ds, "Name.Current.Sci")

#If I want to look at all whales 
whales <- whale_list(ds, "Name.Current.Sci")

#If I want to look at just these species 
whales <- c("delphinus delphis", "phocoena phocoena", "kogia sima", "orcinus orca")

# Plot graphs
plot_all_whale_years(ds, species.col = "Name.Current.Sci", whales,
                     binwidth = 0.5, start.date = 1913, end.date = 2017)


#Original smoothedplots #Using Cleaneddata 
#Having a look at how many of each species 
select(cleaneddata, Name.Current.Sci)
#'Species' just looking at Name.Current.Sci
species <- count(cleaneddata, Name.Current.Sci)
#'Speciesyearcount' cleaned data: a count of current scientific name and year 
speciesyearcount <- count(cleaneddata, Name.Current.Sci, Year)

View(species)
#This is just species and the year - no counting or sorting 
speciesyear <- select(cleaneddata, Year, Name.Current.Sci)

#Plotting the above 
ggplot(data = speciesyearcount, aes(x = Year, y = n, colour = Name.Current.Sci))+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA)) +
  labs(x = "Year", y = "Species count") +
  geom_line() +
  xlim(1913, 2017) +
  labs(title = "All species counts") +
  facet_wrap(~ Name.Current.Sci)

#Focusing on phocoena phocoena 
pphocoena <- speciesyearcount %>% 
  filter(Name.Current.Sci == "phocoena phocoena")

ggplot(data = pphocoena, aes(x = Year, y = n, colour = Name.Current.Sci))+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA)) +
  labs(x = "Year", y = "Phocoena phocoena count") +
  geom_line() +
  xlim(1913, 2017)












#Not sure why this doesn't work...
library(ggplot2)
library(data.table) 
library(dplyr)
#for rbindlist

# break data.frame into chunks by type
#renaming n to count 
rename(speciesyearcount, count = n)
type_chunks <- split(speciesyearcount, speciesyearcount$Name.Current.Sci)

# apply spline function to each chunk
spline_chunks <- lapply(seq_along(type_chunks), function(i) {
  x <- type_chunks[[i]]
  data.frame(Name.Current.Sci = names(type_chunks)[i],
             spline(x$Year, x$count, n = 1331)) # choose a value for n
})

# crush chunks back to one data.frame
spline_df <- rbindlist(spline_chunks)

# original plot
ggplot(speciesyearcount, aes(Year, n)) + geom_line(aes(color = Name.Current.Sci), size = 0.2)

# plot smoothed version
ggplot(spline_df, aes(x, y)) + geom_line(aes(color = Name.Current.Sci), size = 0.5)













