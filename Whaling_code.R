
#Using Natalie's code to look at whaling plots 

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



#Minkes 
whales <- c("balaenoptera acutorostrata")

# Plot graphs for 1985 - 2017 (that's the catch data I have)
minkes <- plot_all_whale_years(ds, species.col = "Name.Current.Sci", whales,
                     binwidth = 0.5, start.date = 1985, end.date = 2017)


#Minke catch data from Norway following 1986 moratorium - need to find dat from 1960

library(dplyr)
minkecount <- speciesyearcount %>%
  filter(Name.Current.Sci == "Balaenoptera acutorostrata")
rename(minkecount, Count = n)


minke_catch_data <- read.csv("Minke_catch_data_Norway_1986.csv")
  
ggplot() + 
  geom_line(data = minke_catch_data, aes(x = Year, y = Count), col = "red") +
  geom_line(data = minkecount, aes(x = Year, y = n), alpha = .5) +
  xlim(1985, 2015)


#######################################################################################
#Looking at whether the whaling data is consistent with the stranding data 
#Blue, Fin, Sei, Right, Bottlenose, Minke, Humpback, Sperm, others 

whaling <- read.csv("North_Atlantic_whaling.csv")
View(whaling)

#Selecting the species and the year 
whaling_species <- whaling %>%
  select(Year, Balaenoptera.musculus, Balaenoptera.physalus, 
         Megaptera.novaeangliae, Balaenoptera.borealis, Physeter.macrocephalus, 
         Others, Eubalaena.glacialis, Hyperoodon.ampullatus, Balaenoptera.acutorostrata, 
         Total.catch)

#Want to rename the columns to match species name in other plots 
whaling_species <- rename(whaling_species, "Balaenoptera physalus" = "Balaenoptera.physalus", 
       "Balaenoptera musculus" = "Balaenoptera.musculus",
       "Megaptera novaeangliae" = "Megaptera.novaeangliae",
       "Balaenoptera borealis" = "Balaenoptera.borealis",
       "Physeter macrocephalus" = "Physeter.macrocephalus", 
       "Eubalaena glacialis" = "Eubalaena.glacialis", 
       "Hyperoodon ampullatus" = "Hyperoodon.ampullatus",
       "Balaenoptera acutorostrata" = "Balaenoptera.acutorostrata")



#Plotting species and year 
library("reshape2")
library(RColorBrewer)

whaling_plot <- melt(whaling_species, id="Year")  # convert to long format

#Renaming the columns 
whaling_plot <- rename(whaling_plot, Count = value, 
       Name.Current.Sci = variable)

#Plotting all catch, North Atlantic + Total catch 
ggplot(data = whaling_plot,
       aes(x = Year, y = Count, colour = Name.Current.Sci, ylab = "Count")) +
  geom_line() +
  facet_wrap(~ Name.Current.Sci) 

#Want to order the data by year (not species as it currently is)
whaling_plot %>%
  arrange(Year)

#Need to remove "Total.Catch" column" 
whaling_species_no_total <- whaling_species %>%
  select("Year", "Balaenoptera musculus", "Balaenoptera physalus", 
         "Megaptera novaeangliae", "Balaenoptera borealis", "Physeter macrocephalus", 
         "Others", "Eubalaena glacialis", "Hyperoodon ampullatus", "Balaenoptera acutorostrata")


#If I want to look at total killed - just look at "Total.catch" in the "whaling_species") 


#Stripping out the same species from the stranding data (those that were hunted)
hunted_stranders <- cleaneddata %>% 
  filter(Name.Current.Sci %in% c("Balaenoptera musculus", "Balaenoptera physalus", 
                                 "Megaptera novaeangliae", "Balaenoptera borealis", 
                                 "Physeter macrocephalus", "Eubalaena glacialis", 
                                 "Hyperoodon ampullatus", "Balaenoptera acutorostrata"))

#Aggregating by year 
hunted_strandersyear <- count(hunted_stranders, Name.Current.Sci, Year)

#Arranging species by year by count 
hunted_stranders_total <- hunted_strandersyear %>%
  group_by(n, Year, Name.Current.Sci) %>%
  arrange(Year)


ggplot(data = hunted_stranders_total,
       aes(x = Year, y = n, colour = Name.Current.Sci, ylab = "Count")) +
  geom_line() +
  facet_wrap(~ Name.Current.Sci) 

#The totals of stranded whales (that were hunting candidates) stranding each year
Total_hunted_stranders <- aggregate(n ~ Year, hunted_stranders_total, sum)
#Total hunted each year (and only looking at 1913 - 2015)
Total_hunted <- whaling %>% 
  select(Year, Total.catch) %>%
  filter(Year %in% c(1913:2015))

#Want to plot total whaled and total strandings of hunted species 

#Changing the total.catch column to n 
Total_hunted_rename <- rename(Total_hunted, n = Total.catch)

#Plotting total hunted and total strandeds (of hunted species)
ggplot(Total_hunted_stranders$n*100, aes(x = Year, y = n, ylab = "Count")) + geom_line(aes(color = "Strandings"))+
  geom_line(data = Total_hunted_rename, aes(color="Hunted")) +
  scale_y_continuous(sec.axis = sec_axis(~.*10))
  labs(color = "Stranding and hunting data") 

  
#Bind the two datasets (Total_hunted and Total_hunted_stranding)
#Clean up the data 
Catch_and_strandings <- bind_cols(Total_hunted, Total_hunted_stranders) %>%
  select(Year, Total.catch, n) %>%
  rename("Strandings" = "n")

#Plot them both on the same graph 
  p <- ggplot(Catch_and_strandings, aes(x = Year))
  p <- p + geom_line(aes(y = Strandings, colour = "Total strandings"))
  
  # adding the stranding data, transformed to match roughly the range of the total catch
  p <- p + geom_line(aes(y = Total.catch/20, colour = "Total catch"))
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*20, name = "Total catch"))
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("blue", "red"))
  p <- p + labs(y = "Total stranding",
                x = "Year",
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.9))
  p  

