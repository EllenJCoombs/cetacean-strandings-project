

#Plots of predictors 

library(ggplot2)

#Geom_mean_max plot 
p <- ggplot(data=Geom_mean_max, aes(x=Year, y=Max_K_index, group=1)) +
  geom_line(colour = "gray44") +
  theme_light()

p + labs(y = "Geomagnetic index (k index)")

#Population plot 
p <- ggplot(data=Population, aes(x=YEAR, y=POPULATION, group=1)) +
  geom_line(colour = "gray44") +
  theme_light()

p + labs(y = "Human population (millions)", x = "Year")

#SST
p <- ggplot(data=SST_yearly_max, aes(x=year, y=year_max, group=1)) +
  geom_line(colour = "gray44") +
  theme_light()

p + labs(y = "Yearly maximum temperature (°C)", x = "Year")+
  geom_smooth()

#Storms 
p <- ggplot(data=Storms, aes(x=Year, y=Storms, group=1)) +
  geom_line(colour = "gray44") +
  theme_light()

p + labs(y = "Storm count", x = "Year") +
  geom_smooth()

#NAO_index 
p <- ggplot(data=NAO_index, aes(x=Year, y=NAO_index, group=1)) +
  geom_line(colour = "gray44") +
  theme_light()

p + labs(y = "North Atlantic Oscillation index", x = "Year") +
  geom_smooth()

#Fishing catch 
Fishing <- read.csv("Fishing_data_UK.csv")
p <- ggplot(data=Fishing, aes(x=Year, y=Annual.catches..1000.tonnes., group=1)) +
  geom_line(colour = "gray44") +
  theme_light()

p + labs(y = "Fishing count (1000 tonnes)", x = "Year") +
  geom_smooth()





