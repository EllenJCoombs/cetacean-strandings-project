

#Plots of predictors 

library(ggplot2)

#Geom_mean_max plot 
p <- ggplot(data=Geom_mean_max, aes(x=Year, y=Max_K_index, group=1)) +
  geom_line(colour = "gray44") +
  theme_light() + 
  labs(y = "Geomagnetic index (k index)")

#Population plot 
q <- ggplot(data=Population, aes(x=YEAR, y=POPULATION, group=1)) +
  geom_line(colour = "gray44") +
  theme_light() + 
  labs(y = "Human population (millions)", x = "Year")

#SST
r <- ggplot(data=SST_yearly_max, aes(x=year, y=year_max, group=1)) +
  geom_line(colour = "gray44") +
  theme_light() + 
  labs(y = "Maximum sea surface temperature (°C)", x = "Year")+
  theme_light()

#Storms 
s <- ggplot(data=Storm_data, aes(x=Year, y=Storms, group=1)) +
  geom_line(colour = "gray44") +
  theme_light() + 
  labs(y = "Storm count", x = "Year") +
  theme_light()

#NAO_index 
t <- ggplot(data=NAO_index, aes(x=Year, y=NAO_index, group=1)) +
  geom_line(colour = "gray44") +
  theme_light() + 
  labs(y = "North Atlantic Oscillation index", x = "Year") +
  theme_light()

#Fishing catch 
Fishing <- read.csv("Fishing_data_UK.csv")
u <- ggplot(data=Fishing, aes(x=Year, y=Annual.catches..1000.tonnes., group=1)) +
  geom_line(colour = "gray44") +
  theme_light() + 
  labs(y = "Fishing count (1000 tonnes)", x = "Year") +
  theme_light()


#Putting together for the paper
require(gridExtra)
plot1 <- 
plot2 <- qplot(1)
grid.arrange(s, p, r, t, u, q, ncol=3)











