
#Required packages 
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)

#This code plots new Figure 1 (line plot)
#New code on 15/06/2018 with the key underneath 

#Create text grob 
Text1 = textGrob(paste("
1. 1920s: Sonar use in French and UK waters
2. 1946: NATO military testing in European waters: submarine, sonar, & torpedo testing increase
3. 1950s: Increase in post-war fishing & whaling effort
4. 1960s: Increase in use of polychlorinated biphenyls (PCBs) and other chemical pollutants 
5. 1985/86 season: Moratorium on whaling comes into effect 
6. 1990: The CSIP and IWDG programmes start
7. 2000s: Increase in pile-driving for offshore wind turbines"), 
                       gp = gpar(fontsize = 8, col = 'black'), hjust = 0)

# get the data
linesdat <- ds %>%
  group_by(Year, whatareyou) %>%
  summarise(total=n())


# make the timeseries plot
plines <- ggplot(linesdat) +
  scale_x_continuous(labels=seq(1900, 2025, by=25),
                     breaks=seq(1900, 2025, by=25),
                     expand=c(0, 0)) +
  geom_line(aes(x=Year, y=total, group=whatareyou, colour=whatareyou)) +
  #scale_fill_viridis() +
  #Changed variable colours - these are colour blind friendly 
  scale_colour_manual(values=c("#D55E00", "#0072B2", "#73BFB8")) +
  #position = "jitter" + 
  theme_minimal() +
  theme(legend.position= "right") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  labs(x="", y="Total stranded individuals", colour="Suborder")

#Adding events to the plot 
plines <- plines + annotate("rect", xmin=1914, xmax=1918, ymin=0, ymax=800, alpha=.1, fill="gray22") +
  geom_text(
    #where the label starts (on the x axis) and where it finishes (on the y axis)
    #Colour of the label 
    aes(x = 1916, y = 780, label = "WWI"), size = 3, colour = "gray38") + 
  annotate("rect", xmin=1939, xmax=1945, ymin=0, ymax=800, alpha=.1, fill="gray22") +
  geom_text(
    aes(x = 1942, y = 780, label = "WWII"), size = 3, colour = "gray38") + 
  geom_text(
    aes(x = 1990, y = 230, label = "5"), size = 3, colour = "gray38") +
  geom_text(
    aes(x = 1992, y = 360, label = "6"), size = 3, colour = "gray38") +
  geom_text(
    aes(x = 1953, y = 50, label = "3"), size = 3, colour = "gray38") +
  geom_text(
    aes(x = 1946, y = 50, label = "2"), size = 3, colour = "gray38") +
  geom_text(
    aes(x = 1920, y = 50, label = "1"), size = 3, colour = "gray38") + 
  geom_text(
    aes(x = 2000, y = 500, label = "7"), size = 3, colour = "gray38") + 
  geom_text(
    aes(x = 1960, y = 50, label = "4"), size = 3, colour = "gray38")
  

#Adding circles for mass stranding years 
plines <- plines + geom_point(aes(x=1950, y=275), size=10, shape=1, color="black") + 
  geom_point(aes(x=1927, y=190), size=10, shape=1, color="gray29") +
  geom_point(aes(x=1935, y=121), size=10, shape=1, color="gray29") +
  geom_point(aes(x=1955, y=85), size=10, shape=1, color="gray29") +
  geom_point(aes(x=1983, y=191), size=10, shape=1, color="gray29") +
  geom_point(aes(x=1985, y=106), size=10, shape=1, color="gray29") +
  geom_point(aes(x=2008, y=614), size=10, shape=1, color="gray29") + 
  geom_point(aes(x=2011, y=705), size=10, shape=1, color="gray29") +
  geom_point(aes(x=2012, y=714), size=10, shape=1, color="gray29") +
  geom_point(aes(x=2015, y=667), size=10, shape=1, color="gray29")


#Alter this code to change the positioning of the text grob 
#Add a row below the 2nd from the bottom
gd = ggplotGrob(plines)
gd = gtable_add_rows(gd, unit(1.5, "grobheight", Text1), -3)
#Add 'lab' grob to that row, under the plot panel
gd = gtable_add_grob(gd, Text1, t = -3, l = gd$layout[gd$layout$name == "panel",]$l) 

grid.newpage()
grid.draw(gd)


#Run the plot - this runs it without the text underneath 
#plines
