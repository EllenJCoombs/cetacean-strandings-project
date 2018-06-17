
library(dplyr)
library(ggplot2)

#This code plots new Figure 1 (line plot) 
#Also text and circled mass strandings 
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
  annotate("segment", x =1985, xend=1985, y=130, yend=750, colour = "gray38", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) +
  geom_text(
    aes(x = 1988, y = 770, label = "1985/1986 season:
        Moratorium on whaling comes into effect"), size = 3, colour = "gray38") +
  #Annotation coordinates 
  annotate("segment", x = 1990, xend = 1990, y = 230, yend = 585, colour="gray38", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1992, y = 610, label = "1990: CSIP and IWDG
        programmes start"), size = 3, colour = "gray38") +
  annotate("segment", x = 1947, xend = 1947, y = 40, yend = 550, colour="gray38", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1953, y = 430, label = "1950s: Increase in 
        post-war fishing & 
        whaling effort"), size = 3, colour = "gray38") + 
  annotate("segment", x = 1953, xend = 1953, y = 40, yend = 400, colour="gray38", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1946, y = 590, label = "1946: NATO military testing 
        in European waters:
        submarine, military sonar,
        and torpedo testing increase"), size = 3, colour = "gray38") +
  annotate("segment", x = 1920, xend = 1920, y = 40, yend = 400, colour="gray38", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1920, y = 420, label = "1920s: Sonar is used 
        in UK and French waters"), size = 3, colour = "gray38") +
  annotate("segment", x = 2000, xend = 2000, y = 40, yend = 110, colour="gray38", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 2000, y = 140, label = "2000s: Increase in 
        pile-driving for offshore 
        wind turbines"), size = 3, colour = "gray38") +
  annotate("segment", x = 1960, xend = 1960, y = 40, yend = 90, colour="gray38", size=0.5, arrow=arrow(length=unit(0.1,"cm"))) + 
  geom_text(
    aes(x = 1960, y = 140, label = "1960s: Increase in
use of polychlorinated 
biphenyls (PCBs)
and other chemical 
pollutants"), size = 3, colour = "gray38")


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
  

#Run the plot
plines
