
library(dplyr)
install.packages("ggthemes")
library(ggthemes) #for theme_tufte 

plines <- ggplot(linesdat) +
  scale_x_continuous(labels=seq(1900, 2025, by=25),
                     breaks=seq(1900, 2025, by=25),
                     expand=c(0, 0)) +
  geom_line(aes(x=Year, y=total, group=whatareyou, colour=whatareyou)) +
  #scale_fill_viridis() +
  #Changed variable colours - these are colour blind friendly 
  scale_colour_manual(values=c("#D55E00", "#000000", "#73BFB8")) +
  #position = "jitter" + 
  theme_tufte() +
  theme(legend.position= "right") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  labs(x="", y="Total stranded individuals", colour="Suborder")


plines


#Odontocetes plot for BES
odontocetesplot <- ds %>%
  filter(whatareyou == "Odontocete")
  
odontocetesplot <- odontocetesplot %>%
  group_by(Year, whatareyou) %>%
  summarise(total=n())


#Mysticetes plot for BES
mysticetesplot <- ds %>%
  filter(whatareyou == "Mysticete")

mysticetesplot <- mysticetesplot %>%
  group_by(Year, whatareyou) %>%
  summarise(total=n())


#All data for BES plot 
allplot <- ds %>%
  group_by(Year, whatareyou) %>%
  summarise(total=n())

allplot <- allplot %>%
  filter(whatareyou == "All")


#All plus odontocete 
all_and_odonts <- ds %>% 
  filter(whatareyou %in% c("Odontocete", "All"))

all_odont_plot <- all_and_odonts %>%
  group_by(Year, whatareyou) %>%
  summarise(total=n())





