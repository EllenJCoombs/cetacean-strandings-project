#Make a measles plot - plot of individual species stranding through time 

library(plyr)
library(ggplot2)
library(viridis)
library(base)

# load all the strandings data
alls <- read.csv("all_strandings.csv") 


# chance Total_strandings to Total_events to get # events
plotdat <- ddply(alls, .(Year, Species), summarize, total=sum(Total_strandings))

#Change the factor levels
#What levels do we have? 
levels(plotdat$Species)

#Specify the factor levels 
plotdat$Species <- factor(c("Balaenoptera acutorostrata","Balaenoptera borealis",
                       "Balaenoptera musculus", "Balaenoptera physalus", 
                       "Delphinus delphis", "Globicephala melas", 
                       "Grampus griseus",  "Hyperoodon ampullatus", 
                       "Kogia breviceps", "Lagenorhynchus acutus", 
                       "Lagenorhynchus albirostris","Megaptera novaeangliae", 
                       "Mesoplodon bidens", "Mesoplodon mirus", "Orcinus orca",
                       "Phocoena phocoena", "Physeter macrocephalus", 
                       "Pseudorca crassidens", "Stenella coeruleoalba",
                       "Tursiops truncatus", "Ziphius cavirostris"))


plotdat$Species <- factor(plotdat$Species, levels = c("Balaenoptera acutorostrata","Balaenoptera borealis",
                                                      "Balaenoptera musculus", "Balaenoptera physalus",
                                                      "Megaptera novaeangliae","Delphinus delphis",
                                                      "Globicephala melas","Grampus griseus",
                                                      "Hyperoodon ampullatus","Kogia breviceps",
                                                      "Lagenorhynchus acutus","Lagenorhynchus albirostris",
                                                      "Mesoplodon bidens","Mesoplodon mirus","Orcinus orca",
                                                      "Phocoena phocoena","Physeter macrocephalus",
                                                      "Pseudorca crassidens","Stenella coeruleoalba",
                                                      "Tursiops truncatus","Ziphius cavirostris"))



# remove the zeros so they are transparent in plot
plotdat <- plotdat[plotdat$total != 0,]
# reverse sp. order alphabetically
plotdat$Species <- factor(plotdat$Species, levels = rev(levels(plotdat$Species)))

#what should the upper limit be?
#> max(plotdat$total)
#[1] 501

#build the plot
p <- ggplot(plotdat) +
  geom_tile(aes(x=Year, y=Species, fill=total)) +
  scale_x_continuous(expand=c(0,0)) +
  theme_minimal() +
  labs(fill="Individuals", y="") +
  #needs to be on the log scale because of pho^2 :( 
  scale_fill_viridis(trans = "log", na.value="white",
                     #501 was from the above commented 'max' code 
                     limits = c(1, 501),
                     breaks = c(1, 10, 20, 50, 100, 250, 500),
                     labels = c(1, 10, 20, 50, 100, 250, 500)) +
  theme(legend.position="bottom", legend.key.width=unit(0.1, "npc"),
        #This makes the text italic 
        axis.text.y=element_text(face="italic"))

print(p)
#Save plot 
ggsave(p, filename="measles.pdf", width=10, height=6)
