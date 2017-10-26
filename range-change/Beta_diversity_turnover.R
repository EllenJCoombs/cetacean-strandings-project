#Beta diversity per decade 
#Community diversity 

#Biodiversity stats per decade 
#Hex maps 
#How do species ranges change over time? 


#Beta diversity - species turnover 

#Species richness using the vegan package 
library(vegan)
library(ggplot2)
#for altering my data into presence/absence
library(picante)

UK_and_Irish <- read.csv("UK_and_Irish_strandings.csv") 


#For this code I am using the whole dataset (not just stranding events) as I am looking at species 
#numbers rather than individual strandings 
#The unknowns are removed so that they aren't mistaken for species 

#Need to reorder the data
speciesyearcount <- dplyr::count(UK_and_Irish, Name.Current.Sci, Year) %>%
  na.omit()

#Remove unknowns 
species_known <- speciesyearcount %>% 
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))

species_known <- species_known %>%
  dplyr::rename(Species = Name.Current.Sci)

#Data need to be reordered for putting into matrix 
reordering <- species_known[c("Year", "n", "Species")]
whale.matrix <- sample2matrix(reordering)

#Number of species per year 
specnumber(whale.matrix)

#Doublecheck
species_richness <- species_known %>%
  dplyr::count(Year) 

species_richness <- species_richness %>%
  dplyr::rename(Total_richness = nn)

#plot of Richness 
#Messing around with geom_smooth 
ggplot(data = species_richness, aes(x = Year, y = Total_richness)) +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA)) +
  labs(x = "Year", y = "Species richness") +
  geom_smooth() +
  geom_point()


#Plotting richness using 'species known'
ggplot(data = species_known, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  labs(x = "Year", y = "Species richness") +
  theme_bw()

ggplot() +
  geom_line(data = species_richness, aes(x = Year, y = Total_richness)) +
  xlim(1913,2015)

write.csv(species_richness, file = "Richness.csv")

#Alpha diversity 
#Simpson's diversity index 
simpson <- diversity(whale.matrix, index = "simpson")
simpson
plot(simpson)

#Shannon's diversity index 
shannon <- diversity(whale.matrix, index = "shannon")
plot(shannon)

#used years as sites 
rarecurve(whale.matrix)

#Beta diversity
betadiver(help=TRUE)
betadiver(whale.matrix, method = "j")
betadiver(whale.matrix, method = "sor")
betadiver(whale.matrix, method = "w")

#Looking at species by year - a count of each species total
speciestotal <- aggregate(n ~ Species, species_known, sum) %>%
  na.omit()

#Gamma diversity 
length(unique(species_known$Species))

#This adds up species where abundance is not NA
length(unique(species_known$Species[!is.na(species_known$n) & species_known$n != 0]))

#Species accumulation curves 
whale.curve <- specaccum(whale.matrix, method = "random", permutations = 1000)
whale.curve

plot(whale.curve, ci.type = "poly", col = "blue", ci.col = "lightblue", 
     lwd = 2, ci.lty = 0, xlab = "Years: 1913 - 2015", 
     ylab = "Cumulative number of whale species")


#Playing around with permutations 
#whale.curve1 <- specaccum(whale.matrix, method = "random", permutations = 1)
#whale.curve2 <- specaccum(whale.matrix, method = "random", permutations = 1)

#par(mfrow = c(1,2))

#plot(whale.curve1,  
#xlab = "number of sites", ylab = "Cumulative number of whale species")

#plot(whale.curve2, 
#xlab = "number of sites", ylab = "Cumulative number of whale species")

#par(mfrow = c(1,1))


#This includes unknowns e.g. 'unknown mysticete'
#Species per year (this no longer incorporates NAs)
total_speciesbyyear <- aggregate(speciesyearcount$n, by = speciesyearcount[c('Year')], length)
rarecurve(total_speciesbyyear)

dev.off()

#Estimating total richness across years 
#Chao2
specpool(whale.matrix)
#Species     chao  chao.se    jack1 jack1.se    jack2     boot  boot.se   n
#All            33 35.64078 3.455292 36.96117 2.429669 37.97059 35.00546 1.346503 103


##################################################################################################
#Function for looking at beta diversity per decade 
#Decadal hex plots 
#Manual solution 

#Splitting data into decades   
reordering$Decade <- gsub("[0-9]$", "0", reordering$Year)

decades <- reordering %>% 
  dplyr::count(Species, Decade)
  
decades <- decades %>%
  dplyr::count(Decade)

decades <- decades %>% 
  dplyr::rename(Species_richness = n) 

#Biodiversity stats per decade 
reordering2 <- reordering[c("Decade", "n", "Species")]
whale.matrix2 <- sample2matrix(reordering2)


#Number of species per year 
specnumber(whale.matrix2)

#plot of Richness 
#Messing around with geom_smooth 
ggplot(data = decades, aes(x = Decade, y = Species_richness)) +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA)) +
  labs(x = "Year", y = "Species richness") +
  geom_smooth() +
  geom_point()


write.csv(decades, file = "Decadal_richness.csv")

#Alpha diversity 
#Simpson's diversity index 
simpson <- diversity(whale.matrix2, index = "simpson")
simpson
plot(simpson)
hist(simpson)

#The inverse Simpson (1/D)
invsimpson <- diversity(whale.matrix2, index = "invsimpson")
invsimpson

#Shannon's diversity index 
#This looks at richness and adundance 
shannon <- diversity(whale.matrix2, index = "shannon")
shannon
plot(shannon)
hist(shannon)

#used decades as sites 
rarecurve(whale.matrix2)

#Beta diversity
betadiver(help=TRUE)
#Jaccard's index (similarity) different = 0 
#Similarity between sites, based on richness, not abundance
betadiver(whale.matrix2, method = "j")
#Sorenson's index (similarity) different = 0 
betadiver(whale.matrix2, method = "sor")
#Whittaker's - dissimilarity (different communities get a value of 1)
betadiver(whale.matrix2, method = "w")

#Gamma diversity 
length(unique(reordering2$Species))

#This adds up species where abundance is not NA
length(unique(reordering2$Species[!is.na(Species$n) & reordering2$n != 0]))

#Species accumulation curves 
whale.curve2 <- specaccum(whale.matrix2, method = "random", permutations = 1000)
whale.curve2

plot(whale.curve2, ci.type = "poly", col = "blue", ci.col = "lightblue", 
     lwd = 2, ci.lty = 0, xlab = "Decades: 1913 - 2015", 
     ylab = "Cumulative number of whale species")


#Estimating total richness across years 
#Chao2
specpool(whale.matrix2)


#Rarecurve with species 
reordering3 <- reordering[c("Name.Current.Sci", "n", "Decade")]
whale.matrix3 <- sample2matrix(reordering3)
rarecurve(whale.matrix3)
 














