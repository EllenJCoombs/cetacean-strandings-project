
#Species richness using the vegan package 
install.packages("vegan")
library(vegan)

#for altering my data into presence/absence???
install.packages("picante")
library(picante)

cleaneddata <- read.csv("cleandatesnames.csv")

#Need to reorder the data...?
reordering <- speciesbyyear[c("Year", "n", "Name.Current.Sci")]
whale.matrix <- sample2matrix(reordering)

#Number of species per year 
specnumber(whale.matrix)

#Alpha diversity 
#Simpson's diversity index 
diversity(whale.matrix, index = "simpson")

#Shannon's diversity index 
diversity(whale.matrix, index = "shannon")

rarecurve(whale.matrix)

#Beta diversity
betadiver(help=TRUE)
betadiver(whale.matrix, method = "j")
betadiver(whale.matrix, method = "sor")
betadiver(whale.matrix, method = "w")

#Gamma diversity 
length(unique(speciesbyyear$Name.Current.Sci))

#This adds up species where abundance is not NA
length(unique(speciesbyyear$Name.Current.Sci[!is.na(speciesbyyear$n) & speciesbyyear$n != 0]))

#Species accumulation curves 
whale.curve <- specaccum(whale.matrix, method = "random", permutations = 1000)
whale.curve

plot(whale.curve, ci.type = "poly", col = "blue", ci.col = "lightblue", 
     lwd = 2, ci.lty = 0, xlab = "Years: 1913 - 2015", 
     ylab = "Cumulative number of whale species")



#PLaying around with permutations 
#whale.curve1 <- specaccum(whale.matrix, method = "random", permutations = 1)
#whale.curve2 <- specaccum(whale.matrix, method = "random", permutations = 1)

#par(mfrow = c(1,2))

#plot(whale.curve1,  
     #xlab = "number of sites", ylab = "Cumulative number of whale species")

#plot(whale.curve2, 
     #xlab = "number of sites", ylab = "Cumulative number of whale species")

#par(mfrow = c(1,1))


dev.off()

#Species per year (this incorporates NAs)
total_speciesbyyear <- aggregate(speciesbyyear$n, by = speciesbyyear[c('Year')], length)
rarecurve(total_speciesbyyear)

#plot of total species per year 
ggplot(data = total_speciesbyyear, aes(x = Year, y = x)) +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "grey40", fill = NA)) +
  labs(x = "Year", y = "Species count") +
  geom_smooth() +
  geom_line()


#Estimating total richness across years 
#Chao2
specpool(whale.matrix)
#Species     chao  chao.se    jack1 jack1.se    jack2     boot  boot.se   n
#All            33 35.64078 3.455292 36.96117 2.429669 37.97059 35.00546 1.346503 103










