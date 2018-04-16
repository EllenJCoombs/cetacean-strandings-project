

#Habitat GAMs 
#Running GAMs with information on whether species are pelagic, coastal or both 
#Information was taken from Reid et al., 2003 

#Pelagic = most time spent in waters >200m 
#Coastal = most time spent in waters  ≤ 200m 
#Both - where it was stated the species spent time in both habitats equally, or where it was unclear

#Read in the data from the cleaned data folder 
all_strandings <- read.csv("all_strandings.csv")

#Split the data as in the Suborder GAMs code 
#Pelagic, coastal, both

#Strip out coastal species from all_strandings 
coast1 <- filter(all_strandings, Species ==  "Lagenorhynchus acutus") 
coast2 <- filter(all_strandings, Species ==  "Lagenorhynchus albirostris") 
coast3 <- filter(all_strandings, Species ==  "Phocoena phocoena")

#Coastal dataset 
coastal <- bind_rows(coast1, coast2, coast3)

#Strip out the 'both species'
both1 <- filter(all_strandings, Species ==  "Delphinus delphis")
both2 <- filter(all_strandings, Species ==  "Grampus griseus")
both3 <- filter(all_strandings, Species ==  "Orcinus orca")
both4 <- filter(all_strandings, Species ==  "Tursiops truncatus")

#'Both' dataset 
both <- bind_rows(both1, both2, both3, both4)

#Stripping out coastal
pelagic1 <- all_strandings[ !(all_strandings$Species %in% coastal$Species), ]
#stripping out both 
pelagic <- pelagic1[ !(pelagic1$Species %in% both$Species), ]


#Adding an additional habitat column to each of the datasets 
coastal$newcolumn <- "coastal"
both$newcolumn <- "both"
pelagic$newcolumn <- "pelagic"

#bind the three datasets 
all_strandings <- bind_rows(coastal, pelagic, both)

#Rename column 
all_strandings <- all_strandings %>%
  rename(Habitat = newcolumn)

#check factors etc 
sapply(all_strandings, class)

#Habitat needs to be a factor, not a character 
all_strandings$Habitat <- as.factor(all_strandings$Habitat)

#Run GAM with suborder as the smooth 
Habitat_GAM <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Habitat, bs="fs")+
                      s(Storms, k=5, bs="ts") +
                      s(Max_K_index, k=4, bs="ts") +
                      s(Max_SST, bs="ts") +
                      s(NAO_index, bs="ts"), 
                    data= all_strandings, 
                    method= "REML",
                    family=nb)

summary(Habitat_GAM)
par(mfrow = c(2,2))
plot(Habitat_GAM) 

#Gam.check
par(mfrow=c(2,2))
gam.check(Habitat_GAM)

