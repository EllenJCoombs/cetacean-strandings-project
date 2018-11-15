#Running additional analysis 
#Habitat GAMs for species 
#Running GAMs with information on whether species are pelagic, coastal or both 
#Information was taken from Reid et al., 2003 

#Oceanic = most time spent in waters >200m 
#Coastal = most time spent in waters  ≤ 200m 
#Both - where it was stated the species spent time in both habitats equally, or where it was unclear

#Here the same correlates from the main analysis are used in 
#the generalised additive models.

library(dplyr)

#Read in the data from the cleaned data folder 
All_strandings <- read.csv("All_strandings.csv")

#Split the data as in the Suborder GAMs code 
#Pelagic, coastal, both

#Strip out coastal species from all_strandings 
coast1 <- filter(All_strandings, Species ==  "Lagenorhynchus acutus") 
coast2 <- filter(All_strandings, Species ==  "Lagenorhynchus albirostris") 
coast3 <- filter(All_strandings, Species ==  "Phocoena phocoena")

#Coastal dataset 
coastal <- bind_rows(coast1, coast2, coast3)

#Strip out the 'both species'
both1 <- filter(All_strandings, Species ==  "Delphinus delphis")
both2 <- filter(All_strandings, Species ==  "Orcinus orca")
both3 <- filter(All_strandings, Species ==  "Tursiops truncatus")
both4 <- filter(All_strandings, Species ==  "Balaenoptera acutorostrata")
both5 <- filter(All_strandings, Species ==  "Megaptera novaeangliae")


#'Both' dataset 
both <- bind_rows(both1, both2, both3, both4, both5)

#Stripping out coastal
oceanic <- All_strandings[ !(All_strandings$Species %in% coastal$Species), ]
#stripping out both 
oceanic2 <- oceanic[ !(oceanic$Species %in% both$Species), ]


#Adding an additional habitat column to each of the datasets 
coastal$newcolumn <- "coastal"
both$newcolumn <- "both"
oceanic2$newcolumn <- "oceanic"

#bind the three datasets 
All_strandings <- bind_rows(coastal, oceanic2, both)
#Rename oceanic2 = oceanic 
All_strandings$newcolumn[All_strandings$newcolumn %in% "oceanic2"] <- "oceanic" 

#Rename column 
All_strandings <- All_strandings %>%
  dplyr::rename(Habitat = newcolumn)

#check factors etc 
sapply(All_strandings, class)

#Habitat needs to be a factor, not a character 
All_strandings$Habitat <- as.factor(All_strandings$Habitat)

All_model <- bind_cols(Population, Storm_data, Geom_mean_max, SST_yearly_max, NAO_index, Fishing)
All_model$X <- NULL
All_model$Year1 <- NULL
All_model$X1 <- NULL
All_model$year <- NULL 
All_model$Year2 <-NULL
All_model$Year <-NULL
All_model$X2 <- NULL
All_model$Year3 <- NULL 

All_model <- All_model %>% 
  dplyr::rename(Year = YEAR) %>%
  dplyr::rename(Population = POPULATION) %>%
  dplyr::rename(Max_SST = year_max) %>%
  dplyr::rename(Fish_catch = Annual.catches..1000.tonnes.)

All_strandings <- full_join(All_strandings, All_model, by = "Year")

#Run GAM with suborder as the smooth 
Habitat_GAM <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Habitat, bs="fs")+
                     s(Storms, k=5, bs="ts") +
                     s(Max_K_index, k=4, bs="ts") +
                     s(Max_SST, bs="ts") +
                     s(NAO_index, bs="ts") +
                     s(Fish_catch, bs="ts"), 
                   data= All_strandings, 
                   method= "REML",
                   family=tw(a=1.2))

summary(Habitat_GAM)
par(mfrow = c(2,2))
plot(Habitat_GAM) 

#Gam.check
par(mfrow=c(2,2))
gam.check(Habitat_GAM)
