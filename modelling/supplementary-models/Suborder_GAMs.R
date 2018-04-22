#Running additional analyses
#Adding suborder and using this a factor smooth instead of species 
#This is using the all_strandings model data 

#Need to split by odontocetes and mysticetes first 
#Add a suborder column
#Rebind 

#this dataset is in the cleaned data file 
all_strandings <- read.csv('all_strandings.csv')

#Strip out the mysticetes from all_strandings data 
myst1 <- filter(all_strandings, Species ==  "Balaenoptera physalus") 
myst2 <- filter(all_strandings, Species ==  "Balaenoptera acutorostrata") 
myst3 <- filter(all_strandings, Species ==  "Balaenoptera borealis")
myst4 <- filter(all_strandings, Species ==  "Balaenoptera musculus") 
myst5 <- filter(all_strandings, Species == "Megaptera novaeangliae") 

#Make mysticete dataset 
mysticetes <- bind_rows(myst1, myst2, myst3, myst4, myst5)

#Now add a column on suborder 
mysticetes$newcolumn <- "mysticete"

#Strip out the odontocetes from the original dataset 
#Selecting out odontocetes from main dataset and then delete Mysticetes - this is easier than
#picking out all 16 odontocetes individually 
odontocetes <- all_strandings[ !(all_strandings$Species %in% mysticetes$Species), ]

#Now add a column on suborder 
odontocetes$newcolumn <- "odontocete"

#bind the two datasets 
all_strandings <- bind_rows(mysticetes, odontocetes)

#Change the name for the column 
all_strandings <- all_strandings %>%
  rename(Suborder = newcolumn)

#Save the new all_strandings dataset 
#This is in 'cleaned data'
all_strandings <- write.csv(all_strandings, file = "all_strandings.csv")


#check factors etc 
sapply(all_strandings, class)

#Suborder needs to be a factor, not a character 
all_strandings$Suborder <- as.factor(all_strandings$Suborder)


#Run GAM with suborder as the smooth 
Suborder_GAM <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Suborder, bs="fs")+
                    s(Storms, k=5, bs="ts") +
                    s(Max_K_index, k=4, bs="ts") +
                    s(Max_SST, bs="ts") +
                    s(NAO_index, bs="ts"), 
                  data= all_strandings, 
                  method= "REML",
                  family=nb)

summary(Suborder_GAM)
par(mfrow = c(2,2))
plot(Suborder_GAM) 

#Gam.check
par(mfrow=c(2,2))
gam.check(Suborder_GAM)



