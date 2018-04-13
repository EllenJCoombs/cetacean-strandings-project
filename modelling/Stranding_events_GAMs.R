

library(dplyr)
library(mgcv) #for GAMs


#Read in the stranding counts data 
#This is in the cleaned data folder
Stranding_events_count <- read.csv("Stranding_events_count.csv")

#This now needs to be merged with the Model_data dataset (used in the main analysis) before I
#duplicated the variables variables to represent all species 
#Model_data is in the 'modelling' folder 

Model_data <- read.csv("Model_data.csv")

#Bind Model_data and Stranding_events_count for GAM

Stranding_events_model <- full_join(Stranding_events_count, Model_data, by = "Year")
#Remove duplicated year column 
Stranding_events_model$Year1 <- NULL 

#Run the GAMs as before 
#This still has a species smoooth - it's just not including mass strandings 
Events_GAM <- gam(Total_events ~ offset(log(Population)) +s(Year, Species, bs="fs")+
                s(Storms, k=5, bs="ts") +
                s(Max_K_index, k=4, bs="ts") +
                s(Max_SST, bs="ts") +
                s(NAO_index, bs="ts"), 
              data= Stranding_events_model, 
              method= "REML",
              family=nb)

summary(Events_GAM)
par(mfrow = c(2,2))
plot(Events_GAM) 

#Gam.check
par(mfrow=c(2,2))
gam.check(Events_GAM)
