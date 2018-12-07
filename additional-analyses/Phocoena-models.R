

## Investigating the effects of harbour porpoises (Phocoena phocoena)
#This code shows which of the datapoints are harbour porpoises 


#This next chunk of code looks at point specifics to determine outliers (in this case
#it led to removing harbour porpoises) 

#Having a look at the response plot (strandings)
par(mfrow=c(1,1))
fitted_A <- fitted(All_strandc)
response_A <-  All_strandc$y
plot(fitted_A[response_A<50], response_A[response_A<50], pch=19, cex=0.2, asp=1)
abline(a=0,b=1)

#Using identify
identify(fitted_A[response_A<50], response_A[response_A<50])

#Pick out any remaining outliers if needed: 
#Example: all_strandings[response_A<50,][1894,]#Phocoena phocoena 

#Highlighting phocoena as possible outliers (can do this with any species)
plot(fitted_A, response_A, pch=19, cex=0.2, asp=1)
points(fitted_A[all_strandings$Species=="Phocoena phocoena"], 
       response_A[all_strandings$Species=="Phocoena phocoena"], pch=19, cex=0.5, col="red")
abline(a=0,b=1)




## Running GAMs for data with Phocoena phocoena (Harbour porpoise) removed 
#This code runs the GAMs without harbour porpoises and then tidies up the output with `broom`


#Removing Harbour porpoise from the dataset 
# ! keeps everything but phocoena
No_phocoena <- all_strandings %>%
  filter(Species != "Phocoena phocoena")

No_phocoena_c1 <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                        s(Storms, k=7, bs="ts") +
                        s(Max_K_index, k=4, bs="ts") +
                        s(Max_SST, bs="ts") +
                        s(NAO_index, bs="ts"), 
                      data= No_phocoena, 
                      method= "REML",
                      family=nb())

#Summary and predictor plots 
summary(No_phocoena_c1)
par(mfrow = c(2,2))
plot(No_phocoena_c1)

#Gam.check 
par(mfrow=c(2,2))
gam.check(No_phocoena_c1)


#Plot with no Phocoena 
par(mfrow=c(1,1))
fitted_A <- fitted(No_phocoena_c1)
response_A <-  No_phocoena_c1$y
plot(fitted_A[response_A<200], response_A[response_A<200], pch=19, cex=0.2, asp=1)
abline(a=0,b=1)


#Using broom to tidy 
#Tidy multiple models at once 
Tidy_no_phocoena <- list(No_phocoena1 = No_phocoena1)

#Tidy and glance datasets 
No_phocoena_tidy <- plyr::ldply(Tidy_no_phocoena, tidy, .id = "model")
No_phocoena_glance <- plyr::ldply(Tidy_no_phocoena, glance, .id = "model")

#Save to csv if required 
#write.csv(No_phocoena_tidy, file = "No_phocoena_tidy.csv")
#write.csv(No_phocoena_glance, file = "No_phocoena_glance.csv")



## Running GAMs for data with Phocoena phocoena (Harbour porpoise) only 
#This code runs the GAMs without harbour porpoises and then tidies up the output with `broom`


#Running the models for phocoena only 
phocoena <- all_strandings %>% 
  filter(Species == "Phocoena phocoena")

#No need for species smooth 
Phocoena_c <- gam(Total_strandings ~ offset(log(Population)) +s(Year, bs="ts") +
                    s(Storms, k=7, bs="ts") +
                    s(Max_K_index, k=4, bs="ts") +
                    s(Max_SST, bs="ts") +
                    s(NAO_index, bs="ts"), 
                  data= phocoena, 
                  method= "REML",
                  family=nb())

summary(Phocoena_c)
par(mfrow = c(2,2))
plot(Phocoena_c)

#Gam.check
par(mfrow=c(2,2))
gam.check(Phocoena_c)


#Using broom to tidy 
#Tidy multiple models at once 
Tidy_Phocoena_c <- list(Phocoena_c = Phocoena_c)

#Tidy and glance datasets 
Phocoena_c_tidy <- plyr::ldply(Tidy_Phocoena_c, tidy, .id = "model")
Phocoena_c_glance <- plyr::ldply(Tidy_Phocoena_c, glance, .id = "model")

#Save to csv if required 
#write.csv(Phocoena_c_tidy, file = "Phocoena_c_tidy.csv")
#write.csv(Phocoena_c_glance, file = "Phocoena_c_glance.csv")
