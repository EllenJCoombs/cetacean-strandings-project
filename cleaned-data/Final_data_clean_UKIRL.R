#Final clean of the data 
#Making sure all unknowns are removed 
#Removing rare species e.g Belugas, Narwhal, Melon-headed 

library(dplyr)

UK_and_Irish <- read.csv("UK_and_Irish_strandings.csv")

UK_and_Irish_known <- UK_and_Irish %>%
  filter(!(Name.Current.Sci %in% c("Unknown", "Unknown odontocete", "Unknown odontocete ", "Unknown delphinid ",
                                   "Unknown delphinid", "Unknown delphinid ", "Unknown mysticete")))


#Removing rare species 
UK_and_Irish_sp <- UK_and_Irish_known %>%
  filter(!(Name.Current.Sci %in% c("Monodon monoceros", "Peponocephala electra", "Delphinapterus leucas", "Kogia sima",
                                   "Mesoplodon densirostris", "Mesoplodon europaeus", "Lagenodelphis hosei")))

#This is the final cleaned dataset with unknowns removed and rare species removed
write.csv(UK_and_Irish_sp, file = "UK_and_Irish_sp.csv")
