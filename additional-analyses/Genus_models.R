
#Genus instead of species models 
#Need to split species column 

#For whole datset 
#Then for 1990s onwards 

library(dplyr)
UK_and_Irish_genus <- UK_and_Irish_sp %>% 
  separate(Species, c("Genus", "Species"))

UK_and_Irish_sp$Name.Current.Sci <- as.character(UK_and_Irish_sp$Name.Current.Sci)


UK_and_Irish_genus <- UK_and_Irish_sp %>%
separate(UK_and_Irish_sp, Name.Current.Sci, into = c("Species", "Genus"), sep = 2)

UK_and_Irish_genus <- UK_and_Irish_sp %>%
separate(data = UK_and_Irish_sp, col = Name.Current.Sci, into = c("left", "right"))


df <- data.frame(do.call('rbind', strsplit(as.factor(UK_and_Irish_sp$Name.Current.Sci),'|',fixed=TRUE)))


sapply(UK_and_Irish_sp, class)

install.packages('reshape')
library(reshape)
df = transform(UK_and_Irish_sp, Name.Current.Sci = colsplit(Name.Current.Sci, split = "\\|", names = c('Species', 'Genus')))