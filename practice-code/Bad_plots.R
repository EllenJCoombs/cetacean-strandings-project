#This code runs on from the 240517 name cleaning code, i.e rerun that code to
#clean the dataset before doing the below 

# Load libraries/packages
library(dplyr)
library(tidyr)
library(ggplot2)

#Some plots 

select(nhmcsip, Date, Name.Current.Sci)

summary(datespecies)
tbl_df(datespecies)
View(datespecies)
glimpse(byspecies)


#Filtering out phocoena phocoena
filter(nhmcsip, Name.Current.Sci == "phocoena phocoena")
phocoena <- nhmcsip %>% filter(Name.Current.Sci == "phocoena phocoena") 
phocoenadate <- select(phocoena, Name.Current.Sci, Year)
names(phocoenadate) 


sapply(phocoenadate, class)

ggplot(data = phocoenadate, aes(x = Year, y = Name.Current.Sci)) + geom_point()
myplot <- ggplot(data = phocoenadate, aes(x= Year, y = Name.Current.Sci)) +
  myplot + geom_point()


#Histogram of phocoena 
ggplot(phocoenadate, aes(x = Year)) +
  geom_histogram()


#Not sure how to get this to work 
ggplot(phocoenadate,
       aes(y = Name.Current.Sci, x = Year)) +
  geom_point()


#Orca plot 
filter(nhmcsip, Name.Current.Sci == "orcinus orca")
orcinus <- nhmcsip %>% filter(Name.Current.Sci == "orcinus orca") 
orcinusdate <- select(orcinus, Name.Current.Sci, Year)
names(orcinusdate) 

ggplot(orcinusdate, aes(x = Year)) +
  geom_histogram()

nhmcsip$Name.Current.Sci

#Delphinus delphis 
filter(nhmcsip, Name.Current.Sci == "delphinus delphis")
deldelphis <- nhmcsip %>% filter(Name.Current.Sci == "delphinus delphis") 
deldelphisdate <- select(deldelphis, Name.Current.Sci, Year)
names(deldelphisdate) 

ggplot(deldelphisdate, aes(x = Year)) +
  geom_histogram()
