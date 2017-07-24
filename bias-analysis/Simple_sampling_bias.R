
library(tidyverse) 

#Having a look at basic sampling effort: dividing species by the total number of 
#records for that year 

#Saving the dataset with the n column (number of total species)

rename(totalyear, Total = n)
write.csv(totalyear, file = "Data_with_totals.csv")


#Using the cleaned dataset 
data <- read.csv("cleandatesnames.csv") 

#Looking at species by year - a count of each species per year
speciesbyyear <- aggregate(n ~ Name.Current.Sci, speciesyearcount, sum)
View(speciesbyyear)
#Arranging species by year by count 
speciesbyyear <- speciesyearcount %>%
  group_by(n, Year, Name.Current.Sci) %>%
  arrange(Year)

#Total each of the years - this is using the totalcount dataset 
totalcount <- speciesyear %>% count("Year") 

ggplot(data = speciesyear, aes(x = Year)) +
         geom_histogram(binwidth = 0.5) +
  labs(x = "Year", y = "Total count") 


#Creating a data table of species, total per year and basic sampling effort 
library(dplyr)
yearoverview <- speciesyearcount %>% left_join(totalcount, by="Year") %>%
  arrange(Year) %>%
  mutate(sampling = n/freq)

library(readr)
library(ggplot2) 

#ugly graph of sampling effort for all species 
ggplot(data = yearoverview, aes(x = Year, y = sampling, color = Name.Current.Sci)) +
  geom_line() +
  xlim(1913, 2017) +
  theme_bw() +
  labs(title = "sampling effort")

#Sampling effort for phocoena phocoena 
#Looking at Phocoena with basic sampling effect 
speciesyearcount %>% 
  filter(Name.Current.Sci == "phocoena phocoena")

pphocoenasampling <- yearoverview[yearoverview$Name.Current.Sci == "phocoena phocoena",]
ggplot(data = pphocoenasampling, aes(x = Year, y = sampling)) +
  geom_line() +
  xlim(1913, 2017) +
  theme_bw() +
  labs(title = "sampling effort")



##########################################################################################
sampling_list <- select(yearoverview, Name.Current.Sci, sampling) 

#Basic sampling effort for each species 
ggplot(yearoverview, aes(x = Year, y = sampling)) +
  geom_line() +
  xlim(1913, 2017) +
  theme_bw() + 
  labs(title = "Basic sampling effort for each species") +
  facet_wrap(~ Name.Current.Sci)

##########################################################################################

#plotting using the cleaned Data_with_totals dataset 
#Should use cleaned data as I haven't cleaned all of the names yet 
mydata <- read.csv("data_with_totals.csv")

total.sampling <-
  mydata %>%
  group_by(Year) %>%
  summarise(total.per.year = length(!is.na(Name.Current.Sci)))

species.sampling <- 
  mydata %>%
  group_by(Year, Name.Current.Sci) %>%
  summarise(records = length(!is.na(Name.Current.Sci))) 

sampling.effort <- full_join(total.sampling, species.sampling, by = "Year")

sampling.effort <- all.data %>%
  mutate(records.sampling = records/total.per.year)


ggplot(sampling.effort, aes(x = Year, y = records.sampling)) +
  geom_line() +
  xlim(1913, 2017) +
  theme_bw() + 
  labs(title = "Basic sampling effort for each species") +
  facet_wrap(~ Name.Current.Sci)


#Splitting out individual species 
ggplot(filter(sampling.effort, Name.Current.Sci == "phocoena phocoena"), aes(x = Year, y = records.sampling)) +
  geom_line() +
  xlim(1913, 2017) +
  theme_bw() + 
  labs(title = "Basic sampling effort for each species") +
  facet_wrap(~ Name.Current.Sci)



