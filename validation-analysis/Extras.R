

library(ggplot2)

csip_ireland <- csip %>% 
  filter(Country %in% c("Ireland"))


nhm_ireland <- nhm 

write.csv(storms, file = "Storm_data.csv")

phocoena <- UK_and_Irish %>%
  filter(Name.Current.Sci == "Phocoena phocoena") 

ddelphis <- UK_and_Irish %>%
  filter(Name.Current.Sci == "Delphinus delphis")

zcavirostris <- UK_and_Irish %>%
  filter(Name.Current.Sci == "Ziphius cavirostris") 


#Need to add two years - 1955 and 1958 had no mysticete strandings 
mysticetescount <- mysticetescount %>% 
  complete(Year = seq(min(1913), max(2015), 1L))

#NAs -> 0 
mysticetescount[is.na(mysticetescount)] <- 0

#Counts of each 
mysticetescount <- count(combinedmysticetes, Year)
odontocetescount <- count(odontocetes, Year)

#Mysticetes plot count 
ggplot(data = mysticetescount, aes(x = Year, y = n)) +
  geom_line() +
  geom_point()

#Odontocetes plot count 
ggplot(data = odontocetescount, aes(x = Year, y = n)) +
  geom_line() +
  geom_point()

#arranging by year
sortedmysticetes <- arrange(combinedmysticetes,(Year))
#NAs -> 0 
sortedmysticetes[is.na(sortedmysticetes)] <- 0

#Line plot of odontocetes 
ggplot() +
  geom_line(data = odontocetescount, aes(x = Year, y = n)) +
  theme_bw()

#Playing around with seperate species 
lalbirostris <- UK_and_Irish_sp %>%
  filter(Name.Current.Sci == "Lagenorhynchus albirostris")


ggplot() + 
  geom_line(data = lalbirostris, aes(x = Year, y = Latitude))+
  theme_bw()


pmacrocephalus_strandings <- UK_and_Irish_sp %>%
  filter(Name.Current.Sci == "Physeter macrocephalus")



#Linear models of correlation  NAO, SST 
NAO_SSTmodel <- lm(NAO_index ~ Max_SST, data = all_strandings)
summary(NAO_SSTmodel)

##Linear models of correlation  NAO, storms 
NAO_stormsmodel <- lm(NAO_index ~ Storms, data = all_strandings)
summary(NAO_stormsmodel)

#Double checking the R-squared 
summary(lm(Max_SST ~ NAO_index, all_strandings))$r.squared
summary(lm(Storms ~ NAO_index, all_strandings))$r.squared


m <- lm(POPULATION ~ STRANDINGS, data = pop_vs_strandings)




#Plot all species stranding - facet_wrap 
#Using all_strandings 

ggplot() + 
  geom_line(data = all_strandings, aes(x = Year, y = Total_strandings, colour = Species)) + 
  theme_bw() +
  labs(y = "Total strandings",
       x = "Year") +
  facet_wrap(~ Species, scales = "free") 


?par


scoeruleoalba <- UK_and_Irish_sp %>%
  filter(Name.Current.Sci == "Stenella coeruleoalba")

S_coeruleoalba_wlat50 <- scoeruleoalba %>%
  filter(Latitude >50)


#Filtering out snd plotting orcas
#Or whichever species 
oorca <- UK_and_Irish_sp %>%
  filter(Name.Current.Sci == "Orcinus orca")

orcacount <- oorca %>%
  count(Year)

ggplot() +
  geom_line(data = orcacount, aes(x = Year, y = n)) +
  theme_bw()


#Total count of all strandings per year 
All_count <- UK_and_Irish_sp %>%
  count(Year)

#Plot the above 
ggplot() + 
  geom_line(data = All_count, aes(x = Year, y = n)) +
  theme_bw()


#Quick plot of SST 
ggplot() + 
  geom_line(data = SST_yearly_max, aes(x = year, y = Year_max)) +
  labs(y = "Maximum yearly SST ("~degree~"C)",
       x = "Year") +
  theme_bw() 

#Quick plot for storms 
ggplot() + 
  geom_line(data = storms, aes(x = Year, y = Storms)) +
  labs(y = "Yearly storm count",
       x = "Year") +
  theme_bw() 


#NAO plot 
ggplot() + 
  geom_line(data = NAO_data, aes(x = Year, y = NAO_index)) +
  labs(y = "NAO index",
       x = "Year") +
  theme_bw() 


#Geomagnetic plot 
ggplot() + 
  geom_line(data = Final_geom, aes(x = Year, y = Max_K_index)) +
  labs(y = "Geomagnetic K-index",
       x = "Year") +
  theme_bw() 

#Population 
ggplot() + 
  geom_line(data = Population, aes(x = Year, y = Population)) +
  labs(y = "UK population (millions)",
       x = "Year") +
  theme_bw() 


#To find out which versio of a package 
packageVersion("mgcv")
#Version of R
getRversion()


Population <- read.csv("Population_UK.csv")

Population <- Population %>%
  rename(Year = YEAR) %>%
  rename(Population = POPULATION)


#Counts of each species for supplementary data 
#This gives species per year 
Species <- UK_and_Irish_sp %>%
  count(Name.Current.Sci, Year)

#Total count 
Species <- UK_and_Irish_sp %>%
  count(Name.Current.Sci)





