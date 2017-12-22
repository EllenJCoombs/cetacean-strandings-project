


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



#Linear models of correlation  storms, SST 
model3 <- lm(Storms ~ Max_SST, data = all_strandings)
summary(model3)

m <- lm(POPULATION ~ STRANDINGS, data = pop_vs_strandings)

summary(lm(Max_SST ~ NAO_index, all_strandings))$r.squared
summary(lm(Storms ~ NAO_index, all_strandings))$r.squared


#Plot all species stranding - facet_wrap 
#Using all_strandings 

ggplot() + 
  geom_line(data = all_strandings, aes(x = Year, y = Total_strandings, colour = Species)) + 
  theme_bw() +
  facet_wrap(~ Species, scales = "free")



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



