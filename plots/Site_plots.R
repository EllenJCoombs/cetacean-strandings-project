

library(dplyr)
library(ggplot2)
library(tidyr)


site <- read.csv("Site1.csv")


#Count of species per year 
#Strandings count per year 1991 - 2015 
#There should be no unkwnowns 
sitecount <- count(site, Code, Year) %>%
  na.omit()

sitecount <- sitecount %>% 
  complete(Year = seq(min(1913), max(2015), 1L), Code = unique(sitecount$Code))
#NAs -> 0 
sitecount[is.na(sitecount)] <- 0

sitecount <- sitecount %>%
  arrange(Code)

sitecount <- sitecount %>%
  rename(strandings_count = n)

#Plots per site 
ggplot(sitecount, aes(x = Year, y = strandings_count)) +
  geom_bar(stat="identity", width=0.5) +
  facet_wrap(~ Code) +
  theme_classic() +
  labs(x="Year",y="Stranding count")



                