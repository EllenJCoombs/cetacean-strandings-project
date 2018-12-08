

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


#counting total strandings per year 
gg1 <- ggplot(data=sitecount, aes(x=Year, y= strandings_total)) +
  geom_bar(stat="identity", width=0.5)
  
  
gg1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


ggplot(sitecount, aes(x = Year, y = strandings_count)) +
  geom_bar(stat="identity", width=0.5) +
  facet_wrap(~ Code) +
  theme_light() 



                