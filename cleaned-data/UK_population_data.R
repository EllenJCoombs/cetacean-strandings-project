#Looking at UK population as a means of assessing sampling effort 
#Step one is to see how regional data compares to total population data
#I.e. can I used total population data, or would that not reflect regional changes? 

library(dplyr)

#Read in the proto-population data (need to make this more detailed)
library(reshape2)
library(mvbutils)
#Check.names removes the X that randomly appeared (not sure what that was)
population_data <- read.csv("Population_data.csv", check.names = FALSE)

#Arranging data in to years, locality and population 
population_data$X <- NULL
population_data <- melt(population_data, measure.vars=names(population_data)%except%"Name")

population_data <- population_data %>% 
  rename(Year = variable) %>%
  rename(Population = value) 

population <- population_data$Population 

#Transforming population to numeric (was in "")
population <- gsub(",","", population)
population_data <- mutate(population_data, Population = as.numeric(Population))

as.numeric <- population

#Checking that it's numeric 
sapply(population_data, class)
#Log transformation 

#plotting population data 
#Need to log the population data 
ggplot() + 
  geom_line(data = population_data, aes(x = Year, y = Population, colour = Name, group = factor(Name))) +
  labs(x = "Year", y = "Population") + 
  geom_smooth (method=lm, se=FALSE, colour = "grey70", size =0.7)
  #coord_cartesian(ylim = c(1,000,000, 60,000,000))
  

#Doesn't show much - split out the data fpr larger and smaller populations and put on a
#seperate y-axis 
large_pops <- population_data %>% 
  filter(Name %in% c("UNITED KINGDOM", "ENGLAND", "ENGLAND & WALES", "GREAT BRITAIN"))

#Not sure why the below didn't work 
#small_pops <- population_data[ !(population_data$Name %in% large_pops$Name), ]

small_pops <- population_data %>% 
  filter(Name %in% c("EAST", "EAST MIDLANDS", "LONDON", "NORTH EAST", "NORTH WEST", "NORTHERN IRELAND",
                     "SCOTLAND", "SOUTH EAST", "SOUTH WEST", "WALES", "WEST MIDLANDS", "YORKSHIRE AND THE HUMBER"))


#Plot both with two different axis
ggplot() + 
  geom_line(data = small_pops, aes(x = Year, y = Population, colour = "grey")) +
  geom_line(data = large_pops, aes(x = Year, y = Population/100, colour = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Regional population")) +
  labs(y = "Total Population",
       x = "Year",
       colour = "Parameter") 




