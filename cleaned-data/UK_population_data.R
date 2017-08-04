#Looking at UK population as a means of assessing sampling effort 
#Step one is to see how regional data compares to total population data
#I.e. can I used total population data, or would that not reflect regional changes? 

library(dplyr)

#Read in the proto-population data (need to make this more detailed)
library(reshape2)
library(mvbutils)
library(ggplot2)
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


#Plot both with two different axis ###NEED TO FINISH###
ggplot() + 
  geom_line(data = small_pops, aes(x = Year, y = Population, colour = "grey")) +
  geom_line(data = large_pops, aes(x = Year, y = Population/100, colour = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Regional population")) +
  labs(y = "Total Population",
       x = "Year",
       colour = "Parameter") 


########################################################################################
library(ggplot2)
library(dplyr)
#Comparing UK population with UK county data 

uk_counties <- read.csv("Country_county_population.csv", header = TRUE)

#Cleaning up the data quickly 
uk_counties <- uk_counties %>%
  select("YEAR", "UK", "CORNWALL", "DEVON", "HAMPSHIRE", "KENT", "CEREDIGION", "NORFOLK", "WEST.LANCASHIRE",
         "NORTHUMBERLAND", "ARGYLL", "HIGHLAND") 


uk_counties <- uk_counties %>% 
  filter(YEAR %in% c(1991:2009))

#Having a look at county fluctuations compared to UK 
install.packages("Rmisc")
library(Rmisc) #for the multiplot function 

c1 <- ggplot(data = uk_counties, aes(x = CORNWALL, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Cornwall population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


c2 <- ggplot(data = uk_counties, aes(x = DEVON, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Devon population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 
  

c3 <- ggplot(data = uk_counties, aes(x = HAMPSHIRE, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Hampshire population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


c4 <- ggplot(data = uk_counties, aes(x = KENT, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Kent population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


c5 <- ggplot(data = uk_counties, aes(x = CEREDIGION, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Ceredigion population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


c5 <- ggplot(data = uk_counties, aes(x = NORFOLK, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Norfolk population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


c6 <- ggplot(data = uk_counties, aes(x = WEST.LANCASHIRE, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "West Lancashire population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


c7 <- ggplot(data = uk_counties, aes(x = NORTHUMBERLAND, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Northumberland population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


c8 <- ggplot(data = uk_counties, aes(x = ARGYLL, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Argyll population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 
  

c9 <- ggplot(data = uk_counties, aes(x = HIGHLAND, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Highland population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


multiplot(c1, c2, c3, c4, c5, c6, c7, c8, c9, cols = 3) 
  

###################################################################################
#Ireland, Scotland and Wales (more)

extra_counties <- read.csv("Ireland_Scot_Wales.csv")

d1 <- ggplot(data = extra_counties, aes(x = KERRY, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Kerry population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


d2 <- ggplot(data = extra_counties, aes(x = SLIGO, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Sligo population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


d3 <- ggplot(data = extra_counties, aes(x = INVERCLYDE, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Inverclyde population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 


install.packages("ggpsmisc") #Can't install this

d4 <- ggplot(data = extra_counties, aes(x = ANGELSEY, y = UK)) +
  geom_point(size = 0.5) +
  labs(x = "Angelsey population", y = "UK population") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) +
  geom_text(aes(x = ANGELSEY, y = UK, label = lm_eqn(lm(y ~ x, df))), parse = TRUE)

d4

multiplot(d1, d2, d3, d4, cols = 2) 

##########################################################################################
#Taking a look at full century data vs. total strandings

#Bring in the data 
pop_vs_strandings <- read.csv("Population_vs_stranding.csv")

#Plot of UK population and UK strandings 
p1 <- ggplot(data = pop_vs_strandings, aes(x = POPULATION, y = STRANDINGS)) +
  geom_point(size = 0.5) +
  labs(x = "UK population (millions)", y = "Stranding count") +
  geom_text(aes(label = YEAR), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 

#Adding a regression line 
m <- lm(pop_vs_strandings$POPULATION ~ pop_vs_strandings$STRANDINGS)
a <- signif(coef(m)[1])
b <- signif(coef(m)[2])
textlab <- paste("y = ",b,"x + ",a, sep="")

p2 <- p1 + geom_smooth(method = lm, formula = y~x) 

p3 <- p2 + geom_text(aes(x = 50, y = 700, label = textlab), color="black", size= 3, parse = FALSE)  

plot(p3)


















