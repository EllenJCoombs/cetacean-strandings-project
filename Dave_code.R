
# some point before you get the data in you need to strip out the .blah from the
# CSIP and use this magic to do that
#sub("\\.\\d+$", "", "id123.45")


cleaneddata <- read.csv("cleandatesnames.csv")


thingo <- cleaneddata
thingo$X <- NULL
thingo <- thingo[duplicated(thingo), ]

# only have popn data from 1991, let's do that for now
thingo <- subset(thingo, thingo$Year>=1991 & thingo$Year<2010)

library(readr)
population <- read_csv("Population_data.csv")
uk_pop <- population[1,]
uk_pop <- uk_pop[,-1]

thingo$population <- as.numeric(uk_pop[match(thingo$Year, names(uk_pop))])

# build richness column
#Using code from "Total_species_plots.R"


speciesyearcount <- dplyr::count(thingo, Name.Current.Sci, Year) %>%
  na.omit() %>%
  arrange(Year)


speciesyearcount <- aggregate(speciesyearcount$n, by = speciesyearcount[c('Year')], length)

thingo_b <- thingo %>%
  select(Year, Latitude, Longitude, population)


thingo_b <- thingo_b %>%
  group_by(Year) %>%
  summarize(mean_Lat = mean(Latitude, na.rm = TRUE), 
  mean_Long = mean(Longitude, na.rm = TRUE),
  popn = mean(population, na.rm = TRUE))


thingo_b <- bind_cols(thingo_b, speciesyearcount) %>%
  rename(richness = x) 

thingo_b$Year1 <- NULL




library(mgcv)

b <- gam(richness ~ offset(log(popn)) + s(mean_Lat) + s(mean_Long), data=thingo_b, method = "REML", family=poisson())

plot(b)

summary(b)


b_t <- gam(richness ~ offset(log(popn)) + s(Year), data=thingo_b, method = "REML", family=poisson())

plot(b_t)

summary(b_t)
