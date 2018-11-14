


UK_and_Irish_sp <- read.csv("UK_and_Irish_sp.csv") 

#Strandings count per year 1990 - 2015 
#There should be no unkwnowns 
speciesyearcount <- count(UK_and_Irish_sp, Name.Current.Sci, Year) %>%
  na.omit()

#Adding a 0 to each year without a record 
#Min is now 1990
speciesyearcount <- speciesyearcount %>% 
  complete(Year = seq(min(1913), max(2015), 1L), Name.Current.Sci = unique(speciesyearcount$Name.Current.Sci))
#NAs -> 0 
speciesyearcount[is.na(speciesyearcount)] <- 0

#Changing the name of the dataset 
All_strandings <- speciesyearcount 
#Rename n to "Total_strandings"
All_strandings <- All_strandings %>% 
  dplyr::rename(Total_strandings = n)
#Reaname Name.Current.Sci to Species 
All_strandings <- All_strandings %>%
  dplyr::rename(Species = Name.Current.Sci)


Fishing <- read.csv("Fishing_data_UK.csv")
Fishing <- Fishing %>%
  dplyr::rename(Fish_catch = Annual.catches..1000.tonnes.)




All_model <- bind_cols(Population, Storm_data, Geom_mean_max, SST_yearly_max, NAO_index, Fishing)
All_model$X <- NULL
All_model$Year1 <- NULL
All_model$X1 <- NULL
All_model$year <- NULL 
All_model$Year2 <-NULL
All_model$Year <-NULL
All_model$X2 <- NULL
All_model$Year3 <- NULL 

All_model <- All_model %>% 
  dplyr::rename(Year = YEAR) %>%
  dplyr::rename(Population = POPULATION) %>%
  dplyr::rename(Max_SST = year_max) %>%
  dplyr::rename(Fish_catch = Annual.catches..1000.tonnes.)

All_strandings <- full_join(All_strandings, All_model, by = "Year")

All_strand <- gam(Total_strandings ~ offset(log(Population)) +s(Year, Species, bs="fs") +
                        s(Storms, k=7, bs="ts") +
                        s(Max_K_index, k=5, bs="ts") +
                        s(Max_SST, bs="ts") +
                        s(NAO_index, bs="ts") +
                        s(Fish_catch, bs="ts"),
                      data= All_strandings, 
                      method = "REML",
                      family=quasipoisson())


#GAM summary and GAM plots 
summary(All_strand)
par(mfrow = c(2,2))
plot(All_strand)

#Gam.check
par(mfrow=c(2,2))
gam.check(All_strand)

#family=tw(a=1.2))
unique(All_model$Fish_catch)
min(All_model$Fish_catch)
max(All_model$Fish_catch)


#Residuals plots for fish
Fish_bins <- cut(All_strandings$Fish_catch, c(290000, 750000, 1000000, 2000000, 3000000, 
                                              400000, 500000, 6500000))
Fish_resid_data <- data.frame(Fish_catch=Fish_bins, resids=residuals(All_strand))
boxplot(resids~Fish_catch, data=Fish_resid_data)



