

Fisheries_data <- read.csv("Landings.csv")

#Country codes 
Fisheries_data <- Fisheries_data %>%
  filter(Country %in% c("GBE", "GBU", "GBS", "IRL"))

Fisheries_data$X <- NULL
Fisheries_data$X.1 <- NULL
Fisheries_data$X.2 <- NULL
Fisheries_data$X.3 <- NULL 
Fisheries_data$X.4 <-NULL 

#Years 
Fisheries_data <- Fisheries_data %>%
  filter(Year %in% c(1913:2015))


Fisheries_data <- Fisheries_data %>%
  select(Year, Landings)

Fisheries_data$Landings = as.numeric(as.character(Fisheries_data$Landings))
#NAs -> 0 
Fisheries_data[is.na(Fisheries_data)] <- 0

#Aggrgate by year 
Fisheries_data <- aggregate(Fisheries_data$Landings, by=list(Year=Fisheries_data$Year), FUN=sum)

write.csv(Fisheries_data, file = "Fisheries_data.csv")

