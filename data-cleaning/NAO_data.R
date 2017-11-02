#NAO data from the Hurrell 
#Saved in Raw data 
#Interpeting NAO data: difference of atmospheric pressure at sea level (SLP) between the Icelandic low and the Azores high
# a positive NAO phase results in stronger-than-average westerly-winds across northern mid-latitudes, which affects
#both marine and terrestrial ecosystems. 

library(dplyr)
library(ggplot2)
NAO_data <- read.csv("NAO_raw_data.csv") 

NAO_data <- NAO_data %>% 
  dplyr::rename(NAO_index = Station.Based.Annual.NAO.Index) %>% 
  dplyr::rename(Year = Hurrell)

NAO_data <- NAO_data %>%
  dplyr::filter(Year %in% c(1913:2015))


write.csv(NAO_data, file = "NAO_data.csv")


ggplot() + 
  geom_line(data = NAO_data, aes(x = Year, y = NAO_index)) + 
  theme_bw()