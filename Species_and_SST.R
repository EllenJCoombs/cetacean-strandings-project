#Splitting data into monthly/quarterly time slots 

#Using the cleaneddata dataset 
library(dplyr)
library(ggplot2)
library(lubridate)


cleaneddata <- read.csv("cleandatesnames.csv")
sapply(cleaneddata, class)

#Having problems with the data format - need dates to be 'date' class
#cleaneddata <- mutate(cleaneddata, Date = dmy(Date))
#Trying this instead - works by changing all 2000s to 1900s
#cleaneddata <- mutate(cleaneddata, Date = format(as.Date(Date, "%d-%b-%y"), "19%y-%m-%d"))

#write.csv(cleaneddata, file = "cleandatesnames.csv")

#Now Date should be 'Date' class 

#Selecting the chosen area (if required)
bactorostrata <- cleaneddata %>% 
  filter(Name.Current.Sci == "Balaenoptera acutorostrata") 

sapply(bactorostrata, class)
#Use a variation of the following code if you want to select a specfic window 
#nw_window <- nw_scotland_strandings %>% 
  #filter(Year %in% c(1989:2000))

#Shows this datset by year but I need it by month.....
#ggplot(nw_window, aes(x = Date)) + 
  #stat_count(width = 0.5) 

ba_window <- bactorostrata %>%
  filter(Year %in% c(1913:2015))

library(zoo)
library(dplyr)

#Splitting my data into strandings per month 
ba_window$monthYear <- as.Date(as.yearmon(ba_window$Date))
ba_window$quarterYear <- as.Date(as.yearqtr(ba_window$Date))
head(ba_window)
#Strandings gathered into months 
ba_monthly <- head(ba_window %>% group_by(monthYear) %>% summarise(n = n()), 516)
#Strandings per quarter 
ba_quarter <- head(ba_window %>% group_by(quarterYear) %>% summarise(n = n()), 324)

#Grouping by week number - not really needed - not running properly
#pp_window$week <- as.Date("1913-01-01")+7*trunc((pp_window$joinTimestamp / 1000)/(3600*24*7))

#Line plot of strandings per month (grouped)
#Have changed monthYear to Date 
ggplot(data = ba_monthly, aes(x = monthYear, y = n, group=1)) +
  geom_line()

ggplot(ba_monthly, aes(n)) +
  geom_histogram(binwidth = 0.5)
  

##########################################################################################
#Plotting species against temperature 
#Example here is Phocoena monthly strandings against STT monthly temps (UK averaged 
#from 14 locations)

#Monthly stranding data is from the above code 
#SST is from the HadISST_Uk.R code 

UK_Ireland_SST <- read.csv("UK_Ireland_SST.csv")

#Renaming time to Date 
UK_Ireland_SST <- UK_Ireland_SST %>%
  dplyr::rename(Date = time)

#Selecting only the UK_mean 
UK_mean_SST <- UK_Ireland_SST %>%
  select(Date, UK_mean) 

#Selecting only 1913:2015
#Need to make Data a date first 
UK_mean_SST <- mutate(UK_mean_SST, Date = ymd(Date))
sapply(UK_mean_SST, class)

write.csv(UK_mean_SST, file = "UK_mean_SST.csv")

#Filtering out specific dates 
UK_mean_SST <- UK_mean_SST %>%
  filter(Date >= "1912-12-16", Date <= "2016-01-16")


#Plotting UK mean temperature 
#Need to play with the months!
bb1 <- ggplot() + 
  geom_line(data = UK_mean_SST, aes(x = Date, y = UK_mean, group = 1)) + 
  #geom_smooth(se = FALSE) +
  #scale_colour_gradient(low='yellow', high='#de2d26') +
  labs(y = "SST",
       x = "time") +
  ylim(0,17) +
  ggtitle("Daily measured sea-surface-temperature, 1870 - 2017",   
          subtitle = "UK mean") + 
  theme_classic()

bb1

#Plotting strandings against temp 

#Changing "monthYear" to "Date" in species data 
ba_monthly <- ba_monthly %>%
  dplyr::rename(Date = monthYear)
  
  
#PPhocoena monthly strandings and monthly SST
  ggplot() + 
    geom_line(data = UK_mean_SST, aes(x = Date, y = UK_mean)) + 
    geom_line(data = ba_monthly, aes(x = Date, y = n*10)) +
    scale_y_continuous(sec.axis = sec_axis(~./10, name = "Monthly strandings")) +
    labs(y = "SST ("~degree~"C)",
         x = "Year")

#Any way of binding the two datasets? 
#library(plyr)
#combined <- rbind.fill(UK_mean_SST[c("Date", "UK_mean")], pp_monthly[c("Date", "n")])

#Making the date for temperature the first of the month (for that whole month) 
#UK_mean_01_month <- UK_mean_SST

#Make the SST yearly 
#UK_mean_01_month$monthYear <- as.Date(as.yearmon(UK_mean_01_month$Date))
#UK_mean_01_month$quarterYear <- as.Date(as.yearqtr(UK_mean_01_month$Date))
#head(UK_mean_01_month)

#Line plot of strandings per month (grouped)
#Have changed monthYear to Date 
#ggplot(data = UK_mean_01_month, aes(x = monthYear, y = UK_mean, group=1)) +
  #geom_line()


#library(plyr)
#combined <- rbind.fill(UK_mean_01_month[c("monthYear", "UK_mean")], pp_monthly[c("Date", "n")])
combined <- UK_mean_01_month %>% 
  select(UK_mean, monthYear) %>%
  dplyr::rename(Date = monthYear)
  
#Combining uk_mean temp with species data 
combined <- merge(combined, ba_monthly, all = TRUE, by = c('Date'))
#Replacing NA with 0 
combined <- combined %>%
  mutate(n = ifelse(is.na(n),0, n))

#Phocoena strandings (monthly) vs monthly SST
ggplot(data = combined, aes(x = UK_mean, y = n)) +
  geom_point(size = 0.5) +
  labs(x = "Sea Surface Temperature ("~degree~"(C))", y = "Monthly Balaenoptera acutorostrata strandings") +
  #geom_text(aes(label = Date), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 

##########################################################################################
#Want to plot yearly max temp against yearly strandings 
#This is total_count 

#Bind STT_yearly max and totalcount 
SST_total_strandings <- bind_cols(SST_yearly_max, totalcount)
#remove duplicate 'Year' 
SST_total_strandings$Year <- NULL
#rename freq to strandings 
SST_total_strandings <- SST_total_strandings %>%
  dplyr::rename(strandings = freq)

#write.csv(SST_total_strandings, file = "SST_strandings_UK.csv")

#Yearly strandings total and yearly max temps 
f1 <- ggplot(data = SST_total_strandings, aes(x = year_max, y = strandings)) +
  geom_point(size = 0.5) +
  geom_text(aes(label = Year), size = 3, vjust = -0.5) +
  labs(x = "Yearly max. sea surface temperature ("~degree~"C)", y = "Yearly total strandings") +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 

f1

#m <- lm(year_max ~ freq, data = SST_total_strandings)
#a <- signif(coef(m)[1])
#b <- signif(coef(m)[2])
#textlab <- paste("y = ",b,"x + ",a, sep="")

#f2 <- f1 + geom_smooth(method = lm, formula = y~x) 

#f3 <- f2 + geom_text(aes(x = 15, y = 700, label = textlab), color="black", size=5, parse = FALSE)  

#plot(f3)

#f1

lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

f4 = f1 + annotate("text", x = 15, y = 700, label = lm_eqn(lm(strandings ~ year_max, SST_total_strandings)), colour="black", size = 5, parse=TRUE)
f4

#Data analysis 
modelSST <- lm(log(strandings) ~ log(year_max), data = SST_total_strandings)
summary(modelSST)


########################################################################################
#Total number of species per year, and maximum temp 
#Using total_speciesbyyear 

#Binding total strandings, total species per year and max years SST 
SST_total_strandings <- bind_cols(total_speciesbyyear, SST_total_strandings)
#Renaming strandings to strandings_total
SST_total_strandings <- SST_total_strandings %>%
  dplyr::rename(strandings_total = strandings)

#Selecting the specific columns
SST_total_strandings <- SST_total_strandings %>%
  select(Year, year_max, species, strandings_total)

#Plotting yearly total species against max yearly SST
ggplot(data = SST_total_strandings, aes(x = year_max, y = species)) +
  geom_point(size = 0.5) +
  geom_text(aes(label = Year), size = 3, vjust = -0.5) +
  labs(x = "Yearly max. sea surface temperature ("~degree~"C)", y = "Total species recorded") +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) 

#Data analysis - total species and yearly max SST
modelSST <- lm(log(species) ~ log(year_max), data = SST_total_strandings)
summary(modelSST)

###########################################################################################
#Monthly species strandings (only have yearly at the moment)
library(dplyr)
library(plyr)
library(lubridate)
library(zoo)
library(ggplot2)

cleaneddata <- read.csv("cleandatesnames.csv")
#I want all species 

species_window <- cleaneddata %>%
  filter(Year %in% c(1913:2015))

sapply(species_window, class)
#Be cautious of the format: dmy, ymd
species_window <- mutate(species_window, Date = dmy(Date))

sapply(species_window, class)

#Splitting my data into strandings per month 
species_window$monthYear <- as.Date(as.yearmon(species_window$Date))
species_window$quarterYear <- as.Date(as.yearqtr(species_window$Date))
head(species_window)

#Strandings gathered into months 
species_monthly <- head(species_window %>% group_by(monthYear) %>% dplyr::summarise(n = n()), 17402)
#Strandings per quarter 
species_quarter <- head(species_window %>% group_by(quarterYear) %>% dplyr::summarise(n = n()), 1133)

#Grouping by week number - not really needed - not running properly
#pp_window$week <- as.Date("1913-01-01")+7*trunc((pp_window$joinTimestamp / 1000)/(3600*24*7))

#Line plot of strandings per month (grouped)
#Have changed monthYear to Date 
species_monthly <- species_monthly %>%
  dplyr::rename(Date = monthYear)

#Monthly strandings 
ggplot(data = species_monthly, aes(x = Date, y = log(n), group=1)) +
  geom_line() +
  labs(y = "Monthly strandings")

ggplot(species_monthly, aes(n)) +
  geom_histogram(binwidth = 0.5)

#After 1970 only
#Can play around with the dates 
species_monthly_1970 <- species_monthly %>%
  filter(Date >= "1970-01-01", Date <= "2016-01-01")
#Plot
ggplot(data = species_monthly_1970, aes(x = Date, y = log(n), group=1)) +
  geom_line() +
  labs(y = "Monthly strandings since 1970") 

#Quarterley strandings - split into Jan, April, July and October 
#Firstly making date 'Date' 
species_quarter <- species_quarter %>%
  dplyr::rename(Date = quarterYear)

ggplot(data = species_quarter, aes(x = Date, y = log(n), group=1)) +
  geom_line() +
  labs(y = "Quarterly strandings")

#After 1970 only
#Can play around with the dates 
species_quarter_1970 <- species_quarter %>%
<<<<<<< HEAD
  filter(Date >= "1970-01-01", Date <= "2016-01-01")
=======
  filter(Date >= "2011-01-01", Date <= "2016-01-01")
>>>>>>> 3797e18e6b8921d0b8f122a34d6bc6773f46092f

#Plot
ggplot(data = species_quarter_1970, aes(x = Date, y = log(n), group=1)) +
  geom_line() +
  labs(y = "Quarterly strandings since 1970") 
<<<<<<< HEAD



=======



>>>>>>> 3797e18e6b8921d0b8f122a34d6bc6773f46092f
#Monthly mean SST against monthly strandings 
ggplot() +
  geom_line(data = species_monthly, aes(x = Date, y = n/5)) +
  geom_line(data = UK_mean_SST, aes(x = Date, y = UK_mean)) +
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Monthly stranded species")) +
  labs(x = "Date", y = "Monthly mean SST ("~degree~"C)")
 
#UK monthly max 
#Change time to date 

<<<<<<< HEAD
UK_Ireland_SST_max <- read.csv("UK_Ireland_SST.csv")
=======
>>>>>>> 3797e18e6b8921d0b8f122a34d6bc6773f46092f
UK_Ireland_SST_max <- UK_Ireland_SST_max %>%
  dplyr::rename(Date = time)

#Specific columns 
UK_monthly_SST <- UK_Ireland_SST_max %>%
  select(Date, Monthly_max)
<<<<<<< HEAD
=======

sapply(UK_monthly_SST, class)

tail(UK_monthly_SST)

#Filter out the dates 
UK_monthly_SST <- mutate(UK_monthly_SST, Date = dmy(Date))
UK_monthly_SST <- UK_monthly_SST %>%
  filter(Date >= "1912-12-16", Date <= "2016-01-16")


ggplot() +
  geom_line(data = species_monthly, aes(x = Date, y = n/5)) +
  geom_line(data = UK_Ireland_SST_max, aes(x = Date, y = Monthly_max)) +
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Monthly stranded species")) +
  labs(x = "Date", y = "Monthly maximum SST ("~degree~"C)")
>>>>>>> 3797e18e6b8921d0b8f122a34d6bc6773f46092f

sapply(UK_monthly_SST, class)

################    1970    #############################
#Filter out the dates 
UK_monthly_SST <- mutate(UK_monthly_SST, Date = ymd(Date))
  UK_monthly_SST <- UK_monthly_SST %>%
           filter(Date >= "1969-12-16", Date <= "2016-01-16")

  
#Monthly max temp and monthly strandings 
ggplot() +
  geom_line(data = species_monthly_1970, aes(x = Date, y = n/10)) +
  geom_line(data = UK_monthly_SST, aes(x = Date, y = Monthly_max)) +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Monthly stranded species")) +
  labs(x = "Date", y = "Monthly maximum SST ("~degree~"C)")

#binding monthly max temp and monthly strandings 
bind_cols(species_monthly, UK_monthly_SST)
