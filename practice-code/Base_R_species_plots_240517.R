

#This code runs on from the 240517 Dates and column names, i.e rerun that code to
#clean the dataset before doing the below 

#Load these if not already loaded 
rm(list = ls())
install.packages("dplyr") 
install.packages("tidyr")
install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(ggplot2)


nhm<-read.csv("EDITNHMdata.csv", header = TRUE)
read.csv("EDITCSIPdata.csv")
csip<-read.csv("EDITCSIPdata.csv", header = TRUE)
names(csip)
names(nhm)


#How many records per species per year? 

####################################################

#Select grabs specific columns 

replace(nhmcsip$Name.Common, is.na(nhmcsip$Name.Common), "unknown cetacean")
replace(nhmcsip$Name.Current.Sci, is.na(nhmcsip$Name.Current.Sci), "unknown cetacean")

#Why don't the NAs continue through??
select(nhm, Year, Name.Current.Sci)
speciesyear<-select(nhm, Year, Name.Current.Sci) 
speciesyear

#Having a look at some plots by year 
splitspeciesyear<-data.frame(speciesyear,stringsAsFactors = TRUE)
splitspeciecsyear

#making species names and years factors
speciecsyear$Name.Current.Sci<-as.factor(speciesyear$Name.Current.Sci)
splitspeciesyear$Year<-as.factor(splitspeciesyear$Year)
splitspeciesyear[splitspeciesyear$year=="1913",]
summary(splitspeciesyear)



#used to be the aggregate function, now group_by 
nhm %>% group_by(Year)%>%group_by(Name.Current.Sci)
dim(splitspeciesdate)
splitspeciesyear %>% group_by(year)%>%group_by(Name.Current.Sci)
dim(splitspeciesyear)
summary(splitspeciesyear)
splitspeciesyear$Name.Current.Sci<-as.factor(splitspeciesyear$Name.Current.Sci)
splitspeciesyear$year<-as.factor(splitspeciesyear$year)
summary(splitspeciesyear)

splitspeciesyear$Name.Current.Sci



#filter instead of subset 
splitspeciesyear %>% filter(year == "1989")
phocoena<-splitspeciesyear %>% filter(Name.Current.Sci == "phocoena phocoena")
summary(phocoena)
#looking at a specific year 
phocoena[phocoena$year %in%c("1943"),]
library(dplyr)
library(ggplot2)
#plotting specific species
dim(phocoena)
names(phocoena)
phocoena$Name.Current.Sci<-as.factor(phocoena$Name.Current.Sci)
phocoena$year<-as.factor(phocoena$year)
phocoenayear<-phocoena$year<-as.factor(phocoena$year)

View(phocoenayear)
plot(phocoenayear, xlab = "Year", tck = -0.01, ylab= "Phocoena phocoena numbers", ylim=c(0,80), tck = -0.01)

#why doesn't : work??
phocoena[phocoena$year %in%c("1943"),]
View(phocoena)

#need to combine information into time bins e.g. 5 to 10 years 
#need to change physeter catodon to physeter macrocephalus 


nhm$Name.Current.Sci <- gsub("Physeter catodon", "Physeter Macrocephalus", nhm$Name.Current.Sci)

####blue whale 
splitspeciesyear %>% filter(Name.Current.Sci == "balaenoptera musculus")
balaenopteramusculus<-splitspeciesyear %>% filter(Name.Current.Sci == "balaenoptera musculus")
balmusyear<-balaenopteramusculus$year<-as.factor(balaenopteramusculus$year)
plot(balmusyear, xlab = "Year", tck = -0.01, ylab= "Bal. Mus numbers", ylim=c(0, 4), tck = -0.01)

###Common dolphin
nhm$Name.Current.Sci
splitspeciesyear %>% filter(Name.Current.Sci == "delphinus delphis")
delphinusdelphis<-splitspeciesyear %>% filter(Name.Current.Sci == "delphinus delphis")
deldelyear<-delphinusdelphis$year<-as.factor(delphinusdelphis$year)
plot(deldelyear, xlab = "Year", tck = -0.01, ylab= "D.delphis numbers", ylim=c(0, 35), tck = -0.01)

###Ziphius cavirostris 
splitspeciesyear %>% filter(Name.Current.Sci == "ziphius cavirostris")
zcavirosris<-splitspeciesyear %>% filter(Name.Current.Sci == "ziphius cavirostris")
zcaviyear<-zcavirosris$year<-as.factor(zcavirosris$year)
plot(zcaviyear, xlab = "Year", tck = -0.01, ylab= "Z. cavirosris", ylim=c(0, 4), tck = -0.01)

#unidentified dolpins (coeruleoalba? in the dataset) #Not working 
splitspeciesyear %>% filter(Name.Current.Sci == "un.delphinidae")
undelph<-splitspeciesyear %>% filter(Name.Current.Sci == "un.delphinidae")
undelphyear<-undelph$year<-as.factor(undelph$year)
plot(undelphyear, xlab = "Year", tck = -0.01, ylab= "Z. cavirosris", ylim=c(0, 4), tck = -0.01)

#Lagenorynchus actus
splitspeciesyear %>% filter(Name.Current.Sci == "lagenorhynchus acutus")
lagenacutus<-splitspeciesyear %>% filter(Name.Current.Sci == "lagenorhynchus acutus")
lagenacutusyear<-lagenacutus$year<-as.factor(lagenacutus$year)
plot(lagenacutusyear, xlab = "Year", tck = -0.01, ylab= "L. acutus", ylim=c(0, 40), tck = -0.01)


splitspeciesyear %>% filter(Name.Current.Sci == "lagenorhynchus albirostris")
lagenalbirostris<-splitspeciesyear %>% filter(Name.Current.Sci == "lagenorhynchus albirostris")

#need to look at: 
#Lagenorhynchus albirostris
#Grampus griseus

splitspeciesyear %>% filter(Name.Current.Sci == "lagenorhynchus albirostris")
lagenalbir<-splitspeciesyear %>% filter(Name.Current.Sci == "lagenorhynchus albirostris")
lagenalbiryear<-lagenalbir$year<-as.factor(lagenalbir$year)
plot(lagenalbiryear, xlab = "Year", tck = -0.01, ylab= "L. albirostris", ylim=c(0, 15), tck = -0.01)

#Tursiops truncatus
splitspeciesyear %>% filter(Name.Current.Sci == "tursiops truncatus")
turstrunc<-splitspeciesyear %>% filter(Name.Current.Sci == "tursiops truncatus")
turstruncyear<-turstrunc$year<-as.factor(turstrunc$year)
plot(turstruncyear, xlab = "Year", tck = -0.01, ylab= "T. truncatus", ylim=c(0, 15), tck = -0.01)

#Hyperoodon ampullatus
splitspeciesyear %>% filter(Name.Current.Sci == "hyperoodon ampullatus")
hyperampull<-splitspeciesyear %>% filter(Name.Current.Sci == "hyperoodon ampullatus")
hyperampullyear<-hyperampull$year<-as.factor(hyperampull$year)
plot(hyperampullyear, xlab = "Year", tck = -0.01, ylab= "H. ampullatus", ylim=c(0, 6), tck = -0.01)

#Mesoplodon bidens
splitspeciesyear %>% filter(Name.Current.Sci == "mesoplodon bidens")
mesobidens<-splitspeciesyear %>% filter(Name.Current.Sci == "mesoplodon bidens")
mesobidensyear<-mesobidens$year<-as.factor(mesobidens$year)
plot(mesobidensyear, xlab = "Year", tck = -0.01, ylab= "M. bidens", ylim=c(0, 6), tck = -0.01)

#Balaenoptera acutorostrata
splitspeciesyear %>% filter(Name.Current.Sci == "balaenoptera acutorostrata")
balacutorostrata<-splitspeciesyear %>% filter(Name.Current.Sci == "balaenoptera acutorostrata")
balacutorostratayear<-balacutorostrata$year<-as.factor(balacutorostrata$year)
plot(balacutorostratayear, xlab = "Year", tck = -0.01, ylab= "B. acutorostrata", ylim=c(0, 10), tck = -0.01)

#Globicephala melas
splitspeciesyear %>% filter(Name.Current.Sci == "globicephala melas")
globmelas<-splitspeciesyear %>% filter(Name.Current.Sci == "globicephala melas")
globmelasyear<-globmelas$year<-as.factor(globmelas$year)
plot(globmelasyear, xlab = "Year", tck = -0.01, ylab= "G. melas", ylim=c(0, 300), tck = -0.01)

#Grampus griseus 
splitspeciesyear %>% filter(Name.Current.Sci == "grampus griseus")
gramgris<-splitspeciesyear %>% filter(Name.Current.Sci == "grampus griseus")
gramgrisyear<-gramgris$year<-as.factor(gramgris$year)
plot(gramgrisyear, xlab = "Year", tck = -0.01, ylab= "G. griseus", ylim=c(0, 10), tck = -0.01)

#Orcinus orca
splitspeciesyear %>% filter(Name.Current.Sci == "orcinus orca")
orcorca<-splitspeciesyear %>% filter(Name.Current.Sci == "orcinus orca")
orcorcayear<-orcorca$year<-as.factor(orcorca$year)
plot(orcorcayear, xlab = "Year", tck = -0.01, ylab= "O. orca", ylim=c(0, 8), tck = -0.01)

#Peponocephala electra
splitspeciesyear %>% filter(Name.Current.Sci == "peponocephala electra")

#Pseudorca crassidens 
splitspeciesyear %>% filter(Name.Current.Sci == "pseudorca crassidens")
pseudorca<-splitspeciesyear %>% filter(Name.Current.Sci == "pseudorca crassidens")
pseudorcayear<-pseudorca$year<-as.factor(pseudorca$year)
plot(pseudorcayear, xlab = "Year", tck = -0.01, ylab= "P.crassidens", ylim=c(0, 200), tck = -0.01)

#monodon monocerus (1949 x2) 
splitspeciesyear %>% filter(Name.Current.Sci == "monodon monoceros")

#Deplhinopterus leucas (1932 x 1) 
splitspeciesyear %>% filter(Name.Current.Sci == "delphinapterus leucas")

#Kogia breviceps (1966, 1980, 1985)
splitspeciesyear %>% filter(Name.Current.Sci == "kogia breviceps")

#Physeter macrocephalus ##not sure why this isn't working....
View(nhm)

splitspeciesyear %>% filter(Name.Current.Sci == "physeter catodon")
phycatodon<-splitspeciesyear %>% filter(Name.Current.Sci == "physeter catodon")
phycatodonyear<-phycatodon$year<-as.factor(phycatodon$year)
plot(phycatodonyear, xlab = "Year", tck = -0.01, ylab= "P.catodon", ylim=c(0, 10), tck = -0.01)

#mesoplodon europaeus (1 1989)
splitspeciesyear %>% filter(Name.Current.Sci == "balaenoptera physalus")
balaenphys<-splitspeciesyear %>% filter(Name.Current.Sci == "balaenoptera physalus")
balaenphysyear<-balaenphys$year<-as.factor(balaenphys$year)
plot(balaenphysyear, xlab = "Year", tck = -0.01, ylab= "B. physalus", ylim=c(0, 5), tck = -0.01)

#Megaptera novaeangliae (2, 1982, 1985)
splitspeciesyear %>% filter(Name.Current.Sci == "megaptera novaeangliae")

#Balaena mysticetus 
splitspeciesyear %>% filter(Name.Current.Sci == "")






group_by(splitspeciesyear, year)
levels(splitspeciesyear$Name.Current.Sci)

splitspeciesyear %>%
  group_by(Name.Current.Sci)

summary(nhm$Name.Common)



###########################################################


