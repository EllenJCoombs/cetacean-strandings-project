#This code runs on from the 240517 name cleaning code, i.e rerun that code to
#clean the dataset before doing the below 
#Base R plot practice - ignore! 

# Load libraries/packages 
library(dplyr)
library(tidyr)
library(tidyverse) 
library(ggplot2)

#Some plots (histograms)
select(nhmcsip, Date, Name.Current.Sci) 

summary(datespecies) 
tbl_df(datespecies)
View(datespecies)
glimpse(byspecies)


species <- nhmcsip$Name.Current.Sci
nhmcsip$Name.Current.Sci

#Filtering out phocoena phocoena
filter(nhmcsip, Name.Current.Sci == "phocoena phocoena")
phocoena <- nhmcsip %>% filter(Name.Current.Sci == "phocoena phocoena") 
#This filters out just phocoena and the year 
phocoenadate <- select(phocoena, Name.Current.Sci, Year)
names(phocoenadate) 


sapply(phocoenadate, class)
#Not sure how to get this to work 
ggplot(data = phocoenadate, aes(x = Year, y = Name.Current.Sci)) + geom_point()
myplot <- ggplot(data = phocoenadate, aes(x= Year, y = Name.Current.Sci)) +
  myplot + geom_point()


#Histogram of phocoena 
filter(nhmcsip, Name.Current.Sci == "phocoena phocoena")
pphocoena <- nhmcsip %>% filter(Name.Current.Sci == "phocoena phocoena") 
pphocoenadate <- select(pphocoena, Name.Current.Sci, Year)
names(pphocoenadate) 

ggplot(pphocoenadate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Phocoena phocoena count")


#Not sure how to get this to work 
ggplot(phocoenadate,
       aes(y = Name.Current.Sci, x = Year)) +
  geom_point()


#Orca plot 
filter(nhmcsip, Name.Current.Sci == "orcinus orca")
oorca <- nhmcsip %>% filter(Name.Current.Sci == "orcinus orca") 
oorcadate <- select(oorca, Name.Current.Sci, Year)
names(oorcadate) 

ggplot(oorcadate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Orcinus orca count")


#Delphinus delphis 
filter(nhmcsip, Name.Current.Sci == "delphinus delphis")
ddelphis <- nhmcsip %>% filter(Name.Current.Sci == "delphinus delphis") 
ddelphisdate <- select(ddelphis, Name.Current.Sci, Year)
names(ddelphisdate) 

ggplot(ddelphisdate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Delphinus delphis count")


#Stenella coeruleoalba
filter(nhmcsip, Name.Current.Sci == "stenella coeruleoalba")
stenellacoer <- nhmcsip %>% filter(Name.Current.Sci == "stenella coeruleoalba") 
stenellacoerdate <- select(stenellacoer, Name.Current.Sci, Year)
names(stenellacoerdate) 

ggplot(stenellacoerdate, aes(x = Year)) +
  geom_histogram()


#Stenella coeruleoalba
filter(nhmcsip, Name.Current.Sci == "stenella coeruleoalba")
scoeruleoalba <- nhmcsip %>% filter(Name.Current.Sci == "stenella coeruleoalba") 
scoeruleoalbadate <- select(scoeruleoalba, Name.Current.Sci, Year)
names(scoeruleoalbadate)

ggplot(scoeruleoalbadate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Stenella coeruleoalba count")


#Fin whale 
filter(nhmcsip, Name.Current.Sci == "balaenoptera physalus")
bphysalus <- nhmcsip %>% filter(Name.Current.Sci == "balaenoptera physalus") 
bphysalusdate <- select(bphysalus, Name.Current.Sci, Year)
names(bphysalusdate) 

ggplot(bphysalusdate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
                   xlim(1900, 2017) +
  ylab("Balaenoptera physalus count")


#Changing the x-axis and bin size 
filter(nhmcsip, Name.Current.Sci == "balaenoptera musculus")
bmusculus <- nhmcsip %>% filter(Name.Current.Sci == "balaenoptera musculus") 
bmusculusdate <- select(bmusculus, Name.Current.Sci, Year)
names(bmusculusdate) 

ggplot(bmusculusdate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Balaenoptera musculus count")


#Long-finned pilot whale 
filter(nhmcsip, Name.Current.Sci == "Globicephala melas")
Gmelas <- nhmcsip %>% filter(Name.Current.Sci == "balaenoptera musculus") 
Gmelasdate <- select(gmelas, Name.Current.Sci, Year)
names(gmelasdate) 

ggplot(balaenopteramusdate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Balaenoptera musculus count")



#Sowerby's beaked whale
filter(nhmcsip, Name.Current.Sci == "mesoplodon bidens")
mbidens <- nhmcsip %>% filter(Name.Current.Sci == "mesoplodon bidens") 
mbidensdate <- select(mbidens, Name.Current.Sci, Year)
names(mbidensdate) 

ggplot(mbidensdate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Mesoplodon bidens count")


#Bottlenose dolphin 
filter(nhmcsip, Name.Current.Sci == "tursiops truncatus")
ttruncatus <- nhmcsip %>% filter(Name.Current.Sci == "tursiops truncatus") 
ttruncatusdate <- select(ttruncatus, Name.Current.Sci, Year)
names(ttruncatusdate) 

ggplot(ttruncatusdate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Tursiops truncatus count")


#risso's dolphin 
filter(nhmcsip, Name.Current.Sci == "grampus griseus")
ggriseus <- nhmcsip %>% filter(Name.Current.Sci == "grampus griseus") 
ggriseusdate <- select(ggriseus, Name.Current.Sci, Year)
names(ggriseusdate) 

ggplot(ggriseusdate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Grampus griseus count")


#Sperm whale 
filter(nhmcsip, Name.Current.Sci == "physeter catodon")
pcatodon <- nhmcsip %>% filter(Name.Current.Sci == "physeter catodon") 
pcatodondate <- select(pcatodon, Name.Current.Sci, Year)
names(pcatodondate) 

ggplot(pcatodondate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Physeter catodon count")


#Cuvier's beaked whale 
filter(nhmcsip, Name.Current.Sci == "ziphius cavirostris")
zcavirostris <- nhmcsip %>% filter(Name.Current.Sci == "ziphius cavirostris") 
zcavirostrisdate <- select(zcavirostris, Name.Current.Sci, Year)
names(zcavirostrisdate) 

ggplot(zcavirostrisdate, aes(x = Year)) +
  geom_histogram(binwidth = 0.5) +
  xlim(1900, 2017) +
  ylab("Ziphius cavirostris count")



