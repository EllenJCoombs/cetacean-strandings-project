#Whaling data 
#Practicing with minkes as the data are pretty good 

#Using Natalie's code 
#Minkes 
whales <- c("balaenoptera acutorostrata")

# Plot graphs for 1985 - 2017 (that's the catch data I have)
minkes <- plot_all_whale_years(ds, species.col = "Name.Current.Sci", whales,
                     binwidth = 0.5, start.date = 1985, end.date = 2017)


#Minke catch data from Norway following 1986 moratorium - need to find data from 1960

library(dplyr)
#Minke stranding data 
minkecount <- speciesyearcount %>%
  filter(Name.Current.Sci == "Balaenoptera acutorostrata")

minkecount_edit <- rename(minkecount, Strandings = n) %>%
  filter(Year %in% c(1985:2015))

#Minke catch data 
minke_catch_data <- read.csv("Minke_catch_data_Norway_Iceland.csv")
minke_catch_data <- rename(minke_catch_data, Catch = Count) 
  
ggplot() + 
  geom_line(data = minke_catch_data, aes(x = Year, y = Catch), col = "red") +
  geom_line(data = minkecount_edit, aes(x = Year, y = Strandings), alpha = .5) +
  xlim(1985, 2015)


#######################################################################################
#Looking at whether the whaling data is consistent with the stranding data 
#Blue, Fin, Sei, Right, Bottlenose, Minke, Humpback, Sperm, others 

whaling <- read.csv("North_Atlantic_whaling.csv")
View(whaling)

#Selecting the species and the year 
whaling_species <- whaling %>%
  select(Year, Balaenoptera.musculus, Balaenoptera.physalus, 
         Megaptera.novaeangliae, Balaenoptera.borealis, Physeter.macrocephalus, 
         Others, Eubalaena.glacialis, Hyperoodon.ampullatus, Balaenoptera.acutorostrata, 
         Total.catch)

#Want to rename the columns to match species name in other plots 
whaling_species <- rename(whaling_species, "Balaenoptera physalus" = "Balaenoptera.physalus", 
       "Balaenoptera musculus" = "Balaenoptera.musculus",
       "Megaptera novaeangliae" = "Megaptera.novaeangliae",
       "Balaenoptera borealis" = "Balaenoptera.borealis",
       "Physeter macrocephalus" = "Physeter.macrocephalus", 
       "Eubalaena glacialis" = "Eubalaena.glacialis", 
       "Hyperoodon ampullatus" = "Hyperoodon.ampullatus",
       "Balaenoptera acutorostrata" = "Balaenoptera.acutorostrata")



#Plotting species and year 
library("reshape2")
library(RColorBrewer)

whaling_plot <- melt(whaling_species, id="Year")  # convert to long format

#Renaming the columns 
whaling_plot <- rename(whaling_plot, Count = value, 
       Name.Current.Sci = variable)

#Plotting all catch, North Atlantic + Total catch 
ggplot(data = whaling_plot,
       aes(x = Year, y = Count, colour = Name.Current.Sci, ylab = "Count")) +
  geom_line() +
  facet_wrap(~ Name.Current.Sci) 

#Want to order the data by year (not species as it currently is) 
whaling_plot %>%
  arrange(Year)

#Need to remove "Total.Catch" column" 
whaling_species_no_total <- whaling_species %>%
  select("Year", "Balaenoptera musculus", "Balaenoptera physalus", 
         "Megaptera novaeangliae", "Balaenoptera borealis", "Physeter macrocephalus", 
         "Others", "Eubalaena glacialis", "Hyperoodon ampullatus", "Balaenoptera acutorostrata")


#If I want to look at total killed - just look at "Total.catch" in the "whaling_species") 


#Stripping out the same species from the stranding data (those that were hunted)
hunted_stranders <- cleaneddata %>% 
  filter(Name.Current.Sci %in% c("Balaenoptera musculus", "Balaenoptera physalus", 
                                 "Megaptera novaeangliae", "Balaenoptera borealis", 
                                 "Physeter macrocephalus", "Eubalaena glacialis", 
                                 "Hyperoodon ampullatus", "Balaenoptera acutorostrata"))

#Aggregating by year 
hunted_strandersyear <- count(hunted_stranders, Name.Current.Sci, Year)

#Arranging species by year by count 
hunted_stranders_total <- hunted_strandersyear %>%
  group_by(n, Year, Name.Current.Sci) %>%
  arrange(Year)


ggplot(data = hunted_stranders_total,
       aes(x = Year, y = n, colour = Name.Current.Sci, ylab = "Count")) +
  geom_line() +
  facet_wrap(~ Name.Current.Sci) 

#The totals of stranded whales (that were hunting candidates) stranding each year
Total_hunted_stranders <- aggregate(n ~ Year, hunted_stranders_total, sum)
#Total hunted each year (and only looking at 1913 - 2015)
Total_hunted <- whaling %>% 
  select(Year, Total.catch) %>%
  filter(Year %in% c(1913:2015))

#Want to plot total whaled and total strandings of hunted species 

#Changing the total.catch column to n 
Total_hunted_rename <- rename(Total_hunted, n = Total.catch)

#Plotting total hunted and total strandeds (of hunted species)
#don't think this works...
ggplot(Total_hunted_stranders$n*100, aes(x = Year, y = n, ylab = "Count")) + geom_line(aes(color = "Strandings"))+
  geom_line(data = Total_hunted_rename, aes(color="Hunted")) +
  scale_y_continuous(sec.axis = sec_axis(~.*100))
  labs(color = "Stranding and hunting data") 

  
#Bind the two datasets (Total_hunted and Total_hunted_stranding)
#Clean up the data 
Catch_and_strandings <- bind_cols(Total_hunted, Total_hunted_stranders) %>%
  select(Year, Total.catch, n) %>%
  dplyr::rename("Strandings" = "n")

#Plot them both on the same graph 
  p <- ggplot(Catch_and_strandings, aes(x = Year))
  p <- p + geom_line(aes(y = Strandings, colour = "Total strandings"))
  
  # adding the stranding data, transformed to match roughly the range of the total catch
  p <- p + geom_line(aes(y = Total.catch/20, colour = "Total catch"))
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*20, name = "Total catch"))
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("blue", "red"))
  p <- p + labs(y = "Total stranding",
                x = "Year",
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.8, 0.9))
  p  

  
#######################################################################################
#Each species stranding and whaling 
  
#Need "iwc" and "hunted_stranders_total" 
#Each species seperately...?
  
iwc <- whaling_species_no_total
  
library(reshape2)
library(mvbutils)
  
#Arranging by species for catch data 
iwc$X <- NULL
iwc <- melt(iwc, measure.vars=names(iwc)%except%"Year") 

#Have loaded the reshape package so need to be careful when using dplyr 
iwc <- iwc %>% 
  dplyr::rename(Name.Current.Sci = variable) %>%
  dplyr::rename(Catch = value) %>% 
  arrange(Year)

#Remove the 1910 - 1913 data 
iwc <- iwc %>%
  filter(Year %in% c(1913:2015))

#Clean up hunted_stranders_total
hunted_stranders_total <- hunted_stranders_total %>%
  dplyr::rename(Strandings = n)

#Plot the data together 
#Remove facet_wrap if you don't want by species plots 
ggplot() + 
  geom_line(data = hunted_stranders_total, aes(x = Year, y = Strandings, colour = "Stranded")) +
  geom_line(data = iwc, aes(x = Year, y = Catch/20, colour = "Hunted")) +
  scale_y_continuous(sec.axis = sec_axis(~.*20, name = "Total catch")) +
  labs(y = "Total stranding",
              x = "Year",
              colour = "Parameter") +
  facet_wrap(~ Name.Current.Sci)


#regression lines 
model1 <- lm(Year ~ Strandings, data = hunted_stranders_total)

###########################################################################
#Fitting a regression line to whaling and catch data
#Adding a 'total per year' to each dataset 

#'Total_hunted'
#'Total-hunted_stranded'

install.packages("devtools")
library(devtools)
library(dplyr)


hunted_and_stranded <- bind_cols(Total_hunted, Total_hunted_stranders)

#Renaming the columns 

hunted_and_stranded <- hunted_and_stranded %>%
  dplyr::rename("Catch" = Total.catch) %>%
  dplyr::rename("Stranded" = n) %>%
  select("Year", "Catch", "Stranded")

#Plot both on the same graph
#Have done log calc but not sure if it needed it (can also remove this and do non log
#calculation)

#Regression line 
a1 <- ggplot(data = hunted_and_stranded, aes(x = Stranded, y = Catch)) +
  geom_point(size = 0.5) +
  geom_text(aes(label = Year), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) +
  labs(x = "Total stranded", y = "Total catch (hunted)") #se=FALSE, colour = "grey70", size =0.7) 

a1

m <- lm(Stranded ~ Catch, data = hunted_and_stranded1970)
a <- signif(coef(m)[1])
b <- signif(coef(m)[2])
textlab <- paste("y = ",b,"x + ",a, sep="")

a2 <- a1 + geom_smooth(method = lm, formula = y~x) 

a3 <- a2 + geom_text(aes(x = 35, y = 150, label = textlab), color="black", size=5, parse = FALSE)  

plot(a3)


##############################################
#After 1970 only with regression line 

hunted_and_stranded1970 <- hunted_and_stranded %>% 
  filter(Year %in% c(1986:2015))

b1 <- ggplot(data = hunted_and_stranded1970, aes(x = Stranded, y = Catch)) +
  geom_point(size = 0.5) +
  geom_text(aes(label = Year), size = 3, vjust = -0.5) +
  geom_smooth(method = lm, se=FALSE, colour = "grey70", size =0.7) +
  labs(x = "Total stranded", y = "Total catch (hunted)") #se=FALSE, colour = "grey70", size =0.7) 


#m <- lm(hunted_and_stranded1970$Stranded ~ hunted_and_stranded1970$Catch)
m <- lm(Stranded ~ Catch, data = hunted_and_stranded1970)
a <- signif(coef(m)[1])
b <- signif(coef(m)[2])
textlab <- paste("y = ",b,"x + ",a, sep="")

b2 <- b1 + geom_smooth(method = lm, formula = y~x) 

b3 <- b2 + geom_text(aes(x = 35, y = 150, label = textlab), color="black", size=5, parse = FALSE)  

plot(b3)

#Alternative method using a function can be used for the two above graphs 
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

a4 = a1 + geom_text(aes(x = 35, y = 150, label = lm_eqn(lm(y ~ x, df))), parse = TRUE)


