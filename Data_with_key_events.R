
#Attempting to add arrows to the stranding data and whaling graph 

#Whaling and stranding plot data 

#Using "Catch_and_strandings" dataset (see "Whaling_code.R" for details)

library(grid)

#This plot adds the two war periods
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

#p <- p + annotate("segment", x = 0, xend = -0, y = 0, yend = -20, colour="blue", size=2, arrow=arrow())

#p <- p + geom_vline(xintercept = 1914, linetype="dotted", 
                #color = "blue", size=1.5)

#p <- p + theme(legend.position = c(0.9, 0.1)) 

#Putting the legend outside the plot 
p <- p + theme(legend.justification = "top")

#WWI
p <- p + annotate("rect", xmin=1914, xmax=1918, ymin=0, ymax=60, alpha=.1, fill="blue") +
geom_text(
  aes(x = 1916, y = 60, label = "WWI"))

#WWII
p <- p + annotate("rect", xmin=1939, xmax=1945, ymin=0, ymax=60, alpha=.1, fill="blue") +
  geom_text(
    aes(x = 1942, y = 60, label = "WWII"))

#Moratorium 1986
p <- p + annotate("rect", xmin=1985, xmax=2015, ymin=0, ymax=60, alpha=.1, fill="blue") +
  geom_text(
    aes(x = 1985, y = 60, label = "Moratorium: 1985/1986 season"))

p

dev.off()


#Adding arrows to the graph 







