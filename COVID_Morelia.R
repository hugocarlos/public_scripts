# Libraries
library(ggplot2)
# Data taken from CONABIO, Mexico
# https://conabio.maps.arcgis.com/home/item.html?id=ecfda57c446041429dfe116991282af1
# Loading parsed data
Morelia <- read.csv(url("https://raw.githubusercontent.com/hugocarlos/public_scripts/master/COVID_Morelia.tsv"),
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE)
Morelia <- read.csv("~/Documents/GitHub/public_scripts/COVID_Morelia.tsv",
                    header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# Correcting the time
Morelia$Time <- 1:nrow(Morelia)
Morelia$Date <- as.Date(Morelia$Time, origin = "2020-06-16")
# Averaging
Morelia$Average <- sapply(1:nrow(Morelia), function(x){
  if(x < 7){
    return(NA)
  }
  mean(Morelia$NewCases[(x-6):x])
})
# Plot
(p <- ggplot(Morelia, aes(x = Date)) +
  geom_point(aes(y = ActiveCases, colour = "Casos activos")) +
  geom_line(aes(y = ActiveCases, colour = "Casos activos")) +
  scale_x_date(date_labels = "%b %d", date_breaks = "4 days", limits = c(Morelia$Date[3], max(Morelia$Date))) +
#  xlim(Morelia$Date[3], max(Morelia$Date)) +
  ylim(-1, max(Morelia$ActiveCases)) +
  geom_bar(stat = "identity", aes(y = NewCases, fill = "Nuevos casos")) +
  geom_line(aes(y = Average, colour = "7-días promedio")) +
  scale_colour_manual(values = c("black", "tomato")) +
  scale_fill_manual(values = c("gray60")) +
  labs(x = "Fecha", y = "Número de casos", colour = "") +
  ggtitle("Casos en Morelia (activos y nuevos)") +
  theme(plot.title = element_text(size=13, face="bold"),
        legend.title = element_blank(),
        legend.position = c(0.36, 0.8),
        legend.background = element_rect(fill = "lightblue"),
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        plot.background = element_rect(fill = "lightblue")))
png("COVID_Morelia.png", width = 700, height = 400, units = "px", res = 100)
print(p)
dev.off()
