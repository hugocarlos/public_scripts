# Libraries
library(ggplot2)
# Data taken from CONABIO, Mexico
# https://conabio.maps.arcgis.com/home/item.html?id=ecfda57c446041429dfe116991282af1
# Loading parsed data
Morelia <- read.csv(url("https://raw.githubusercontent.com/hugocarlos/public_scripts/master/COVID_Morelia_cp.tsv"),
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE)
#Morelia <- read.csv("~/Documents/GitHub/public_scripts/COVID_Morelia.tsv",
Morelia <- read.csv("~/Documents/GitHub/public_scripts/COVID_Morelia_cp.tsv",
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
# Adding weekends
extended_vector <- rep(c("weekday", "weekday", "weekday", "weekend", "weekend", "weekday", "weekday"),
                       (nrow(Morelia)/7 + 1))
tail(Morelia)
Morelia$weekend <- extended_vector[1:nrow(Morelia)]
# Plot
(p <- ggplot(Morelia, aes(x = Date)) +
#  geom_point(aes(y = ActiveCases, colour = "Casos activos")) +
  geom_point(aes(y = ActiveCases, colour = weekend)) +
  geom_line(aes(y = ActiveCases, colour = "Casos activos")) +
  geom_bar(stat = "identity", aes(y = NewCases, fill = "Nuevos casos")) +
  geom_line(aes(y = Average, colour = "7-días promedio")) +
  scale_x_date(date_labels = "%b %d", date_breaks = "3 days",
               limits = c(Morelia$Date[170], max(Morelia$Date) + 1)) +
  #xlim(Morelia$Date[3], max(Morelia$Date)) +
  ylim(-1, max(Morelia$ActiveCases)) +
  scale_colour_manual(values = c("black", "sienna", "sienna", "orangered")) +
  scale_fill_manual(values = c("gray60")) +
  labs(x = "Fecha", y = "Número de casos", colour = "") +
  ggtitle("Casos en Morelia (activos y nuevos)") +
  theme(plot.title = element_text(size = 13, face="bold"),
        legend.title = element_blank(),
        legend.position = c(0.82, 0.75),
        legend.background = element_rect(fill = "lightblue"),
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        plot.background = element_rect(fill = "lightblue")))
png("COVID_Morelia.png", width = 700, height = 400, units = "px", res = 100)
print(p)
dev.off()

ggplot(Morelia, aes(x = Date)) +
 geom_bar(stat = "identity", aes(y = NewCases, fill = "New Cases")) +
 geom_line(aes(y = Average, colour = "7-days average")) +
 scale_x_date(date_labels = "%b %d", date_breaks = "3 days",
              limits = c(Morelia$Date[17], max(Morelia$Date) + 1)) +
 scale_colour_manual(values = c("black", "sienna", "sienna", "orangered")) +
     scale_fill_manual(values = c("gray60")) +
     labs(x = "Date", y = "Number of Positive Cases", colour = "") +
 ggtitle("Cases in Morelia") +
 theme(plot.title = element_text(size=13, face="bold"),
       legend.title = element_blank(),
       legend.position = c(0.12, 0.8),
       legend.background = element_rect(fill = "lightblue"),
       panel.background = element_rect(fill = "lightblue",
                                       colour = "lightblue",
                                       size = 0.5, linetype = "solid"),
       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
       panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                       colour = "white"), 
       panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                       colour = "white"),
       plot.background = element_rect(fill = "lightblue"))


ggplot(Morelia, aes(x = Date)) +
  geom_line(aes(y = Average, colour = "7-días promedio")) +
  scale_x_date(date_labels = "%b %d", date_breaks = "5 days",
               limits = c(Morelia$Date[50], max(Morelia$Date) + 1)) +
  ggtitle("Casos en Morelia (promedio de 1 semana)") +
  theme(plot.title = element_text(size=13, face="bold"),
        legend.title = element_blank(),
        legend.position = c(0.36, 0.8),
        legend.background = element_rect(fill = "lightblue"),
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        plot.background = element_rect(fill = "lightblue"))
