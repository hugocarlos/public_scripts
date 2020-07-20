# Libraries
library(ggplot2)
# Data taken from CONABIO, Mexico
# https://conabio.maps.arcgis.com/home/item.html?id=ecfda57c446041429dfe116991282af1
# Loading parsed data
Morelia <- read.csv(url("https://raw.githubusercontent.com/hugocarlos/public_scripts/master/COVID_Morelia.tsv"),
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE)
#Morelia <- read.csv("~/Documents/GitHub/public_scripts/COVID_Morelia.tsv",
#                    header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# Correcting the time
Morelia$Time <- 1:nrow(Morelia)
Morelia$Date <- as.Date(Morelia$Time, origin = "2020-06-16")
# 
# Plot
p <- ggplot(Morelia, aes(x = Date)) +
  geom_point(aes(y = ActiveCases, colour = "Casos activos")) +
  geom_line(aes(y = ActiveCases, colour = "Casos activos")) +
  scale_x_date(date_labels = "%b %d", date_breaks = "3 days") +
  ylim(-1, max(Morelia$ActiveCases)) +
  geom_point(aes(y = NewCases, colour = "Nuevos casos")) +
  geom_line(aes(y = NewCases, colour = "Nuevos casos")) +
  scale_colour_manual(values = c("tomato", "black")) +
  labs(x = "Fecha", y = "Número de casos", colour = "") +
  ggtitle("Casos en Morelia (activos y nuevos)") +
  theme(plot.title = element_text(size=13, face="bold"),
        legend.position = c(0.14, 0.82),
        legend.background = element_rect(fill = "lightblue"),
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        plot.background = element_rect(fill = "lightblue"))
png("COVID_Morelia.png", width = 700, height = 400, units = "px", res = 100)
print(p)
dev.off()
