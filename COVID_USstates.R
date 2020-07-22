# Plot of global incidence as if US states were different countries

df <- data.frame(Country = character(), Incidence = integer(), Type = character())
df <- rbind(df, data.frame(Country = "Brazil", Incidence = 2166532, Type = "Country"))
df <- rbind(df, data.frame(Country = "India", Incidence = 1194085, Type = "Country"))
df <- rbind(df, data.frame(Country = "Russia", Incidence = 783328, Type = "Country"))
df <- rbind(df, data.frame(Country = "South Africa", Incidence = 381798, Type = "Country"))
df <- rbind(df, data.frame(Country = "Peru", Incidence = 362087, Type = "Country"))
df <- rbind(df, data.frame(Country = "Mexico", Incidence = 356255, Type = "Country"))
df <- rbind(df, data.frame(Country = "Chile", Incidence = 334683, Type = "Country"))
df <- rbind(df, data.frame(Country = "Spain", Incidence = 313274, Type = "Country"))
df <- rbind(df, data.frame(Country = "UK", Incidence = 295817, Type = "Country"))
df <- rbind(df, data.frame(Country = "New York", Incidence = 435753, Type = "State"))
df <- rbind(df, data.frame(Country = "California", Incidence = 410176, Type = "State"))
df <- rbind(df, data.frame(Country = "Florida", Incidence = 369834, Type = "State"))
df <- rbind(df, data.frame(Country = "Texas", Incidence = 357127, Type = "State"))

df <- df[order(-df$Incidence), ]
df$Country <- factor(df$Country, levels = df$Country)

ggplot(df, aes(x = Country, y = Incidence, fill = Type)) +
  geom_bar(stat = "identity", color = "black") +
  scale_x_discrete(limits = as.vector(df$Country)) +
  scale_fill_brewer(palette = "Greys") +
  theme_minimal() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
