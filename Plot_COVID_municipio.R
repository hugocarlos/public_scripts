# 
# Data: http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip

#FECHA_ACTUALIZACION,ID_REGISTRO,ORIGEN,SECTOR,ENTIDAD_UM,SEXO,ENTIDAD_NAC,ENTIDAD_RES,MUNICIPIO_RES,TIPO_PACIENTE,
#         2020-07-23,     1162e9,     2,     4,        09,   2,         20,         09,          002,            1,

#FECHA_INGRESO,FECHA_SINTOMAS, FECHA_DEF,INTUBADO,NEUMONIA,EDAD,NACIONALIDAD,EMBARAZO,HABLA_LENGUA_INDIG,DIABETES,
#   2020-03-23,    2020-03-22,9999-99-99,      97,       2,  28,           1,      97,                 2,       2,

#EPOC,ASMA,INMUSUPR,HIPERTENSION,OTRA_COM,CARDIOVASCULAR,OBESIDAD,RENAL_CRONICA,TABAQUISMO,OTRO_CASO,RESULTADO,
#   2,   2,       2,           2,       2,             2,       2,            2,         2,       99,        1,

#MIGRANTE,PAIS_NACIONALIDAD,PAIS_ORIGEN,UCI
#      99,           México,         99, 97

# SEXO 2: hombre
# TIPO_PACIENTE: 1 ambulatorio, 2 hospitalizado
# RESULTADO: 1 positivo, 2 no positivo, 3 resultado pendiente

# Libraries
library(tidyverse)

# Did not work... manually downloaded
#data2 <- read_csv("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",
#                  col_names = TRUE, n_max = 10, quote = "\"")

#data <- read_csv("~/Documents/Personal/others/datos_abiertos_covid19.zip",
#                  col_names = TRUE, quote = "\"")
unzip("~/Documents/Personal/others/datos_abiertos_covid19.zip", exdir = "~/Documents/Personal/others/")
data <- read.csv("~/Documents/Personal/others/200725COVID19MEXICO.csv",
                 header = TRUE, quote = "\"", sep = ",")


# Calculating average positivity in the last 7 days
tolerance_for_tests <- 7
uncertain_period <- 14
today <- Sys.Date()
daily_positivities <- t(sapply(1:tolerance_for_tests, function(x){
  today_cases <- length(which(data$FECHA_INGRESO == as.character(today - x + 1) &
                                data$RESULTADO == 1))
  today_tests <- length(which(data$FECHA_INGRESO == as.character(today - x + 1)))
  toReturn <- c(today_cases, today_tests)
  return(toReturn)
}))
positivity <- sum(daily_positivities[ ,1]) / sum(daily_positivities[ ,2])

# Input values
una_entidad <- 9 # CDMX
un_municipio <- 3 # Coyoacan
municipality <- "Coyoacan"
una_entidad <- 16 # Michoacan
un_municipio <- 53 # Morelia
una_fecha <- today - 28
municipality <- "Morelia"

# Emptying space
# rm(data)

# Filtering by municipality, reducing columns
subdata <- data %>%
  filter(MUNICIPIO_RES == un_municipio & ENTIDAD_RES == una_entidad) %>%
  select(EDAD, RESULTADO, SEXO, TIPO_PACIENTE, FECHA_SINTOMAS)
# reformatting
subdata$FECHA_SINTOMAS <- as.Date(subdata$FECHA_SINTOMAS)
# Filtering by date of symptoms
subdata_recent <- subdata %>%
  filter(FECHA_SINTOMAS >= una_fecha)

# Positive cases
tabla_pos <- table(subdata_recent %>%
                     filter(RESULTADO == 1) %>%
                     select(FECHA_SINTOMAS))
# Pending result
tabla_pend <- table(subdata_recent %>%
                      filter(RESULTADO == 3) %>%
                      select(FECHA_SINTOMAS))

all_dates <- union(names(tabla_pos), names(tabla_pend))
tabla_estimated <- sapply(all_dates, function(x){
  suma <- 0
  if(!is.na(tabla_pos[x]))
    suma <- suma + tabla_pos[x]
  if(!is.na(tabla_pend[x]))
    suma <- suma + (tabla_pend[x] * positivity)
  return(suma)
})
names(tabla_estimated) <- all_dates

df <- data.frame(Dates = as.Date(names(tabla_estimated)), Cases = tabla_estimated)
#rownames(df) <- NULL

(q <- ggplot(df, aes(x = Dates)) +
#    geom_point(aes(y = Cases, colour = "Inicio de síntomas")) +
    geom_bar(stat = "identity", aes(y = Cases, colour = "Inicio de síntomas")) +
#    geom_line(aes(y = Cases, colour = "Inicio de síntomas")) +
    geom_rect(aes(xmin = Dates[nrow(df) - uncertain_period + 1], xmax = Dates[nrow(df)] + 1,
                  ymin = 0, ymax = (max(Cases) + 2)),
              alpha = 0.03, fill = "tomato") +
    annotate(geom = "text", x = df$Dates[nrow(df)] - 6, y = (max(df$Cases) - 2),
             label = "Estos valores pueden aumentar", color = "black") +
    scale_x_date(date_labels = "%b %d", date_breaks = "2 days") +
    scale_colour_manual(values = "black") +
    labs(x = "Fecha", y = "Número de casos estimados", colour = "") +
    ggtitle(paste0("Casos estimados por fecha de inicio de síntomas en ", municipality)) +
    theme(legend.position="none",
          plot.title = element_text(size=13, face="bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"),
          plot.background = element_rect(fill = "lightblue")))


png(ppaste0("COVID_estimados_", municipality, ".png"), width = 700, height = 400, units = "px", res = 100)
print(p)
dev.off()
