# 
# Data: http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip
#FECHA_ACTUALIZACION ID_REGISTRO ORIGEN SECTOR ENTIDAD_UM SEXO ENTIDAD_NAC ENTIDAD_RES
#         2020-10-13      071735      2      9         21    2          21          21
#MUNICIPIO_RES TIPO_PACIENTE FECHA_INGRESO FECHA_SINTOMAS  FECHA_DEF INTUBADO NEUMONIA EDAD
#          114             1    2020-03-18     2020-03-12 9999-99-99       97        2   75
#NACIONALIDAD EMBARAZO HABLA_LENGUA_INDIG INDIGENA DIABETES EPOC ASMA INMUSUPR HIPERTENSION
#           1       97                  2        2        1    2    2        2            2
#OTRA_COM CARDIOVASCULAR OBESIDAD RENAL_CRONICA TABAQUISMO OTRO_CASO TOMA_MUESTRA
#       2              1        2             2          2         2            1
#RESULTADO_LAB CLASIFICACION_FINAL MIGRANTE PAIS_NACIONALIDAD PAIS_ORIGEN UCI
#            1                   3       99            México          97  97

# SEXO 2: hombre
# TIPO_PACIENTE: 1 ambulatorio, 2 hospitalizado
# RESULTADO_LAB: 1 positivo, 2 no positivo, 3 resultado pendiente, 4 resultado no adecuado, 97 no aplica
# Disease: 1 = Yes, 2 = Not
# CLASIFICACION_FINAL: 1 = associacion epidemiologica, 2 = comite de dictaminacion, 3 = confirmado por laboratorio,
# 4:7 son no positivos

# Libraries
library(tidyverse)

catalogoEntidades <- read.csv("~/Documents/GitHub/public_scripts/Catalogo_de_ENTIDADES.tsv", header = TRUE,
                              sep = "\t")
catalogoMunicipios <- read.csv("~/Documents/GitHub/public_scripts/Catalogo_MUNICIPIOS.tsv", header = TRUE,
                               sep = "\t")
options(timeout = 600)
download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",
              "~/Documents/Personal/others/datos_abiertos_covid19.zip")
unzip("~/Documents/Personal/others/datos_abiertos_covid19.zip", exdir = "~/Documents/Personal/others/")
# If the file was downloaded after 10pm and before midnight it is possible that the new file is already
# available. This possibility is not considered yet.
today <- Sys.Date() - 1
data <- read.csv(paste0("~/Documents/Personal/others/", substr(gsub("-", "", today), 3, 8),
                        "COVID19MEXICO.csv"), header = TRUE, quote = "\"", sep = ",")
#data <- read.csv(paste0("~/Documents/Personal/others/COVID19MEXICO.csv"), header = TRUE, quote = "\"",
#                 sep = ",")

# Calculating average positivity in the last 7 days
tolerance_for_tests <- 7
uncertain_period <- 14
daily_positivities <- t(sapply(1:tolerance_for_tests, function(x){
  # x <- 1
  # Counting the positive tests
  oneday_pos <- length(which(data$FECHA_INGRESO == as.character(today - x + 1) &
                                data$RESULTADO_LAB == 1))
  # Counting the negative tests
  oneday_neg <- length(which(data$FECHA_INGRESO == as.character(today - x + 1) &
                               data$RESULTADO_LAB == 2))
  toReturn <- c(oneday_pos, (oneday_pos + oneday_neg))
  return(toReturn)
}))
(positivity <- sum(daily_positivities[ ,1]) / sum(daily_positivities[ ,2]))

# Input values
municipality <- "Morelia" # "Coyoacán" # "Benito Juárez" # "Álvaro Obregón" # "Querétaro" # "Pátzcuaro"

un_municipio <- which(catalogoMunicipios$MUNICIPIO == toupper(municipality))
# catalogoMunicipios[un_municipio, ]
if(any(!length(un_municipio))){
  print("No municipalities have this name!")
}else if(length(un_municipio) > 1){
  print("More than one municipality with this name. Specify the state!")
#  una_entidad <- "CIUDAD DE MÉXICO" # "Quintana Roo" 
  una_entidad <- catalogoEntidades$CLAVE_ENTIDAD[grep(una_entidad, catalogoEntidades$ENTIDAD_FEDERATIVA,
                                                      ignore.case = TRUE)]
  un_municipio <- catalogoMunicipios$CLAVE_MUNICIPIO[which(catalogoMunicipios$MUNICIPIO == toupper(municipality) &
                                                           catalogoMunicipios$CLAVE_ENTIDAD == una_entidad)]
}else{
  una_entidad <- catalogoMunicipios$CLAVE_ENTIDAD[un_municipio]
  un_municipio <- catalogoMunicipios$CLAVE_MUNICIPIO[un_municipio]
}

# Emptying space
period_plotted <- 90 # Number of bars
una_fecha <- today - period_plotted + 1

# Filtering by municipality, reducing columns
subdata <- data %>%
  filter(MUNICIPIO_RES == un_municipio & ENTIDAD_RES == una_entidad) %>%
  select(EDAD, RESULTADO_LAB, CLASIFICACION_FINAL, SEXO, TIPO_PACIENTE, FECHA_SINTOMAS)
# Creating a variable for COVID positive by lab test or other
subdata <- subdata %>%
  mutate(COVID = ifelse(CLASIFICACION_FINAL < 4, 1, 2))
# reformatting
subdata$FECHA_SINTOMAS <- as.Date(subdata$FECHA_SINTOMAS)
# Filtering by date of symptoms
subdata_recent <- subdata %>%
  filter(FECHA_SINTOMAS >= una_fecha)

# Positive cases
tabla_pos <- table(subdata_recent %>%
                     filter(COVID == 1) %>%
                     select(FECHA_SINTOMAS))
# Pending result
tabla_pend <- table(subdata_recent %>%
                      filter(RESULTADO_LAB == 3) %>%
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

# averaging
cases_means <- sapply(7:(period_plotted - 14), function(x){
  # x <- 7
  mean(tabla_estimated[(x-6):x])
})
cases_means_df <- data.frame(Dates = as.Date(names(tabla_estimated[7:(period_plotted - 14)])),
                             avg_mean = cases_means)
population <- 849053 # Morelia
population <- 614447 # Coyoacan
# cases_means_df$avg_mean <- cases_means_df$avg_mean*100000/population

df <- data.frame(Dates = as.Date(names(tabla_estimated)), Cases = tabla_estimated)
# df$Cases <- df$Cases*100000/population
#rownames(df) <- NULL

(q <- ggplot() +
    geom_bar(stat = "identity", aes(x = df$Dates, y = df$Cases, colour = "Inicio de síntomas")) +
    geom_point(aes(x = cases_means_df$Dates, y = cases_means_df$avg_mean, colour = "Promedio 7-días")) +
    geom_line(aes(x = cases_means_df$Dates, y = cases_means_df$avg_mean, colour = "Promedio 7-días")) +
#    geom_rect(aes(xmin = df$Dates[nrow(df) - uncertain_period + 1], xmax = df$Dates[nrow(df)] + 0,
    geom_rect(aes(xmin = df$Dates[nrow(df) - uncertain_period + 2], xmax = today,
                  ymin = 0, ymax = (max(df$Cases) + 2)),
              alpha = 0.3, fill = "tomato") +
    annotate(geom = "text", x = df$Dates[nrow(df)] - 4.5, y = (max(df$Cases) - 5),
             label = "Estos valores", color = "black", size = 3) +
    annotate(geom = "text", x = df$Dates[nrow(df)] - 4.5, y = (max(df$Cases) - 10),
             label = "pueden aumentar", color = "black", size = 3) +
    scale_x_date(date_labels = "%b %d", date_breaks = "2 days") +
    scale_colour_manual(values = c("black", "green4")) +
    labs(x = "Fecha", y = "Número de casos estimados", colour = "") +
    ggtitle(paste0("Casos estimados por fecha de inicio de síntomas en ", municipality)) +
#    theme(legend.position = c(0.85, 0.7),
    theme(legend.position = c(0.15, 0.88),
          legend.background = element_rect(fill = "lightblue"),
          legend.title = element_blank(),
#          legend.background = element_blank(),
          legend.key = element_rect(fill = NULL, color = NULL),
          plot.title = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"),
          plot.background = element_rect(fill = "lightblue")))

png(paste0("COVID_estimados_", municipality, ".png"), width = 700, height = 400, units = "px", res = 100)
print(p)
dev.off()

# Plot of cases in <50 year old individuals
library(tidyverse)
EDAD_lim <- 30
today <- Sys.Date() - 1
data <- read.csv(paste0("~/Documents/Personal/others/", substr(gsub("-", "", today), 3, 8),
                        "COVID19MEXICO.csv"), header = TRUE, quote = "\"", sep = ",")
# Calculating average positivity in the last 7 days
tolerance_for_tests <- 7
uncertain_period <- 14
young <- data %>%
  filter(EDAD < EDAD_lim)
daily_positivities <- t(sapply(1:tolerance_for_tests, function(x){
  oneday_pos <- length(which(young$FECHA_INGRESO == as.character(today - x + 1) &
                               young$RESULTADO_LAB == 1))
  # Counting the negative tests
  oneday_neg <- length(which(young$FECHA_INGRESO == as.character(today - x + 1) &
                               young$RESULTADO_LAB == 2))
  toReturn <- c(oneday_pos, (oneday_pos + oneday_neg))
  return(toReturn)
}))
(positivity <- sum(daily_positivities[ ,1]) / sum(daily_positivities[ ,2]))
# Creating a variable for COVID positive by lab test or other
subdata <- young %>%
  mutate(COVID = ifelse(CLASIFICACION_FINAL < 4, 1, 2))
# reformatting
subdata$FECHA_SINTOMAS <- as.Date(subdata$FECHA_SINTOMAS)
period_plotted <- 90 # Number of bars
una_fecha <- today - period_plotted + 1
# Filtering by date of symptoms
subdata_recent <- subdata %>%
  filter(FECHA_SINTOMAS >= una_fecha)
# Positive cases
tabla_pos <- table(subdata_recent %>%
                     filter(COVID == 1) %>%
                     select(FECHA_SINTOMAS))
# Pending result
tabla_pend <- table(subdata_recent %>%
                      filter(RESULTADO_LAB == 3) %>%
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
# averaging
cases_means <- sapply(7:(period_plotted - 14), function(x){
  # x <- 7
  mean(tabla_estimated[(x-6):x])
})
cases_means_df <- data.frame(Dates = as.Date(names(tabla_estimated[7:(period_plotted - 14)])),
                             avg_mean = cases_means)
df <- data.frame(Dates = as.Date(names(tabla_estimated)), Cases = tabla_estimated)
(q <- ggplot() +
    geom_bar(stat = "identity", aes(x = df$Dates, y = df$Cases, colour = "Inicio de síntomas")) +
    geom_point(aes(x = cases_means_df$Dates, y = cases_means_df$avg_mean, colour = "Promedio 7-días")) +
    geom_line(aes(x = cases_means_df$Dates, y = cases_means_df$avg_mean, colour = "Promedio 7-días")) +
    #    geom_rect(aes(xmin = df$Dates[nrow(df) - uncertain_period + 1], xmax = df$Dates[nrow(df)] + 0,
    geom_rect(aes(xmin = df$Dates[nrow(df) - uncertain_period + 2], xmax = today,
                  ymin = 0, ymax = (max(df$Cases) + 2)),
              alpha = 0.3, fill = "tomato") +
    annotate(geom = "text", x = df$Dates[nrow(df)] - 5.5, y = (max(df$Cases) * 0.95),
             label = "Estos valores", color = "black", size = 3) +
    annotate(geom = "text", x = df$Dates[nrow(df)] - 5.5, y = (max(df$Cases) * 0.85),
             label = "pueden aumentar", color = "black", size = 3) +
    scale_x_date(date_labels = "%b %d", date_breaks = "2 days") +
    scale_colour_manual(values = c("black", "green4")) +
    labs(x = "Fecha", y = "Número de casos estimados", colour = "") +
    ggtitle(paste0("Casos estimados por fecha de inicio de síntomas en Mexico, menores de ", EDAD_lim)) +
    theme(legend.position = c(0.15, 0.88),
          legend.background = element_rect(fill = "lightblue"),
          legend.title = element_blank(),
          #          legend.background = element_blank(),
          legend.key = element_rect(fill = NULL, color = NULL),
          plot.title = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.background = element_rect(fill = "lightblue",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"),
          plot.background = element_rect(fill = "lightblue")))

