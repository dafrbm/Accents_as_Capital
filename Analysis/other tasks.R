# ALGUNAS DESCRIPTIVAS O COSAS RÁPIDAS QUE ME PIDAN
rm(list = ls())
# Fijar un path
setwd("C:/Users/Lenovo/OneDrive - Universidad de los andes/TREES/Accents as Capital/Data")

# Unos paquetes necesarios
library(readr)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(factorEx)
library(dplyr)

# 1. Late-Arrivals al Survey
# Con Jorge pudimos notar que tenemos una diferencia en el número de obs.
# Por acá documento esas diferencias

df <- read.csv("master.csv", header=TRUE)

# Contar y eliminar observaciones que no finalizaron survey

df$StartDate <- as.POSIXct(df$StartDate, format = "%Y-%m-%d %H:%M:%S") #Date format
df$Finished<-factor(df$Finished)
summary(df$Finished)
df <- filter(df, Finished == "True") #Droppeando los que no terminaron survey

# Jorge aplicó el mismo filtro y me envió una lista de ID's. Acá estoy sacando
# las mismas descriptivas y voy a hacer el pegue para echarles un ojo

ids_jorge <- read.csv("C:/Users/Lenovo/Downloads/lista_ids_jorge")
colnames(ids_jorge)[2] <- "Consent"
ids_jorge$Consent<-factor(ids_jorge$Consent)
summary(ids_jorge$Consent)

#Un nuevo objeto para comparar rápidamente

df1 <- df[, c(1:20)]
merged <- merge(df1, ids_jorge, by.x = "ResponseId", by.y = "ResponseId")

#Creando un indicador para diferenciarlas (Fecha máxima en el merged (10-junio 
#21:13))

fecha_ref <- as.POSIXct("2024-06-10 21:13:19",  format = "%Y-%m-%d %H:%M:%S")
df <- df %>% 
  mutate (indicator = factor(
    ifelse(StartDate <= fecha_ref, 1, 0),
    levels = c(1,0),
    labels = c("Datos de Jorge", "Nuevas")))
summary(df$indicator)

# Contar y eliminar observaciones que no consintieron participar
df$P1<-factor(df$P1)
summary(df$P1)
df <- filter(df, P1 == "Sí")
summary(df$indicator)

# Sacando una gráfica para ver la distribución en el tiempo de las observaciones

library(ggplot2)

ggplot(df, aes(x=StartDate, fill = indicator)) +
  geom_histogram(binwidth = 86400, color = "black", alpha = 0.7) +
  geom_vline(xintercept = as.numeric (as.POSIXct("2024-06-10 21:13:19" )),
             linetype = "dashed", color = "red", size = 0.3, alpha = 0.8) +
  labs(
    title = "Distribución de observaciones en el tiempo",
    x = "Fecha",
    y = "Conteo",
    fill = "Indicator"
  ) +
  scale_x_datetime(
    breaks  = c(
      seq(as.POSIXct("2024-03-01"), as.POSIXct("2024-05-31"), by = "1 month"),
      seq(as.POSIXct("2024-06-01"), as.POSIXct("2024-06-17"), by = "3 days")
    ),
    date_labels = "%b %d"
    ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5)
        )

# Voy a filtrar solo Junio para que se vea más claro

df_june <- df %>% 
           filter(StartDate >= as.POSIXct("2024-06-01"))

ggplot(df_june, aes(x=StartDate, fill = indicator)) +
  geom_histogram(
    binwidth = 86400,
    boundary = as.numeric(as.POSIXct("2024-06-01 00:00:00")),
    color = "black", 
    alpha = 0.7) +
  labs(
    title = "Distribución de observaciones en el tiempo",
    x = "Fecha",
    y = "Conteo",
    fill = "Indicator"
  ) +
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%b %d",
    expand = c(0,0)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,, vjust = 4, hjust = 1),
        plot.title = element_text(hjust = 0.5)
        )





