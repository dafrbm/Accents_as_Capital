#####################################
#   REPLICATION: ACCENTS AS CAPITAL #
#   Author: David Becerra           #
#####################################

# PARTE 1: DATA CLEANING
rm(list = ls())
# Fijar un path
setwd("C:/Users/Lenovo/OneDrive - Universidad de los andes/TREES/Accents as Capital/Data")

# Unos paquetes necesarios
library(readr)
library(readxl)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(stringr)
library(dplyr)

# Importar datos
df <- read.csv("data_conjoint.csv", header=T, comment.char="#")

# Exluir unas filas con info que no necesito (metadata y labels)

df <- df[-c(1:2), ] #Borrando los metadatos de Qualtrics

# ARREGLOS RÁPIDOS

# Contar y eliminar observaciones que no finalizaron survey

df$StartDate <- as.POSIXct(df$StartDate, format = "%Y-%m-%d %H:%M:%S") #Lo necesito después

df$Finished<-factor(df$Finished)
summary(df$Finished)
df <- filter(df, Finished == "True")

#COMPARANDO CON LOS DATOS DE JORGE

ids_jorge <- read.csv("C:/Users/Lenovo/Downloads/lista_ids_jorge")
colnames(ids_jorge)[2] <- "Consent"
ids_jorge$Consent<-factor(ids_jorge$Consent)
summary(ids_jorge$Consent)

#Creando un indicador para diferenciarlas (Fecha máxima en el merged (10-junio 
#21:13))

fecha_ref <- as.POSIXct("2024-06-10 21:13:19",  format = "%Y-%m-%d %H:%M:%S")
df <- df %>% 
      mutate (indicator = factor(
        ifelse(StartDate <= fecha_ref, 1, 0),
        levels = c(1,0),
        labels = c("Data_Jorge", "Nuevas")))
summary(df$indicator)

# Contar y eliminar observaciones que no consintieron participar
df$P1<-factor(df$P1)
summary(df$P1)
df <- filter(df, P1 == "Sí")
summary(df$indicator)

# Hay muchas columnas con datos del timing entre clicks (Qualtrics provee esta info)
# así que las voy a eliminar también.

# Crear una lista con todas los columnas que tengan 'time' en el nombre
time_cols <- grep("time", names(df), value = TRUE, ignore.case = TRUE)

# Removiendo las columnas del objeto
df <- df %>%
  select(-all_of(time_cols))

rm(ids_jorge, fecha_ref, time_cols)

#Un último arreglo para los NA's
df <- df %>% mutate(across(where(is.character), ~ na_if(., "")))

#GUARDO EL DATASET GRANDE

write.csv(df, "master.csv", row.names = FALSE)

# Para realizar el análisis, tengo 6 datasets por crear (full conjoint y los 5 
# Leave-One-Out) para ahorrar memoria en el pc. Esto se saca con un loop

# Primero voy a hacer un conteo para mirar samples n
df$Treatment<-factor(df$Treatment)
summary(df$Treatment)

#Loop para crear los datasets

# Valores únicos de Treatment
treatment_val <- unique(df$Treatment)

for (treatment in treatment_val) {
  # Filtrar por la categoría
  treatment_data <- df %>% filter(Treatment == treatment)
  
  # Crear el archivo
  file_name <- paste0("treatment_", treatment, ".csv")
  
  # Exportar CSV
  write.csv(treatment_data, file_name, row.names = FALSE)
  
  # Borrar el objeto
  rm(treatment_data)
  
  # Limpiar memoria sin usar
  gc()
}

rm(file_name, treatment, treatment_val)

# SOLO FULL CONJOINT

# Volviendo a cargar datos
df <- read.csv("treatment_Conjoint.csv", header=T)

# Cada versión del experimento parece tener unas columnas específicas, voy a 
# identificar las columnas vacías y a eliminarlas.

# Crear una lista con los nombres de las columnas vacías
na_cols <- df %>%
  select(where(~ all(is.na(.)))) %>%
  colnames()

# Unos patrones de nombres de variables

patterns <- c("Date", "Location", "clicks_conjoint")
attrib_cols <- names(df)[grepl("_attrib.*", names(df)) & !grepl("^c", names(df))]

# Junto todos en un objeto
dump <- c(
  na_cols,
  attrib_cols,
  unlist(lapply(patterns, function(p) grep(p, names(df), value = TRUE, ignore.case = TRUE)))
)

# Eliminar las columnas
df <- df %>% select(-all_of(unique(dump)))%>%
             select(-c(Status, IPAddress, Progress, Duration..in.seconds., 
                       Finished, DistributionChannel, UserLanguage, comments_1, 
                       ipad_email, ipad_email_1_TEXT))

rm(attrib_cols, cols_to_remove, na_cols, patterns) #Borrar los vectores de nombres

#Vamos a cambiar algunos nombres para facilitar los reshapes y otras cosillas

df <- df %>%
  rename(
    "outcome1_1" = X1_Conjoint.P1_1,
    "outcome2_1" = X1_Conjoint.P2_1,
    "outcome3_1" = X1_Conjoint.P3_1_1,
    "outcome4_1" = X1_Conjoint.P3_1_2,
    "outcome5_1" = X1_Conjoint.P3_1_3,
    "outcome6_1" = X1_Conjoint.P4_1,
    "deferential1_1" = X1_Conjoint.Def.1_1,
    "deferential2_1" = X1_Conjoint.Def.2_1,
    "outcome1_2" = X2_Conjoint.P1_1,
    "outcome2_2" = X2_Conjoint.P2_1,
    "outcome3_2" = X2_Conjoint.P3_1_1,
    "outcome4_2" = X2_Conjoint.P3_1_2,
    "outcome5_2" = X2_Conjoint.P3_1_3,
    "outcome6_2" = X2_Conjoint.P4_1,
    "deferential1_2" = X2_Conjoint.Def.1_1,
    "deferential2_2" = X2_Conjoint.Def.2_1,
    "outcome1_3" = X3_Conjoint.P1_1,
    "outcome2_3" = X3_Conjoint.P2_1,
    "outcome3_3" = X3_Conjoint.P3_1_1,
    "outcome4_3" = X3_Conjoint.P3_1_2,
    "outcome5_3" = X3_Conjoint.P3_1_3,
    "outcome6_3" = X3_Conjoint.P4_1,
    "deferential1_3" = X3_Conjoint.Def.1_1,
    "deferential2_3" = X3_Conjoint.Def.2_1,
    "outcome1_4" = X4_Conjoint.P1_1,
    "outcome2_4" = X4_Conjoint.P2_1,
    "outcome3_4" = X4_Conjoint.P3_1_1,
    "outcome4_4" = X4_Conjoint.P3_1_2,
    "outcome5_4" = X4_Conjoint.P3_1_3,
    "outcome6_4" = X4_Conjoint.P4_1,
    "deferential1_4" = X4_Conjoint.Def.1_1,
    "deferential2_4" = X4_Conjoint.Def.2_1,
    "outcome1_5" = X1_Conjoint.P1_2,
    "outcome2_5" = X1_Conjoint.P2_2,
    "outcome3_5" = X1_Conjoint.P3_2_1,
    "outcome4_5" = X1_Conjoint.P3_2_2,
    "outcome5_5" = X1_Conjoint.P3_2_3,
    "outcome6_5" = X1_Conjoint.P4_2,
    "deferential1_5" = X1_Conjoint.Def.1_2,
    "deferential2_5" = X1_Conjoint.Def.2_2,
    "outcome1_6" = X2_Conjoint.P1_2,
    "outcome2_6" = X2_Conjoint.P2_2,
    "outcome3_6" = X2_Conjoint.P3_2_1,
    "outcome4_6" = X2_Conjoint.P3_2_2,
    "outcome5_6" = X2_Conjoint.P3_2_3,
    "outcome6_6" = X2_Conjoint.P4_2,
    "deferential1_6" = X2_Conjoint.Def.1_2,
    "deferential2_6" = X2_Conjoint.Def.2_2,
    "outcome1_7" = X3_Conjoint.P1_2,
    "outcome2_7" = X3_Conjoint.P2_2,
    "outcome3_7" = X3_Conjoint.P3_2_1,
    "outcome4_7" = X3_Conjoint.P3_2_2,
    "outcome5_7" = X3_Conjoint.P3_2_3,
    "outcome6_7" = X3_Conjoint.P4_2,
    "deferential1_7" = X3_Conjoint.Def.1_2,
    "deferential2_7" = X3_Conjoint.Def.2_2,
    "outcome1_8" = X1_Conjoint.P1_3,
    "outcome2_8" = X1_Conjoint.P2_3,
    "outcome3_8" = X1_Conjoint.P3_3_1,
    "outcome4_8" = X1_Conjoint.P3_3_2,
    "outcome5_8" = X1_Conjoint.P3_3_3,
    "outcome6_8" = X1_Conjoint.P4_3,
    "deferential1_8" = X1_Conjoint.Def.1_3,
    "deferential2_8" = X1_Conjoint.Def.2_3,
    "outcome1_9" = X2_Conjoint.P1_3,
    "outcome2_9" = X2_Conjoint.P2_3,
    "outcome3_9" = X2_Conjoint.P3_3_1,
    "outcome4_9" = X2_Conjoint.P3_3_2,
    "outcome5_9" = X2_Conjoint.P3_3_3,
    "outcome6_9" = X2_Conjoint.P4_3,
    "deferential1_9" = X2_Conjoint.Def.1_3,
    "deferential2_9" = X2_Conjoint.Def.2_3,
    "outcome1_10" = X3_Conjoint.P1_3,
    "outcome2_10" = X3_Conjoint.P2_3,
    "outcome3_10" = X3_Conjoint.P3_3_1,
    "outcome4_10" = X3_Conjoint.P3_3_2,
    "outcome5_10" = X3_Conjoint.P3_3_3,
    "outcome6_10" = X3_Conjoint.P4_3,
    "deferential1_10" = X3_Conjoint.Def.1_3,
    "deferential2_10" = X3_Conjoint.Def.2_3,
    "outcome1_11" = X2_Conjoint.P1_4,
    "outcome2_11" = X2_Conjoint.P2_4,
    "outcome3_11" = X2_Conjoint.P3_4_1,
    "outcome4_11" = X2_Conjoint.P3_4_2,
    "outcome5_11" = X2_Conjoint.P3_4_3,
    "outcome6_11" = X2_Conjoint.P4_4,
    "deferential1_11" = X2_Conjoint.Def.1_4,
    "deferential2_11" = X2_Conjoint.Def.2_4,
    "outcome1_12" = X3_Conjoint.P1_4,
    "outcome2_12" = X3_Conjoint.P2_4,
    "outcome3_12" = X3_Conjoint.P3_4_1,
    "outcome4_12" = X3_Conjoint.P3_4_2,
    "outcome5_12" = X3_Conjoint.P3_4_3,
    "outcome6_12" = X3_Conjoint.P4_4,
    "deferential1_12" = X3_Conjoint.Def.1_4,
    "deferential2_12" = X3_Conjoint.Def.2_4,
    "outcome1_13" = X4_Conjoint.P1_4,
    "outcome2_13" = X4_Conjoint.P2_4,
    "outcome3_13" = X4_Conjoint.P3_4_1,
    "outcome4_13" = X4_Conjoint.P3_4_2,
    "outcome5_13" = X4_Conjoint.P3_4_3,
    "outcome6_13" = X4_Conjoint.P4_4,
    "deferential1_13" = X4_Conjoint.Def.1_4,
    "deferential2_13" = X4_Conjoint.Def.2_4,
  )

# Y escribir un .csv para guardar todo

write.csv(df, "treatment_Conjoint.csv", row.names = FALSE)


#FALTA ARREGLAR EL CÓDIGO PARA LOS LEAVE-ONE-OUT
