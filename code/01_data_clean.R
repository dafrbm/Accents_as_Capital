# Preparation -------------------------------------------------------------

source(here::here("code/package_loading.R"))

# Wrangling for master.data -----------------------------------------------------------

## Cargar y limpiar metadatos

df <- read.csv("data/raw/data_conjoint.csv",
               header = T,
               comment.char = "#")

df <- df[-c(1:2), ] #Borrar metadatos

## Limpiando columnas no necesarias o vacías

### Reemplazando celdas vacías con NA's

df <- df %>% mutate(across(where(is.character), ~ na_if(., "")))

### Identificando columnas llenas de NA's o con patrones de nombre

na_cols <- df %>% dplyr::select(where(~ all(is.na(.)))) %>% colnames()
patterns <- c("time", "Location", "clicks")
dump <- c(na_cols, unlist(lapply(patterns, function(p) grep(p, names(df), value = TRUE, ignore.case = TRUE))))

# 6. Remove specified columns
df <- df %>% dplyr::select(-all_of(unique(dump))) %>%
  dplyr::select(-c(Status, IPAddress, Progress, Duration..in.seconds., DistributionChannel, UserLanguage, comments_1, ipad_email, ipad_email_1_TEXT))

# 7. Clean up temporary objects
rm(patterns, na_cols, patterns)

## Filtros

table(df$Finished,df$P1)

df$Finished <- factor(df$Finished)
summary(df$Finished)
df <- filter(df, Finished == "True") # Surveys terminados

df$P1 <- factor(df$P1)
summary(df$P1)
df <- filter(df, P1 == "Sí") # Consintieron participar

## Comparación con sample de Jorge (agregando late-arrivals)
# Cargando ID's de Jorge

ids_jorge <- read.csv("data/raw/lista_ids_jorge")
colnames(ids_jorge)[2] <- "Consent"
ids_jorge$Consent <- factor(ids_jorge$Consent)
summary(ids_jorge$Consent)

# Creando un indicador para identificar late arrivals

df$StartDate <- as.POSIXct(df$StartDate, format = "%Y-%m-%d %H:%M:%S")

fecha_ref <- as.POSIXct("2024-06-10 21:13:19", format = "%Y-%m-%d %H:%M:%S")
df <- df %>%
  mutate (indicator = factor(
    ifelse(StartDate <= fecha_ref, 1, 0),
    levels = c(1, 0),
    labels = c("Data_Jorge", "Nuevas")
  ))
summary(df$indicator)

rm(ids_jorge, fecha_ref, time_cols)

# Para realizar el análisis, tengo 6 datasets por crear (full conjoint y los 5
# Leave-One-Out) para ahorrar memoria en el pc. Esto se saca con un loop

# Primero voy a hacer un conteo para mirar samples n
df$Treatment <- factor(df$Treatment)
summary(df$Treatment)

# Loop para separar Treatments --------------------------------------------

# Valores únicos de Treatment
treatment_val <- unique(df$Treatment)

for (treatment in treatment_val) {
  # Filtrar por la categoría
  treatment_data <- df %>% filter(Treatment == treatment)

  # Crear el archivo
  file_name <- paste0("data/raw/treatment_", treatment, ".csv")

  # Exportar CSV
  write.csv(treatment_data, file_name, row.names = FALSE)

  # Borrar el objeto
  rm(treatment_data)

  # Limpiar memoria sin usar
  gc()
}

rm(file_name, treatment, treatment_val)
