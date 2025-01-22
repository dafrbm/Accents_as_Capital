#####################################
#   REPLICATION: ACCENTS AS CAPITAL #
#   PARTE 2: RESHAPE               #
#   Author: David Becerra           #
#####################################

rm(list = ls())

# Unos paquetes necesarios
library(readxl)
library(readr)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(stringr)
library(janitor)
library(dplyr)

# RESHAPE FULL CONJOINT (A VERSION)
# El último paso que tengo pendiente es hacer el reshape. La unidad de análisis en
# los conjoint son los perfiles.
df <- read.csv("treatment_Conjoint.csv", header=T)

# Un filtro para dejar solo los datos que matchean ID's de Jorge, desmarcar este
# para los análisis posteriores a la replicación
df <- filter(df, indicator == "Data_Jorge")

# El proceso es el siguiente: Creo un objeto con los atributos, otro con los
# outcomes y luego hacemos merge con ID's, candidato y task

# Cargando los datos de los audios
mapping_file <- read_excel(
  "Mapeo Audios Qualtrics - audio profile characteristics.xlsx") %>%
  dplyr::select( Identifier = `URL`, Region, Sex, Class, Big5 = `Big 5`,
                 Big5_Translation = `Big 5 Translation`)

# RESHAPE PARA ATRIBUTOS
# Para este se hace en dos partes, primero necesito tener los atributos en una sola columna
re_attrib <- df %>% select(ResponseId, starts_with("c"))
re_attrib <- re_attrib %>%
  # Reshape en long para sacar nombres y valores de atributos
  pivot_longer(
    cols = matches("c\\d+_attrib\\d+_(name|cand\\d+)"),
    names_to = c("task", "attribute", "type"),
    names_pattern = "c(\\d+)_attrib(\\d+)_(name|cand\\d+)",
    values_to = "value"
  ) %>%
  # Reshape wide para sacar candidatos en dos columnas
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>%
  # Group and clean up
  group_by(ResponseId, task, attribute) %>%
  # Asegurar valores únicos
  mutate(
    name = first(na.omit(name)),
    cand1 = first(na.omit(cand1)),
    cand2 = first(na.omit(cand2))
  ) %>%
  ungroup() %>%
  # Eliminar columnas vacías
  filter(if_any(c(name, cand1, cand2), ~!is.na(.)))

# Reshape para quedar con candidatos como rows y atributos como columnas
re_attrib <- re_attrib %>% select(-c(attribute))
re_attrib <- re_attrib %>%
  pivot_longer(
    cols = starts_with("cand"),
    names_to = "candidate",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = name,
    values_from = value
  )

# Extraer únicamente el link de La columna que está guardando el código de
# Qualtrics para los audios
re_attrib <- re_attrib %>%
  mutate(Identifier = str_extract(Audios, "(?<=<source src=)[^ ]+"))

# Merge con el archivo de audios
re_attrib <- re_attrib %>%
  left_join(mapping_file, by = c("Identifier" = "Identifier"))

# Arreglando la columna de candidatos
re_attrib <- re_attrib %>%
  mutate(candidate = ifelse(candidate == "cand1", 1,
                            ifelse(candidate == "cand2", 2, NA_integer_)))

# Renombrando unas columnas y limpiando nombres
re_attrib <- re_attrib %>%
  rename(
    id = `ResponseId`,
    audios = `Audios`,
    experience = `Experiencia laboral para su edad`,
    income = `Ingreso personal`,
    education = `Nivel educativo`
  ) %>%
  janitor::clean_names()

####################################
# RESHAPE PARA LOS OUTCOMES
# Ahora hacer lo mismo pero para los outcomes, creamos dos objetos con patrones
out_cols <- names(df)[grepl("outcome", names(df))]
def_cols <- names(df)[grepl("deferential", names(df))]

# Creamos un dataframe que solo contenga esta info
choice <- df %>%
  dplyr::select(ResponseId, starts_with("P"), all_of(out_cols), all_of(def_cols)) %>%
  rename(id = `ResponseId`)

# Hacemos reshape sobre las decisiones, esta rutina la obtuve de:
# https://github.com/asdurso/How-to-Reshape-Conjoint-Data-in-R

re_choice <- choice %>%
  gather(key = task, value = score, contains("_1"), contains("_2"),
         contains("_3"), contains("_4"), contains("_5"), contains("_6"),
         contains("_7"), contains("_8"), contains("_9"), contains("_10"),
         contains("_11"), contains("_12"), contains("_13")) %>%
  separate(task, c("variable", "task", "index")) %>%
  mutate(index = ifelse(is.na(index), "", index))  %>%
  unite("variable_index", c("variable", "index"), sep = "") %>%
  spread(key = variable_index, value = score)

####################################
# Merge entre atributos y outcomes
merge_reshape <- left_join(re_attrib, re_choice, by = c("id", "task"))

# Dropear observaciones con NA's en outcomes

outcome_cols <- c("outcome1", "outcome2", "outcome3", "outcome4", "outcome5",
                  "outcome6")
merge_reshape <- merge_reshape %>% filter(if_any(all_of(outcome_cols), ~ !is.na(.)))

# Crear dummies para los outcomes que matcheen la nueva estructura
for (i in 1:6) {
  merge_reshape <- merge_reshape %>%
    mutate(
      !!paste0("Y", i) := ifelse(
        (candidate == 1 & !!sym(paste0("outcome", i)) == "Persona 1") |
          (candidate == 2 & !!sym(paste0("outcome", i)) == "Persona 2"), 1,
        ifelse(
          (candidate == 1 & !!sym(paste0("outcome", i)) == "Persona 2") |
            (candidate == 2 & !!sym(paste0("outcome", i)) == "Persona 1"), 0, NA
        )
      )
    )
}

#Transformar outcomes del deferential

merge_reshape <- merge_reshape %>%
  mutate(
    Y7 = case_when(
      (candidate == 1 & (deferential1 == "Persona 1" | deferential2 == "Persona 1")) ~ 1,
      (candidate == 2 & (deferential1 == "Persona 2" | deferential2 == "Persona 2")) ~ 1,
      (candidate == 1 & (deferential1 == "Persona 2" | deferential2 == "Persona 2")) ~ 0,
      (candidate == 2 & (deferential1 == "Persona 1" | deferential2 == "Persona 1")) ~ 0,
      TRUE ~ NA_real_
    )
  )

#Crear indicadores para el deferential experiment
merge_reshape <- merge_reshape %>%
  mutate(def_indicator = ifelse(!is.na(deferential1) | !is.na(deferential2), 1, 0)) %>%
  mutate(def_letter = ifelse(!is.na(deferential2), 1, 0))

#Sacando las columnas que ya no necesito (outcomes y deferential)

deferential_cols <- grep("deferential", names(merge_reshape), value = TRUE, ignore.case = TRUE)
dump <- c(outcome_cols, deferential_cols)

# Removiendo las columnas del objeto
merge_reshape <- merge_reshape %>%
                 dplyr::select(-all_of(dump)) %>%
                 dplyr::select(-c(identifier, audios))

# Arreglando nombres

merge_reshape <- merge_reshape %>% janitor::clean_names()

# Arreglando factors que necesito

# Fijando unos niveles para los datos
merge_reshape$experience <- factor(
                            merge_reshape$experience, levels = c("Baja", "Promedio",
                                                                 "Alta"))

merge_reshape$income <- factor(
                        merge_reshape$income,
                        levels = c("0 - 1'100.000", "1'100.001 - 2'600.000",
                                   "2'600.001 - 4'200.000", "4'200.001 - 6'800.000",
                                   "Más de 6'800.000"))

merge_reshape$education <- factor(merge_reshape$education, levels = c("Básica", "Bachillerato",
                                              "Técnica - Tecnológica",
                                              "Grado Universitario",
                                              "Grado y Posgrado Universitario"))

merge_reshape$sex <- factor(merge_reshape$sex, levels = c("Male", "Female"))

merge_reshape$class <- factor(merge_reshape$class, levels = c("Low", "High"))

merge_reshape$big5_translation <- factor(merge_reshape$big5_translation, levels = c("Extroverted", "Calmed",
                                                            "Persistent", "Genereous",
                                                            "Imaginative"))

# Define columns to convert to factors
cols_factor <- c("experience", "income", "education", "sex", "class",
                 "big5", "big5_translation", "p1_2", "p2_1",
                 "p2_2", "p2_3")

# Define baseline levels
baseline_levels <- list(
  experience = "Baja",
  income = "0 - 1'100.000",
  education = "Básica",
  sex = "Male",
  class = "Low",
  big5 = "Azul",
  big5_translation = "Extroverted",
  p1_2 = "Hombre/Masculino",
  p2_1 = "1",
  p2_2 = "Menos de 1'100.000",
  p2_3 = "1"
)

# Convert columns to factors and set baseline levels
merge_reshape[cols_factor] <- lapply(cols_factor, function(col) {
  factor_col <- factor(merge_reshape[[col]], levels = unique(merge_reshape[[col]]))
  relevel(factor_col, ref = baseline_levels[[col]])
})

# Guardar el reshaped completo
saveRDS(merge_reshape, file = "reshaped_full_conjoint")
