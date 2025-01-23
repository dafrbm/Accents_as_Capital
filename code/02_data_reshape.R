# Clean and Reshape Conjoint A --------------------------------------------

# Volviendo a cargar datos
df <- read.csv("data-raw/treatment_Conjoint.csv", header = T)

# Escoger las columnas de attributes correspondientes
attrib_cols <- names(df)[grepl("_attrib.*", names(df)) &
                           !grepl("^c", names(df))]

# Y escribir un .csv para guardar todo

write.csv(df, "data-raw/treatment_Conjoint.csv", row.names = FALSE)

# El proceso es el siguiente: Creo un objeto con los atributos, otro con los
# outcomes y luego hacemos merge con ID's, candidato y task

# Cargando los datos de los audios
mapping_file <- read_excel("data-raw/Mapeo Audios Qualtrics - audio profile characteristics.xlsx") %>%
  dplyr::select(
    Identifier = `URL`,
    Region,
    Sex,
    Class,
    Big5 = `Big 5`,
    Big5_Translation = `Big 5 Translation`
  )


# Reshape Attributes ------------------------------------------------------

# Para este se hace en dos partes, primero necesito tener los atributos en una sola columna
re_attrib <- df %>% dplyr::select(ResponseId, starts_with("c"))
re_attrib <- re_attrib %>%
  # Reshape en long para sacar nombres y valores de atributos
  pivot_longer(
    cols = matches("c\\d+_attrib\\d+_(name|cand\\d+)"),
    names_to = c("task", "attribute", "type"),
    names_pattern = "c(\\d+)_attrib(\\d+)_(name|cand\\d+)",
    values_to = "value"
  ) %>%
  # Reshape wide para sacar candidatos en dos columnas
  pivot_wider(names_from = type, values_from = value) %>%
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
  filter(if_any(c(name, cand1, cand2), ~ !is.na(.)))

# Reshape para quedar con candidatos como rows y atributos como columnas
re_attrib <- re_attrib %>% dplyr::select(-c(attribute))
re_attrib <- re_attrib %>%
  pivot_longer(cols = starts_with("cand"),
               names_to = "candidate",
               values_to = "value") %>%
  pivot_wider(names_from = name, values_from = value)

# Extraer únicamente el link de La columna que está guardando el código de
# Qualtrics para los audios
re_attrib <- re_attrib %>%
  mutate(Identifier = str_extract(Audios, "(?<=<source src=)[^ ]+"))

# Merge con el archivo de audios
re_attrib <- re_attrib %>%
  left_join(mapping_file, by = c("Identifier" = "Identifier"))

# Arreglando la columna de candidatos
re_attrib <- re_attrib %>%
  mutate(candidate = ifelse(
    candidate == "cand1",
    1,
    ifelse(candidate == "cand2", 2, NA_integer_)
  ))

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


# Reshape Outcomes --------------------------------------------------------

# Ahora hacer lo mismo pero para los outcomes, creamos dos objetos con patrones
out_cols <- names(df)[grepl("outcome", names(df))]
def_cols <- names(df)[grepl("deferential", names(df))]

# Creamos un dataframe que solo contenga esta info
choice <- df %>%
  dplyr::select(ResponseId,
                indicator,
                starts_with("P"),
                all_of(out_cols),
                all_of(def_cols)) %>%
  rename(id = `ResponseId`)

# Hacemos reshape sobre las decisiones, esta rutina la obtuve de:
# https://github.com/asdurso/How-to-Reshape-Conjoint-Data-in-R

re_choice <- choice %>%
  gather(
    key = task,
    value = score,
    contains("_1"),
    contains("_2"),
    contains("_3"),
    contains("_4"),
    contains("_5"),
    contains("_6"),
    contains("_7"),
    contains("_8"),
    contains("_9"),
    contains("_10"),
    contains("_11"),
    contains("_12"),
    contains("_13")
  ) %>%
  separate(task, c("variable", "task", "index")) %>%
  mutate(index = ifelse(is.na(index), "", index))  %>%
  unite("variable_index", c("variable", "index"), sep = "") %>%
  spread(key = variable_index, value = score)


# Merge -------------------------------------------------------------------

# Merge entre atributos y outcomes
merge_reshape <- left_join(re_attrib, re_choice, by = c("id", "task"))

# Dropear observaciones con NA's en outcomes

outcome_cols <- c("outcome1",
                  "outcome2",
                  "outcome3",
                  "outcome4",
                  "outcome5",
                  "outcome6")
merge_reshape <- merge_reshape %>% filter(if_any(all_of(outcome_cols), ~ !is.na(.)))

# Crear dummies para los outcomes que matcheen la nueva estructura
for (i in 1:6) {
  merge_reshape <- merge_reshape %>%
    mutate(!!paste0("Y", i) := ifelse(
      (candidate == 1 & !!sym(paste0("outcome", i)) == "Persona 1") |
        (candidate == 2 &
           !!sym(paste0("outcome", i)) == "Persona 2"),
      1,
      ifelse(
        (candidate == 1 & !!sym(paste0("outcome", i)) == "Persona 2") |
          (candidate == 2 &
             !!sym(paste0("outcome", i)) == "Persona 1"),
        0,
        NA
      )
    ))
}

#Transformar outcomes del deferential

merge_reshape <- merge_reshape %>%
  mutate(Y7 = case_when(
    (
      candidate == 1 &
        (deferential1 == "Persona 1" | deferential2 == "Persona 1")
    ) ~ 1,
    (
      candidate == 2 &
        (deferential1 == "Persona 2" | deferential2 == "Persona 2")
    ) ~ 1,
    (
      candidate == 1 &
        (deferential1 == "Persona 2" | deferential2 == "Persona 2")
    ) ~ 0,
    (
      candidate == 2 &
        (deferential1 == "Persona 1" | deferential2 == "Persona 1")
    ) ~ 0,
    TRUE ~ NA_real_
  ))

#Crear indicadores para el deferential experiment
merge_reshape <- merge_reshape %>%
  mutate(def_indicator = ifelse(!is.na(deferential1) |
                                  !is.na(deferential2), 1, 0)) %>%
  mutate(def_letter = ifelse(!is.na(deferential2), 1, 0))

#Sacando las columnas que ya no necesito (outcomes y deferential)

deferential_cols <- grep("deferential",
                         names(merge_reshape),
                         value = TRUE,
                         ignore.case = TRUE)
dump <- c(outcome_cols, deferential_cols)

# Removiendo las columnas del objeto
merge_reshape <- merge_reshape %>%
  dplyr::select(-all_of(dump)) %>%
  dplyr::select(-c(identifier, audios))

# Arreglando nombres

merge_reshape <- merge_reshape %>% janitor::clean_names()

# Arreglando factors que necesito

# Fijando unos niveles para los datos

merge_reshape <- merge_reshape %>%
  mutate(experience = factor(
    experience,
    levels = c("Baja", "Promedio", "Alta"),
    labels = c("Low", "Average", "High"),
    ordered = TRUE
  ))

merge_reshape$income <- factor(
  merge_reshape$income,
  levels = c(
    "0 - 1'100.000",
    "1'100.001 - 2'600.000",
    "2'600.001 - 4'200.000",
    "4'200.001 - 6'800.000",
    "Más de 6'800.000"
  ),
  labels = c(
    "0 - 1,100,000",
    "1,100,001 - 2,600,000",
    "2,600,001 - 4,200,000",
    "4,200,001 - 6,800,000",
    "More than 6,800,000"
  ),
  ordered = TRUE
)

merge_reshape$education <- factor(
  merge_reshape$education,
  levels = c(
    "Básica",
    "Bachillerato",
    "Técnica - Tecnológica",
    "Grado Universitario",
    "Grado y Posgrado Universitario"
  ),
  labels = c(
    "Elementary",
    "High School",
    "Technical",
    "Undergraduate",
    "Postgraduate"
  ),
  ordered = TRUE
)

merge_reshape$sex <- factor(merge_reshape$sex, levels = c("Male", "Female"),
                            ordered = TRUE)

merge_reshape$class <- factor(merge_reshape$class, levels = c("Low", "High"),
                              ordered = TRUE)

merge_reshape$big5_translation <- factor(
  merge_reshape$big5_translation,
  levels = c(
    "Extroverted",
    "Calmed",
    "Persistent",
    "Genereous",
    "Imaginative"
  ),
  ordered = TRUE
)

merge_reshape$p2_1 <- factor(
  merge_reshape$p2_1,
  levels = c(
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6 o más"
  ),
  ordered = TRUE
)

merge_reshape$p2_2 <- factor(
  merge_reshape$p2_2,
  levels = c(
    "Menos de 1'100.000",
    "1'100.001 - 1'700.000",
    "1'700.001 - 2'600.000",
    "2'600.001 - 4'200.000",
    "4'200.001 - 6'800.000",
    "6'800.001 - 8'400.000",
    "8'400.001 - 10'000.000",
    "Más de 10'000.001"
  ),
  ordered = TRUE
)

merge_reshape$p2_3 <- factor(
  merge_reshape$p2_3,
  levels = c(
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "Otro"
  ),
  ordered = TRUE
)

# Haciendo PCA y creando indicadores que voy a necesitar

# Creando unas nuevas variables
merge_reshape$hh_size <- as.numeric(factor(merge_reshape$p2_1, ordered = TRUE))
merge_reshape$hh_inc <- as.numeric(factor(merge_reshape$p2_2, ordered = TRUE))
merge_reshape$hh_strat <- as.numeric(factor(merge_reshape$p2_3, ordered = TRUE))

# Estandarizar las variables numéricas (debido a rangos diferentes en variables)
dat_std <- scale(merge_reshape[, c("hh_size", "hh_inc", "hh_strat")])

# Imputar medias sobre NA's
dat_std <- apply(dat_std, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Realizar PCA
resultado_pca <- prcomp(dat_std, scale. = FALSE) # Ya escalamos manualmente

# Crear nuevas variables basadas en los puntajes del primer componente principal
merge_reshape$ses_pca <- resultado_pca$x[, 1]

# Crear indicadores basados en medidas de posición
# Indicador 1: 50/50
percentil_50 <- quantile(merge_reshape$ses_pca, 0.5)
merge_reshape$ses_pca1 <- ifelse(merge_reshape$ses_pca <= percentil_50, 1, 0)

# Indicador 2: 33/67
percentil_33 <- quantile(merge_reshape$ses_pca, 1/3)
merge_reshape$ses_pca2 <- ifelse(merge_reshape$ses_pca <= percentil_33, 1, 0)

# Guardar el reshaped completo
saveRDS(merge_reshape, file = "data/conjoint_va")

#FALTA ARREGLAR EL CÓDIGO PARA LOS LEAVE-ONE-OUT

# Clean and Reshape Conjoint B Versions --------------------------------------------

# Volviendo a cargar datos
df <- read.csv("data-raw/treatment_Conjoint.csv", header = T)

# Cada versión del experimento parece tener unas columnas específicas, voy a
# identificar las columnas vacías y a eliminarlas.

# Crear una lista con los nombres de las columnas vacías
na_cols <- df %>%
  dplyr::select(where( ~ all(is.na(.)))) %>%
  colnames()

# Unos patrones de nombres de variables

patterns <- c("Date", "Location", "clicks_conjoint")
attrib_cols <- names(df)[grepl("_attrib.*", names(df)) &
                           !grepl("^c", names(df))]

# Junto todos en un objeto
dump <- c(na_cols, attrib_cols, unlist(lapply(patterns, function(p)
  grep(p, names(df), value = TRUE, ignore.case = TRUE))))

# Eliminar las columnas
df <- df %>% dplyr::select(-all_of(unique(dump))) %>%
  dplyr::select(
    -c(
      Status,
      IPAddress,
      Progress,
      Duration..in.seconds.,
      Finished,
      DistributionChannel,
      UserLanguage,
      comments_1,
      ipad_email,
      ipad_email_1_TEXT
    )
  )

rm(attrib_cols, na_cols, patterns) #Borrar los vectores de nombres

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

write.csv(df, "data-raw/treatment_Conjoint.csv", row.names = FALSE)

# Un filtro para dejar solo los datos que matchean ID's de Jorge, desmarcar este
# para los análisis posteriores a la replicación
df <- filter(df, indicator == "Data_Jorge") #Desmarcar esto cuando vaya a correr los análisis

# El proceso es el siguiente: Creo un objeto con los atributos, otro con los
# outcomes y luego hacemos merge con ID's, candidato y task

# Cargando los datos de los audios
mapping_file <- read_excel("data-raw/Mapeo Audios Qualtrics - audio profile characteristics.xlsx") %>%
  dplyr::select(
    Identifier = `URL`,
    Region,
    Sex,
    Class,
    Big5 = `Big 5`,
    Big5_Translation = `Big 5 Translation`
  )


# Reshape Attributes ------------------------------------------------------

# Para este se hace en dos partes, primero necesito tener los atributos en una sola columna
re_attrib <- df %>% dplyr::select(ResponseId, starts_with("c"))
re_attrib <- re_attrib %>%
  # Reshape en long para sacar nombres y valores de atributos
  pivot_longer(
    cols = matches("c\\d+_attrib\\d+_(name|cand\\d+)"),
    names_to = c("task", "attribute", "type"),
    names_pattern = "c(\\d+)_attrib(\\d+)_(name|cand\\d+)",
    values_to = "value"
  ) %>%
  # Reshape wide para sacar candidatos en dos columnas
  pivot_wider(names_from = type, values_from = value) %>%
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
  filter(if_any(c(name, cand1, cand2), ~ !is.na(.)))

# Reshape para quedar con candidatos como rows y atributos como columnas
re_attrib <- re_attrib %>% dplyr::select(-c(attribute))
re_attrib <- re_attrib %>%
  pivot_longer(cols = starts_with("cand"),
               names_to = "candidate",
               values_to = "value") %>%
  pivot_wider(names_from = name, values_from = value)

# Extraer únicamente el link de La columna que está guardando el código de
# Qualtrics para los audios
re_attrib <- re_attrib %>%
  mutate(Identifier = str_extract(Audios, "(?<=<source src=)[^ ]+"))

# Merge con el archivo de audios
re_attrib <- re_attrib %>%
  left_join(mapping_file, by = c("Identifier" = "Identifier"))

# Arreglando la columna de candidatos
re_attrib <- re_attrib %>%
  mutate(candidate = ifelse(
    candidate == "cand1",
    1,
    ifelse(candidate == "cand2", 2, NA_integer_)
  ))

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


# Reshape Outcomes --------------------------------------------------------

# Ahora hacer lo mismo pero para los outcomes, creamos dos objetos con patrones
out_cols <- names(df)[grepl("outcome", names(df))]
def_cols <- names(df)[grepl("deferential", names(df))]

# Creamos un dataframe que solo contenga esta info
choice <- df %>%
  dplyr::select(ResponseId,
                starts_with("P"),
                all_of(out_cols),
                all_of(def_cols)) %>%
  rename(id = `ResponseId`)

# Hacemos reshape sobre las decisiones, esta rutina la obtuve de:
# https://github.com/asdurso/How-to-Reshape-Conjoint-Data-in-R

re_choice <- choice %>%
  gather(
    key = task,
    value = score,
    contains("_1"),
    contains("_2"),
    contains("_3"),
    contains("_4"),
    contains("_5"),
    contains("_6"),
    contains("_7"),
    contains("_8"),
    contains("_9"),
    contains("_10"),
    contains("_11"),
    contains("_12"),
    contains("_13")
  ) %>%
  separate(task, c("variable", "task", "index")) %>%
  mutate(index = ifelse(is.na(index), "", index))  %>%
  unite("variable_index", c("variable", "index"), sep = "") %>%
  spread(key = variable_index, value = score)

# Merge -------------------------------------------------------------------

# Merge entre atributos y outcomes
merge_reshape <- left_join(re_attrib, re_choice, by = c("id", "task"))

# Dropear observaciones con NA's en outcomes

outcome_cols <- c("outcome1",
                  "outcome2",
                  "outcome3",
                  "outcome4",
                  "outcome5",
                  "outcome6")
merge_reshape <- merge_reshape %>% filter(if_any(all_of(outcome_cols), ~ !is.na(.)))

# Crear dummies para los outcomes que matcheen la nueva estructura
for (i in 1:6) {
  merge_reshape <- merge_reshape %>%
    mutate(!!paste0("Y", i) := ifelse(
      (candidate == 1 & !!sym(paste0("outcome", i)) == "Persona 1") |
        (candidate == 2 &
           !!sym(paste0("outcome", i)) == "Persona 2"),
      1,
      ifelse(
        (candidate == 1 & !!sym(paste0("outcome", i)) == "Persona 2") |
          (candidate == 2 &
             !!sym(paste0("outcome", i)) == "Persona 1"),
        0,
        NA
      )
    ))
}

#Transformar outcomes del deferential

merge_reshape <- merge_reshape %>%
  mutate(Y7 = case_when(
    (
      candidate == 1 &
        (deferential1 == "Persona 1" | deferential2 == "Persona 1")
    ) ~ 1,
    (
      candidate == 2 &
        (deferential1 == "Persona 2" | deferential2 == "Persona 2")
    ) ~ 1,
    (
      candidate == 1 &
        (deferential1 == "Persona 2" | deferential2 == "Persona 2")
    ) ~ 0,
    (
      candidate == 2 &
        (deferential1 == "Persona 1" | deferential2 == "Persona 1")
    ) ~ 0,
    TRUE ~ NA_real_
  ))

#Crear indicadores para el deferential experiment
merge_reshape <- merge_reshape %>%
  mutate(def_indicator = ifelse(!is.na(deferential1) |
                                  !is.na(deferential2), 1, 0)) %>%
  mutate(def_letter = ifelse(!is.na(deferential2), 1, 0))

#Sacando las columnas que ya no necesito (outcomes y deferential)

deferential_cols <- grep("deferential",
                         names(merge_reshape),
                         value = TRUE,
                         ignore.case = TRUE)
dump <- c(outcome_cols, deferential_cols)

# Removiendo las columnas del objeto
merge_reshape <- merge_reshape %>%
  dplyr::select(-all_of(dump)) %>%
  dplyr::select(-c(identifier, audios))

# Arreglando nombres

merge_reshape <- merge_reshape %>% janitor::clean_names()

# Arreglando factors que necesito

# Fijando unos niveles para los datos

merge_reshape <- merge_reshape %>%
  mutate(experience = factor(
    experience,
    levels = c("Baja", "Promedio", "Alta"),
    labels = c("Low", "Average", "High")
  ))

merge_reshape$income <- factor(
  merge_reshape$income,
  levels = c(
    "0 - 1'100.000",
    "1'100.001 - 2'600.000",
    "2'600.001 - 4'200.000",
    "4'200.001 - 6'800.000",
    "Más de 6'800.000"
  ),
  labels = c(
    "0 - 1,100,000",
    "1,100,001 - 2,600,000",
    "2,600,001 - 4,200,000",
    "4,200,001 - 6,800,000",
    "More than 6,800,000"
  )
)

merge_reshape$education <- factor(
  merge_reshape$education,
  levels = c(
    "Básica",
    "Bachillerato",
    "Técnica - Tecnológica",
    "Grado Universitario",
    "Grado y Posgrado Universitario"
  ),
  labels = c(
    "Elementary",
    "High School",
    "Technical",
    "Undergraduate",
    "Postgraduate"
  )
)

merge_reshape$sex <- factor(merge_reshape$sex, levels = c("Male", "Female"))

merge_reshape$class <- factor(merge_reshape$class, levels = c("Low", "High"))


merge_reshape$big5_translation <- factor(
  merge_reshape$big5_translation,
  levels = c(
    "Extroverted",
    "Calmed",
    "Persistent",
    "Genereous",
    "Imaginative"
  )
)
# Define columns to convert to factors
cols_factor <- c(
  "p1_2",
  "p2_1",
  "p2_2",
  "p2_3"
)

# Define baseline levels
baseline_levels <- list(
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
saveRDS(merge_reshape, file = "data/conjoint_va")
