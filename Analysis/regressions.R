#####################################
#   REPLICATION: ACCENTS AS CAPITAL #
#   PARTE 3: Regressions            #
#   Author: David Becerra           #
#####################################

# Cargando el paquete para los pAMCE's
library(factorEx) #Esta librería crea conflictos con algunas funciones de dplyr
library(tidyverse)
library(ggplot2)
library(ggplotify)
library(gridExtra)
library(dplyr)

#Cargamos los datos del reshape
df <- readRDS("reshaped_full_conjoint")

# Ahora nos quedamos únicamente con bogotanos clasificando bogotanos
df <- filter(df, p1_3 == "Bogotá" & region == "Bogota")
d<-df %>% select(-c(y7))
d <- na.omit(d)

#Me faltaba crear un pairid
d$pairid <- paste(d$id, d$task, sep = "_")

# Definir las proporciones para cada combinación de income, education y sex
# Nota: Los valores aquí son ejemplos y deben ajustarse según tus datos reales
proporciones <- expand.grid(
  income = c("0 - 1'100.000", "1'100.001 - 2'600.000", "2'600.001 - 4'200.000", "4'200.001 - 6'800.000", "Más de 6'800.000"),
  education = c("Básica", "Bachillerato", "Técnica - Tecnológica", "Grado Universitario", "Grado y Posgrado Universitario"),
  sex = c("Male", "Female")
)

# Asignar proporciones manualmente
# Asegúrate de que la suma total sea 1
proporciones$prop <- c(
  0.0416*0.5, 0.0168*0.5, 0.0016*0.5, 0.01*0.5, 0.00033*0.5,
  0.2580*0.5, 0.1614*0.5, 0.0115*0.5, 0.0030*0.5, 0.0013*0.5,
  0.0465*0.5, 0.0838*0.5, 0.0132*0.5, 0.004*0.5, 0.0013*0.5,
  0.0343*0.5, 0.1079*0.5, 0.0591*0.5, 0.03*0.5, 0.0191*0.5,
  0.0013*0.5, 0.0155*0.5, 0.0271*0.5, 0.0271*0.5, 0.0333*0.5,
  0.0346 *0.5, 0.0075 *0.5, 0.0003 *0.5, 0.0003 *0.5, 0.0003*0.5,
  0.2532 *0.5, 0.0814 *0.5, 0.0023 *0.5, 0.0023 *0.5, 0.0003*0.5,
  0.0918 *0.5, 0.0825 *0.5, 0.0049 *0.5, 0.0013 *0.5, 0.0019*0.5,
  0.0534 *0.5, 0.1339 *0.5, 0.0608 *0.5, 0.0278 *0.5, 0.0146*0.5,
  0.0032 *0.5, 0.0266 *0.5, 0.0446 *0.5, 0.0343 *0.5, 0.0353*0.5
)

# Crear la tabla de contingencia
tabla_contingencia <- xtabs(prop ~ income + education + sex, data = proporciones)

# Definir otras distribuciones marginales
class <- c(Low = 0.5, High = 0.5)
experience <- c(Baja = 1/3, Promedio = 1/3, Alta = 1/3)
big5_translation <- c(
  Extroverted = 0.2,
  Calmed = 0.2,
  Persistent = 0.2,
  Genereous = 0.2,
  Imaginative = 0.2
)

# Crear la lista target_dist
target_dist <- list(
  tabla_contingencia,
  class = class,
  experience = experience,
  big5_translation = big5_translation
)

partial_joint_name <- list(c("income", "education", "sex"), "class", 
                           "experience", "big5_translation")

# pAMCE me pide tener niveles similares en variables de joint distribution
vars <- c("income", "education", "sex", "class", "experience", "big5_translation")

for (var in vars) {
  cat("Revisando:", var, "\n")
  
  # Retrieve levels from data
  data_levels <- levels(d[[var]])
  
  # Retrieve levels from target_dist
  target_levels <- if (var == "income") {
    dimnames(tabla_contingencia)$income
  } else if (var == "education") {
    dimnames(tabla_contingencia)$education
  } else if (var == "sex") {
    dimnames(tabla_contingencia)$sex
  } else {
    names(get(var))
  }
  
  # Compare levels
  if (!identical(data_levels, target_levels)) {
    cat("Mismatch found!\n")
    cat("Data levels: ", paste(data_levels, collapse = ", "), "\n")
    cat("Target levels: ", paste(target_levels, collapse = ", "), "\n\n")
  } else {
    cat("Levels match!\n\n")
  }
}

d$income <- factor(d$income, levels = dimnames(tabla_contingencia)$income)
d$education <- factor(d$education, levels = dimnames(tabla_contingencia)$education)
d$sex <- factor(d$sex, levels = dimnames(tabla_contingencia)$sex)
d$class <- factor(d$class, levels = names(class))
d$experience <- factor(d$experience, levels = names(experience))
d$big5_translation <- factor(d$big5_translation, levels = names(big5_translation))

dimnames(tabla_contingencia)$income <- levels(d$income)
dimnames(tabla_contingencia)$education <- levels(d$education)
dimnames(tabla_contingencia)$sex <- levels(d$sex)

#Calculando pAMCE

# Lista de variables de resultado
outcomes <- c("y1", "y2", "y3", "y4", "y5", "y6")

# Lista para almacenar los resultados
results <- list()

# Bucle para iterar sobre cada variable de resultado
for (outcome in outcomes) {
  # Crear la fórmula dinámica
  formula <- as.formula(paste(outcome, "~ income + education + sex + class + experience + big5_translation"))
  
  # Ejecutar design_pAMCE para la variable de resultado actual
  pamce_result <- design_pAMCE(
    formula = formula,
    factor_name = c("class", "income", "education"),
    data = d,
    cluster_id = d$id,
    target_dist = target_dist,
    target_type = "partial_joint",
    partial_joint_name = partial_joint_name
  )
  
  #Graficar el resultado
  png(filename =paste0(
    "C:/Users/Lenovo/OneDrive - Universidad de los andes/TREES/Accents as Capital/Output/Graphs/pAMCE_", outcome, ".png"), width = 800, height = 600)
  plot(pamce_result, factor_name = c("class", "income", "education"),
       , mar = 22, xlim  = c(-0.5,0.5))
  dev.off()
  
  # Almacenar el resultado en la lista
  results[[outcome]] <- pamce_result
}