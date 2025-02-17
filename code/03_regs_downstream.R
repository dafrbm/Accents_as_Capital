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
patterns <- c("time", "Location", "clicks", "X", "attrib", "Date", "Recipient")
dump <- c(na_cols, unlist(lapply(patterns, function(p) grep(p, names(df), value = TRUE, ignore.case = TRUE))))

# 6. Remove specified columns
df <- df %>% dplyr::select(-all_of(unique(dump)))

df <- df %>% dplyr::select(-c(Status, IPAddress, Progress, Duration..in.seconds.,
                              DistributionChannel, UserLanguage, comments_1, ipad_email))

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

df$Treatment <- factor(df$Treatment)
summary(df$Treatment)

saveRDS(df, file = "data/downstream_effects")

# Cargar las librerías necesarias
library(tidyverse)
library(estimatr)
library(broom)
library(kableExtra)
library(sandwich)

df <- readRDS("data/downstream_effects")

# Función para transformar las variables
transformar_variables <- function(df) {
  df_transformed <- df %>%
    mutate(
      # Transformar variables DE.1_X (escala 1-5)
      across(starts_with("DE.1_"), ~case_when(
        . == "1 = Nada" ~ 1,
        . == "2" ~ 2,
        . == "3" ~ 3,
        . == "4" ~ 4,
        . == "5 = Mucho" ~ 5,
        . == "No sabe" ~ NA_real_,
        TRUE ~ NA_real_
      )),

      # Transformar DE.2 (probabilidad billetera)
      DE.2 = case_when(
        DE.2 == "Bastante alta" ~ 5,
        DE.2 == "Alta" ~ 4,
        DE.2 == "Baja" ~ 3,
        DE.2 == "Bastante baja" ~ 2,
        DE.2 == "Cercana a 0" ~ 1,
        DE.2 == "No sabe" ~ NA_real_,
        TRUE ~ NA_real_
      ),

      # DE.3 (optimismo económico)
      DE.3 = case_when(
        DE.3 == "Mejor" ~ 1,
        DE.3 == "Igual" ~ 0,
        DE.3 == "Peor" ~ 0,
        DE.3 == "No sabe" ~ NA_real_,
        TRUE ~ NA_real_
      ),

      # DE.4 (Social mobility perception)
      DE.4 = case_when(
        DE.4 == "Bastante alta" ~ 5,
        DE.4 == "Alta" ~ 4,
        DE.4 == "Baja" ~ 3,
        DE.4 == "Bastante baja" ~ 2,
        DE.4 == "Cercana a 0" ~ 1,
        DE.4 == "No sabe" ~ NA_real_,
        TRUE ~ NA_real_
      ),

      DE.5 = case_when(
        DE.5 == "Porque trabajó más duro que otros" ~ 0,
        DE.5 == "Porque tuvo más ventajas que otros" ~ 1,
        DE.5 == "No sabe" ~ NA_real_,
        TRUE ~ NA_real_
      ),

      DE.6_3 = case_when(
        DE.6_3 == "1" ~ 1,
        DE.6_3 == "2" ~ 0,
        DE.6_3 == "3" ~ 0,
        DE.6_3 == "4" ~ 0,
        TRUE ~ NA_real_
      ),

      # Crear variables de tratamiento
      treatment_AB = ifelse(Treatment == "Solo tabla", 0, 1),

      treatment_A = case_when(
        Treatment == "Solo tabla" ~ 0,
        Treatment == "Conjoint" ~ 1,
        TRUE ~ NA_real_
      )
    )

  # Crear índices después de la transformación
  df_transformed <- df_transformed %>%
    mutate(
      # Índice de familismo
      indice_familismo = ((DE.1_2 + DE.1_3)/2) - ((DE.1_1 + DE.1_4)/2),

      # Índice de confianza institucional
      indice_institucional = (DE.1_5 + DE.1_6)/2
    )

  return(df_transformed)
}

# Función para ejecutar regresión y obtener estadísticas
ejecutar_regresion_completa <- function(df, var_resultado, var_treatment) {
  # Subset de datos para el tratamiento específico
  df_analysis <- df %>%
    filter(!is.na(!!sym(var_treatment)))

  # Regresión
  reg <- lm_robust(
    formula = as.formula(paste(var_resultado, "~", var_treatment)),
    data = df_analysis,
    se_type = "HC2"
  )

  # Estadísticas de control
  control_stats <- df_analysis %>%
    filter(!!sym(var_treatment) == 0) %>%
    summarise(
      control_mean = mean(!!sym(var_resultado), na.rm = TRUE),
      control_sd = sd(!!sym(var_resultado), na.rm = TRUE)
    )

  # Número de observaciones
  n_obs <- nobs(reg)

  # R-cuadrado
  r2 <- reg$r.squared

  # P-valor
  p_value <- reg$p.value[2]

  list(
    coefficient = coef(reg)[2],
    se = reg$std.error[2],
    p_value = p_value,
    control_mean = control_stats$control_mean,
    control_sd = control_stats$control_sd,
    n_obs = n_obs,
    r2 = r2
  )
}

# Función para ejecutar análisis completo
ejecutar_analisis_tabla <- function(df) {
  # Variables de resultado en orden
  vars_resultado <- c(
    "indice_familismo",
    "DE.2",
    "indice_institucional",
    "DE.3",
    "DE.4",
    "DE.5",
    "DE.6_3"
  )

  # Nombres descriptivos
  nombres_vars <- c(
    "Index of familism",
    "Probability of returning a lost wallet",
    "Index of Institutional trust",
    "Optimism future",
    "Social mobility perceptions in Colombia",
    "Low meritocracy",
    "Inequality"
  )

  # Panel A: A + B vs. C
  resultados_AB <- map(vars_resultado, ~ejecutar_regresion_completa(df, .x, "treatment_AB"))

  # Panel B: A vs. C
  resultados_A <- map(vars_resultado, ~ejecutar_regresion_completa(df, .x, "treatment_A"))

  # Crear tabla de resultados
  crear_panel <- function(resultados, panel_name) {
    tibble(
      Panel = panel_name,
      Variable = nombres_vars,
      Coefficient = map_dbl(resultados, ~.x$coefficient),
      SE = map_dbl(resultados, ~.x$se),
      P_value = map_dbl(resultados, ~.x$p_value),
      Control_Mean = map_dbl(resultados, ~.x$control_mean),
      Control_Sd = map_dbl(resultados, ~.x$control_sd),
      N = map_dbl(resultados, ~.x$n_obs),
      R2 = map_dbl(resultados, ~.x$r2)
    )
  }

  bind_rows(
    crear_panel(resultados_AB, "Panel A. A + B vs. C"),
    crear_panel(resultados_A, "Panel B. A vs. C")
  )
}

# Modificar la función crear_tabla_final
crear_tabla_final <- function(resultados) {
  # Primero, reorganizar los datos para tener los outcomes en columnas
  tabla_wide <- resultados %>%
    mutate(
      Estimate = sprintf("%.4f%s\n(%.4f)",
                         Coefficient,
                         case_when(
                           P_value < 0.01 ~ "***",
                           P_value < 0.05 ~ "**",
                           P_value < 0.1 ~ "*",
                           TRUE ~ ""
                         ),
                         SE)
    ) %>%
    group_by(Panel) %>%
    summarise(
      `Class-based accents = 1` = list(Estimate),
      `Control Mean` = list(round(Control_Mean, 3)),
      `Control Sd` = list(round(Control_Sd, 3)),
      Observations = list(N),
      R2 = list(round(R2, 4)),
      `P-value` = list(round(P_value, 4))
    )

  # Crear la tabla con kable
  tabla_formateada <- tabla_wide %>%
    kable(
      format = "latex",
      booktabs = TRUE,
      caption = "Table 5: Downstream effects",
      align = c("l", "c", "c", "c", "c", "c", "c")
    ) %>%
    kable_styling(
      latex_options = c("striped", "scale_down"),
      full_width = TRUE
    ) %>%
    add_header_above(c(" " = 1,
                       "Index of\nfamilism" = 1,
                       "Probability of\nreturning a\nlost wallet" = 1,
                       "Index of\nInstitutional\ntrust" = 1,
                       "Optimism\nfuture" = 1,
                       "Social mobility\nperceptions in\nColombia" = 1,
                       "Low\nmeritocracy" = 1,
                       "Inequality" = 1))

  # Guardar en archivo LaTeX
  writeLines(tabla_formateada, "output/tables/tabla_downstream.tex")

  return(tabla_formateada)
}

# Ejecutar nuevamente
df_transformed<-transformar_variables(df)

resultados_finales <- ejecutar_analisis_tabla(df_transformed)

tabla_final <- crear_tabla_final(resultados_finales)

# También crear una versión en HTML para visualización
tabla_html <- crear_tabla_final(resultados_finales) %>%
  kable(format = "html") %>%
  kable_styling(bootstrap_options = "striped")

print(tabla_html)

# Selección LASSO para las covariables

