source(here::here("code/package-loading.R"))

#Cargamos los datos del reshape
df <- readRDS("data/conjoint_va")

# Un filtro para dejar solo los datos que matchean ID's de Jorge, desmarcar este
# para los análisis posteriores a la replicación
#df <- filter(df, indicator == "Data_Jorge") #Desmarcar esto cuando vaya a correr los análisis

# Arreglos y joint distributions ------------------------------------------

# Ahora nos quedamos únicamente con bogotanos clasificando bogotanos
df <- filter(df, p1_3 == "Bogotá" & region == "Bogota")

#Me faltaba crear un pairid
df$pairid <- paste(df$id, df$task, sep = "_")

# Definir las proporciones para cada combinación de income, education y sex
# Nota: Los valores aquí son ejemplos y deben ajustarse según tus datos reales
proporciones <- expand.grid(
  income = c(
    "0 - 1,100,000",
    "1,100,001 - 2,600,000",
    "2,600,001 - 4,200,000",
    "4,200,001 - 6,800,000",
    "More than 6,800,000"
  ),
  education = c(
    "Elementary",
    "High School",
    "Technical",
    "Undergraduate",
    "Postgraduate"
  ),
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
experience <- c(Low = 1/3, Average = 1/3, High = 1/3)
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

# Ensuring factors in data match target distribution levels
factor_vars <- c("income", "education", "sex", "class", "experience", "big5_translation")

for (var in factor_vars) {
  # Extract target levels depending on whether it's partial joint or marginal
  if (var %in% c("income", "education", "sex")) {
    # partial_joint_name includes these as a joint distribution
    # Retrieve levels from the multidimensional distribution (tabla_contingencia)
    if (var == "income") target_levels <- dimnames(tabla_contingencia)$income
    if (var == "education") target_levels <- dimnames(tabla_contingencia)$education
    if (var == "sex") target_levels <- dimnames(tabla_contingencia)$sex
  } else {
    # For marginal factors
    target_levels <- names(get(var))
  }

  # Convert in data
  df[[var]] <- factor(df[[var]], levels = target_levels)

  # Check if there are any observations outside of defined levels (NA after factor assignment)
  missing_levels <- sum(is.na(df[[var]]))
  if (missing_levels > 0) {
    warning(sprintf("There are %d observations in %s that do not match target levels and will be dropped.", missing_levels, var))
  }
}

# pAMCE -------------------------------------------------------------------

# Lista de variables de resultado
outcomes <- c("y1", "y2", "y3", "y4", "y5", "y6", "y7")

# Lista para almacenar los resultados
results <- list()

# Bucle para iterar sobre cada variable de resultado
for (outcome in outcomes) {

  # Crear un dataset específico
  d <- df %>%
    dplyr::select(id, outcome, indicator, all_of(factor_vars))
  d <- drop_na(d)

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
    "output/graphs/BpAMCE_", outcome, ".png"), width = 800, height = 600)
  plot(pamce_result, factor_name = c("class", "income", "education"),
       , mar = 22, xlim  = c(-0.5,0.5))
  dev.off()

  # Almacenar el resultado en la lista
  results[[outcome]] <- pamce_result
}

# Combine all results into one data frame
plot_data <- bind_rows(
  lapply(names(results), function(outcome) {
    # results[[outcome]]$AMCE is a list by factor
    # Combine all factors for this outcome
    bind_rows(
      lapply(results[[outcome]]$AMCE, as.data.frame),
      .id = "factor"
    ) %>%
      mutate(out = outcome)
  })
)

# Now 'plot_data' has columns like: type, factor, level, estimate, se, low.95ci, high.95ci, outcome
# Filter to "target" if you only want the target pAMCE
plot_data <- plot_data %>% filter(type == "target")

plot_data$level <- factor(
  plot_data$level,
  levels = c(
    "High",
    "1,100,001 - 2,600,000",
    "2,600,001 - 4,200,000",
    "4,200,001 - 6,800,000",
    "More than 6,800,000",
    "High School",
    "Technical",
    "Undergraduate",
    "Postgraduate"
  )
)

# Assume 'plot_data' is a combined data frame for all outcomes and factors
# from your pAMCE results. It should have columns:
# outcome, factor, level, estimate, low.95ci, high.95ci, p_value

# 1. Reorder outcomes so the first four appear in the top rows and the next three in the bottom rows.
# Suppose we have outcomes: y1, y2, y3, y4, y5, y6, y7
plot_data$out <- factor(plot_data$out, levels = c("y1", "y2", "y3", "y4", "y5", "y6", "y7"))

# Alternatively, if you want two distinct panels: top panel (y1-y4) and bottom panel (y5-y7),
# you can create a "panel" variable:
plot_data <- plot_data %>%
  mutate(panel = ifelse(out %in% c("y1","y2","y3","y4"), "Top", "Bottom")) %>%
  mutate(panel = factor(panel, levels = c("Top", "Bottom")))

#p-values and stars
plot_data <- filter(plot_data, type == "target") %>%
  mutate(
    Z = estimate / se,
    p_value = 2 * pnorm(abs(Z), lower.tail = FALSE),
    sig_stars = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

plot_data <- plot_data %>%
  mutate(estimate_label = sprintf("%.4f", estimate))

plot_data$est_pval <- paste(plot_data$estimate_label, plot_data$sig_stars, sep = " ")

for (outcome in outcomes) {
  plot_data_out <- filter(plot_data, out == !!outcome)

  p <- ggplot(plot_data_out, aes(x = estimate, y = level)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = low.95ci, xmax = high.95ci), height = 0.2, color = "gray50") +
    geom_point(color = "darkblue", size = 3) +

    # Add coefficient labels directly above the points
    geom_text(aes(label = est_pval), nudge_y = 0.3, size = 4, color = "black", hjust = 0.5) +

    facet_wrap(~ factor, scales = "free", ncol = 1, shrink = TRUE,axis.labels = "all_y") +
    theme_minimal(base_size = 14) +
    labs(
      x = "Estimate",
      y = NULL,
      title = paste("Population Average Marginal Component Effects -", outcome)
    ) +
    theme(
      strip.text.y = element_text(angle = 0),
      panel.grid.major.y = element_line(linetype = "dotted")
    ) +

    # Fix x-axis limits
    xlim(-0.45, 0.45)

  # Save the plot
  ggsave(
    filename = paste0("output/graphs/pAMCE_", outcome, ".png"),
    plot = p,
    width = 10,
    height = 7,
    bg = "white"
  )
}










