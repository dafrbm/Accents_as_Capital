
# Preparation (target distribution and factors) ---------------------------

source(here::here("R/package-loading.R"))

#Cargamos los datos del reshape
df <- readRDS("data/conjoint_va")

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

# Loop pAMCE --------------------------------------------------------------

# Set seed for reproducibility
set.seed(123)

# Your list of outcome variables
outcomes <- c("y1", "y2", "y3", "y4", "y5", "y6", "y7")

# Lists to store results
results <- list()
bootstrap_results <- list()

# Number of bootstrap iterations
nboot <- 1000

# Identify the unit of clustering (from your dataset)
cluster_ids <- base::unique(df$id)


for (out in outcomes) {
  # Create the formula dynamically
  form <- as.formula(paste(out, "~ income + education + sex + class + experience + big5_translation"))

  # Crear un dataset específico
  d <- df %>%
    dplyr::select(id, out, income, education, sex, class, experience, big5_translation)
  d <- drop_na(d)

  # Run design_pAMCE once to get initial estimates and structure
  pamce_result <- design_pAMCE(
    formula = form,
    factor_name = c("class", "income", "education"),  # specify the factors of interest
    data = d,
    cluster_id = d$id,
    target_dist = target_dist,
    target_type = "partial_joint",
    partial_joint_name = partial_joint_name
  )

  # Store initial result
  results[[out]] <- pamce_result

  # Extract factors and levels to prepare for storing bootstrap estimates
  # pamce_result$AMCE is a list, each element corresponds to one factor
  factor_names <- names(pamce_result$AMCE)

  # For bootstrap storage:
  # We'll create a nested list structure: for each factor, store a data frame with one row per bootstrap and columns for levels
  pamce_boot <- list()

  for (f in factor_names) {
    # Filter only the "target" rows since these are the design-based pAMCE estimates
    factor_df <- pamce_result$AMCE[[f]] %>% filter(type == "target")
    # Initialize a data frame to store bootstrap estimates for this factor
    pamce_boot[[f]] <- data.frame(matrix(NA, nrow = nboot, ncol = nrow(factor_df)))
    colnames(pamce_boot[[f]]) <- factor_df$level
  }

  # Run the bootstrap loop
  for (b in 1:nboot) {
    # Resample cluster IDs with replacement
    sampled_ids <- sample(cluster_ids, size = length(cluster_ids), replace = TRUE)
    # Construct the bootstrap sample dataset
    boot_data <- d %>% semi_join(data.frame(id = sampled_ids), by = "id")

    # Re-run design_pAMCE on the bootstrap sample
    out_boot <- design_pAMCE(
      formula = form,
      factor_name = c("class", "income", "education"),
      data = boot_data,
      cluster_id = boot_data$id,
      target_dist = target_dist,
      target_type = "partial_joint",
      partial_joint_name = partial_joint_name
    )

    # Store the target pAMCE estimates for each factor
    for (f in factor_names) {
      factor_df <- out_boot$AMCE[[f]] %>% filter(type == "target")
      pamce_boot[[f]][b, ] <- factor_df$estimate
    }
  }

  # Compute bootstrap standard errors
  bootstrap_se <- lapply(pamce_boot, function(df) apply(df, 2, sd))

  # You could also compute confidence intervals here if desired:
  # For example, 95% CI using quantiles:
  # bootstrap_ci <- lapply(pamce_boot, function(df) apply(df, 2, quantile, probs = c(0.025, 0.975)))

  # Store bootstrap results for this outcome
  bootstrap_results[[out]] <- list(
    "boot_estimates" = pamce_boot,
    "boot_se" = bootstrap_se
    # "boot_ci" = bootstrap_ci
  )
}

# Now 'results' holds your original pAMCE estimates for each outcome
# and 'bootstrap_results' holds the bootstrap estimates and standard errors.

# Suppose we now create plots after we've completed the main loop and have both
# 'results' and 'bootstrap_results' available.
# We know the factors of interest:
factors_of_interest <- c("class", "income", "education")

# Create a directory for the new plots if needed
# dir.create("C:/Users/Lenovo/OneDrive - Universidad de los andes/TREES/Accents as Capital/Output/Graphs/Bootstrap", showWarnings = FALSE)

for (outcome in outcomes) {

  # Extract the original pAMCE results
  pamce_result <- results[[outcome]]

  # Extract bootstrap SEs
  boot_se_list <- bootstrap_results[[outcome]]$boot_se

  # Combine into a long data frame
  # pamce_result$AMCE is a list with one element per factor
  plot_data <- bind_rows(
    lapply(names(pamce_result$AMCE), function(f) {
      df <- pamce_result$AMCE[[f]] %>%
        filter(type == "target") %>%
        mutate(factor = f)
      # Add bootstrap SEs
      # boot_se_list[[f]] is a named vector of SEs for each level
      # Ensure that the order of levels in df matches the columns in boot_se_list[[f]]
      se_vec <- boot_se_list[[f]][colnames(boot_se_list[[f]]) %in% df$level]
      # Make sure levels align
      se_vec <- se_vec[df$level]

      df$boot_se <- se_vec
      # Compute 95% CIs using the bootstrap SE
      df <- df %>%
        mutate(low_95ci = estimate - 1.96*boot_se,
               high_95ci = estimate + 1.96*boot_se)
      return(df)
    })
  )

  # Now `plot_data` has estimate, boot_se, and CI for each factor-level.
  # Order factors so that we get class first, income second, education third
  plot_data$factor <- factor(plot_data$factor, levels = factors_of_interest)

  # Create the plot
  p <- ggplot(plot_data, aes(x = estimate, y = level)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = low.95ci, xmax = high.95ci), height = 0.2, color = "gray50") +
    geom_point(color = "darkblue", size = 3) +
    facet_wrap(~ factor, scales = "free_y", ncol = 3) +
    theme_minimal(base_size = 14) +
    labs(
      x = "Estimate",
      y = NULL,
      title = paste("Population Average Marginal Component Effects -", outcome),
      subtitle = "Bootstrap-adjusted standard errors"
    ) +
    theme(
      panel.border = element_rect(fill = NA, color = "black"), # panel border
      strip.text.y = element_text(angle = 0),
      panel.grid.major.y = element_line(linetype = "dotted")
    )

  # Save the plot
  ggsave(filename = paste0(
    "output/graphs/pAMCE_boot_",
    outcome, ".png"), plot = p, width = 10, height = 7, bg = "white")
}
