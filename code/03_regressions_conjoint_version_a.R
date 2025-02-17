source(here::here("code/package_loading.R"))

#Cargamos los datos del reshape
conjoint_a <- readRDS("data/conjoint_va")

# Arreglos y joint distributions ------------------------------------------

table(conjoint_a$region, conjoint_a$p1_3)

# Ahora nos quedamos únicamente con bogotanos clasificando bogotanos

conjoint_a <- filter(conjoint_a, region == "Bogota")
conjoint_a <- filter(conjoint_a, p1_3 == "Bogotá")

#Me faltaba crear un pairid
conjoint_a$pairid <- paste(conjoint_a$id, conjoint_a$task, sep = "_")

# Definir las proporciones para cada combinación de income, education y sex
# Tengo que hacer este ejercicio porque con los factor todavía me da mal el resultado

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

# pAMCE -------------------------------------------------------------------
# Calculating pAMCE for y1 (proof) ----------------------------------------

data <- conjoint_a %>%
  dplyr::select(-c(y7))

# First, ensure proper contrast specification
data$class <- factor(data$class, levels = c("Low", "High"))
data$income <- factor(data$income,
                      levels = c("0 - 1,100,000",
                                 "1,100,001 - 2,600,000",
                                 "2,600,001 - 4,200,000",
                                 "4,200,001 - 6,800,000",
                                 "More than 6,800,000"))
data$education <- factor(data$education,
                         levels = c("Elementary",
                                    "High School",
                                    "Technical",
                                    "Undergraduate",
                                    "Postgraduate"))
data$experience <- factor(data$experience,
                          levels = c("Low", "Average", "High"))
data$sex <- factor(data$sex,
                   levels = c("Male", "Female"))
data$big5_translation <- factor(data$big5_translation,
                                levels = c(
                                "Extroverted",
                                "Calmed",
                                "Persistent",
                                "Genereous",
                                "Imaginative"))

# Set contrasts to treatment contrasts
contrasts(data$class) <- contr.treatment(2)
contrasts(data$income) <- contr.treatment(5)
contrasts(data$education) <- contr.treatment(5)
contrasts(data$experience) <- contr.treatment(3)
contrasts(data$sex) <- contr.treatment(2)
contrasts(data$big5_translation) <- contr.treatment(5)

data_y7 <- conjoint_a[!is.na(conjoint_a$y7), ]

# First, ensure proper contrast specification
data_y7$class <- factor(data_y7$class, levels = c("Low", "High"))
data_y7$income <- factor(data_y7$income,
                      levels = c("0 - 1,100,000",
                                 "1,100,001 - 2,600,000",
                                 "2,600,001 - 4,200,000",
                                 "4,200,001 - 6,800,000",
                                 "More than 6,800,000"))
data_y7$education <- factor(data_y7$education,
                         levels = c("Elementary",
                                    "High School",
                                    "Technical",
                                    "Undergraduate",
                                    "Postgraduate"))
data_y7$experience <- factor(data_y7$experience,
                          levels = c("Low", "Average", "High"))
data_y7$sex <- factor(data_y7$sex,
                   levels = c("Male", "Female"))
data_y7$big5_translation <- factor(data_y7$big5_translation,
                                levels = c(
                                  "Extroverted",
                                  "Calmed",
                                  "Persistent",
                                  "Genereous",
                                  "Imaginative"))

# Set contrasts to treatment contrasts
contrasts(data_y7$class) <- contr.treatment(2)
contrasts(data_y7$income) <- contr.treatment(5)
contrasts(data_y7$education) <- contr.treatment(5)
contrasts(data_y7$experience) <- contr.treatment(3)
contrasts(data_y7$sex) <- contr.treatment(2)
contrasts(data_y7$big5_translation) <- contr.treatment(5)

# Then run your pAMCE
pamce_y1 <- design_pAMCE(formula = y1 ~ income + education + sex + class + experience + big5_translation,
                         factor_name = c("class", "income", "education", "sex", "big5_translation", "experience"),
                         data = data,
                         cluster_id = data$id,
                         target_dist = target_dist,
                         target_type = "partial_joint",
                         partial_joint_name = partial_joint_name)

summary(pamce_y1)

# Conjoint Version A (Fig 3) ----------------------------------------------

# Function to calculate pAMCE for a single outcome
calculate_single_pamce <- function(outcome_var, data, target_dist, partial_joint_name) {
  formula <- as.formula(paste(outcome_var, "~ income + education + sex + class + experience + big5_translation"))

  design_pAMCE(formula = formula,
               factor_name = c("class", "income", "education", "sex", "big5_translation", "experience"),
               data = data,
               cluster_id = data$id,
               target_dist = target_dist,
               target_type = "partial_joint",
               partial_joint_name = partial_joint_name)
}

# Function to calculate pAMCEs for all outcomes
calculate_all_pamces <- function(data, target_dist, partial_joint_name) {

  # Calculate pAMCEs for y1-y6
  pamce_list <- list()
  for(i in 1:6) {
    outcome <- paste0("y", i)
    pamce_list[[outcome]] <- calculate_single_pamce(outcome, data, target_dist, partial_joint_name)
  }

  # Calculate pAMCE for y7 separately
  pamce_list[["y7"]] <- calculate_single_pamce("y7", data_y7, target_dist, partial_joint_name)

  return(pamce_list)
}

create_pamce_plot <- function(pamce_obj, outcome_title) {
  # Extract AMCE summary
  pamce_summary <- summary(pamce_obj)

  # Filter for only the factors of interest
  factors_of_interest <- c("class", "income", "education")
  results_df <- data.frame(
    variable = factor(pamce_summary$factor),
    level = pamce_summary$level,
    estimate = pamce_summary$Estimate,
    std.error = pamce_summary$`Std. Error`,
    p_value = pamce_summary$`p value`
  )

  # Filter the dataframe
  results_df <- results_df[results_df$variable %in% factors_of_interest, ]

  # Add significance stars
  results_df$stars <- ifelse(results_df$p_value < 0.01, "***",
                             ifelse(results_df$p_value < 0.05, "**",
                                    ifelse(results_df$p_value < 0.1, "*", "")))

  # Format coefficients with three decimal places
  results_df$coef_text <- sprintf("%.4f%s", results_df$estimate, results_df$stars)

  # Calculate position for text (after CI bars)
  results_df$text_position <- results_df$estimate + (1.96 * results_df$std.error) + 0.02

  # Create mapping for income levels
  income_levels <- c(
    "2" = "1,100,001 - 2,600,000",
    "3" = "2,600,001 - 4,200,000",
    "4" = "4,200,001 - 6,800,000",
    "5" = "More than 6,800,000"
  )

  # Create mapping for education levels
  education_levels <- c(
    "2" = "High School",
    "3" = "Technical",
    "4" = "Undergraduate",
    "5" = "Postgraduate"
  )

  # Relabel and reorder levels
  results_df$level <- ifelse(
    results_df$variable == "class", "High Class",
    ifelse(results_df$variable == "income",
           income_levels[results_df$level],
           ifelse(results_df$variable == "education",
                  education_levels[results_df$level],
                  results_df$level)
    )
  )

  # Create ordered factors for proper ordering
  results_df$level <- factor(
    results_df$level,
    levels = c(
      # For Class
      "High Class",
      # For Income (ascending order)
      "More than 6,800,000",
      "4,200,001 - 6,800,000",
      "2,600,001 - 4,200,000",
      "1,100,001 - 2,600,000",
      # For Education (ascending too order)
      "Postgraduate",
      "Undergraduate",
      "Technical",
      "High School"
    )
  )

  # Fix variable names for display and order
  results_df$variable <- factor(
    tools::toTitleCase(as.character(results_df$variable)),
    levels = c("Class", "Income", "Education")
  )

  # Create plot
  ggplot(results_df, aes(x = estimate, y = level)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 2) +
    geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                       xmax = estimate + 1.96*std.error),
                   height = 0.2) +
    # Add coefficient labels with adjusted position
    geom_text(aes(x = text_position, label = coef_text),
              hjust = -0.1,  # Adjust horizontal position
              size = 3) +    # Adjust text size
    facet_grid(variable ~ ., scales = "free_y", space = "free_y") +
    labs(title = outcome_title,
         x = "Population AMCE Estimate",
         y = "") +
    theme_minimal() +
    theme(panel.grid.major.y = element_line(color = "gray90"),
          strip.text.y = element_text(angle = 0),
          plot.title = element_text(size = 11),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)) +
    # Set x-axis limits symmetrically
    # With this:
    coord_cartesian(xlim = c(-0.6, 0.6))
}

library(stargazer)

create_latex_table <- function(pamce_list, data, data_y7) {
  # Create header text
  header <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Population Average Marginal Component Effects (pAMCE)}",
    "\\begin{tabular}{l*{7}{c}}",
    "\\hline\\hline",
    "& \\makecell{Stepping\\\\into other's\\\\shoes} & \\makecell{Inspires\\\\more\\\\confidence} & \\makecell{Prefer to\\\\do\\\\business} & \\makecell{Prefer to\\\\have\\\\friendship} & \\makecell{Prefer to\\\\share\\\\workplace} & \\makecell{Likely to\\\\be your\\\\boss} & \\makecell{Likely to\\\\receive\\\\letter} \\\\",
    "\\hline"
  )

  # Initialize rows
  rows <- character(0)

  # Process each outcome
  # Calculate number of factors from the first pamce object
  n_factors <- nrow(summary(pamce_list[[1]]))

  estimates <- matrix(nrow = n_factors, ncol = 7)
  std_errors <- matrix(nrow = n_factors, ncol = 7)

  for(i in 1:7) {
    summ <- summary(pamce_list[[paste0("y", i)]])
    estimates[, i] <- summ$Estimate
    std_errors[, i] <- summ$`Std. Error`
  }

  # Get factor and level names from the first pamce summary
  first_summ <- summary(pamce_list[[1]])
  row_names <- paste(first_summ$factor, first_summ$level, sep = " (")
  row_names <- paste0(row_names, ")")

  # Create rows with estimates and standard errors
  for(i in 1:nrow(estimates)) {
    est_row <- character(7)
    for(j in 1:7) {
      # Format estimate with stars based on p-value
      p_val <- 2 * (1 - pnorm(abs(estimates[i,j]/std_errors[i,j])))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))

      est_row[j] <- sprintf("%.4f%s & (%.4f)",
                            estimates[i,j],
                            stars,
                            std_errors[i,j])
    }
    # Remove the last & from the last element
    est_row[7] <- sub(" & $", "", est_row[7])

    # Add row name and combine cells
    rows <- c(rows, sprintf("%s & %s \\\\",
                            row_names[i],
                            paste(est_row, collapse = " & ")))

    # Add midrule after each factor group changes
    if(i < nrow(estimates)) {
      if(first_summ$factor[i] != first_summ$factor[i + 1]) {
        rows <- c(rows, "\\midrule")
      }
    }
  }

  # Calculate number of observations and respondents
  n_obs_y1_y6 <- nrow(data)
  n_resp_y1_y6 <- length(unique(data$id))
  n_obs_y7 <- nrow(data_y7)
  n_resp_y7 <- length(unique(data_y7$id))

  # Add sample size information
  sample_info <- c(
    "\\midrule",
    sprintf("Observations (y1-y6) & \\multicolumn{6}{c}{%d} & - \\\\", n_obs_y1_y6),
    sprintf("Respondents (y1-y6) & \\multicolumn{6}{c}{%d} & - \\\\", n_resp_y1_y6),
    sprintf("Observations (y7) & \\multicolumn{6}{c}{-} & %d \\\\", n_obs_y7),
    sprintf("Respondents (y7) & \\multicolumn{6}{c}{-} & %d \\\\", n_resp_y7)
  )

  # Create footer
  footer <- c(
    "\\hline\\hline",
    "\\multicolumn{8}{l}{\\textit{Notes:} Standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1} \\\\",
    "\\end{tabular}",
    "\\end{table}"
  )

  # Combine all parts
  latex_table <- c(
    "% Add these packages to your preamble:",
    "% \\usepackage{booktabs}",
    "% \\usepackage{makecell}",
    "",
    header,
    rows,
    sample_info,
    footer
  )

  return(paste(latex_table, collapse = "\n"))
}

# Calculate all pAMCEs
pamce_results <- calculate_all_pamces(data, target_dist, partial_joint_name)

# Create plots for each outcome
outcome_titles <- c("Which person seems more capable of stepping into the other’s shoes?",
                    "Which person inspires more confidence",
                    "Which person would you prefer to do business?",
                    "Which person would you prefer to have a friendship?",
                    "Which person would you prefer to share your workplace?",
                    "Which person is more likely to be your boss?",
                    "Which person is more likely to receive the letter?"
)

# Function to test the structure of the first result
test_first_plot <- function(pamce_results, outcome_titles) {
  plot <- create_pamce_plot(pamce_results$y1, outcome_titles[1])
  print(plot)
}

# Create all plots
plots <- lapply(1:7, function(i) {
  create_pamce_plot(pamce_results[[paste0("y", i)]], outcome_titles[i])
})

# Save plots
for(i in 1:7) {
  ggsave(
    filename = paste0("output/graphs/pamce_plot_y", i, ".png"),
    plot = plots[[i]],
    width = 8,
    height = 6,
    units = "in",
    bg = "white"
  )
}

# Create and save latex table
latex_table <- create_latex_table(pamce_results, data, data_y7)
writeLines(latex_table, "output/tables/pamce_results.tex")

# Heterogeneous Effects ------------------------------------

# First, let's set up the low_ses factor consistently
data$low_ses <- ifelse(data$ses_pca > median(data$ses_pca, na.rm = TRUE), 1, 0)
data_y7$low_ses <- ifelse(data_y7$ses_pca > median(data$ses_pca, na.rm = TRUE), 1, 0)

# Make sure to create the factor with levels in the SAME order as your target_dist
data$low_ses <- factor(data$low_ses, levels = c(0, 1), labels = c("Low", "High"))
data_y7$low_ses <- factor(data_y7$low_ses, levels = c(0, 1), labels = c("Low", "High"))

contrasts(data$low_ses) <- contr.treatment(2)
contrasts(data_y7$low_ses) <- contr.treatment(2)

# Define marginal distributions with matching levels
low_ses <- c(Low = 0.5, High = 0.5)  # Make sure levels match factor levels

# Create the target_dist list
target_dist_het <- list(
  tabla_contingencia,
  class = class,
  experience = experience,
  big5_translation = big5_translation,
  low_ses = low_ses
)

partial_joint_het <- list(c("income", "education", "sex"), "class",
                           "experience", "big5_translation", "low_ses")

# Función para efectos heterogéneos (Tabla 3)
pamce_het <- function(outcome_var, data, target_dist_het, partial_joint_het) {

  formula <- as.formula(paste(outcome_var,
                              "~ class + low_ses + class*low_ses + income +
                              education + sex + experience + big5_translation"))

  design_pAMCE(
    formula = formula,
    factor_name = c("class", "low_ses", "class*low_ses","income", "education",
                    "sex", "experience", "big5_translation"),
    data = data,
    cluster_id = data$id,
    target_dist = target_dist_het,
    target_type = "partial_joint",
    partial_joint_name = partial_joint_het,
    cross_int = FALSE  # Since this is within-profile interaction
  )
}

# pAMCEs para 6 outcomes principales
pamce_ses_list <- list()
for(i in 1:6) {
  outcome <- paste0("y", i)
  pamce_ses_list[[outcome]] <- pamce_het(outcome, data, target_dist_het, partial_joint_het)
}

# pAMCE para deferential
pamce_ses_list[["y7"]] <- pamce_het("y7", data_y7, target_dist_het, partial_joint_het)

create_ses_latex_table <- function(pamce_list, data, data_y7) {
  # Header remains the same
  header <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Heterogeneous treatment effects by respondents' SES}",
    "\\begin{tabular}{l*{7}{c}}",
    "\\hline\\hline",
    "& \\makecell{Stepping\\\\into other's\\\\shoes} & \\makecell{Inspires\\\\more\\\\confidence} & \\makecell{Prefer to\\\\do\\\\business} & \\makecell{Prefer to\\\\have\\\\friendship} & \\makecell{Prefer to\\\\share\\\\workplace} & \\makecell{Likely to\\\\be\\\\boss} & \\makecell{Likely to\\\\receive\\\\letter} \\\\",
    "\\hline"
  )

  # Get interaction effects
  interaction_estimates <- numeric(7)
  interaction_se <- numeric(7)

  for(i in 1:7) {
    model_results <- pamce_list[[paste0("y", i)]][["AMCE"]][["class*low_ses"]]
    # Get only the interaction term where type=="target" and level=="class2:low_ses2"
    int_row <- model_results[model_results$type == "target" & model_results$level == "class2:low_ses2", ]
    interaction_estimates[i] <- int_row$estimate
    interaction_se[i] <- int_row$se
    # Get interaction effects
    interaction_estimates <- numeric(7)
    interaction_se <- numeric(7)

    for(i in 1:7) {
      model_results <- pamce_list[[paste0("y", i)]][["AMCE"]][["class*low_ses"]]
      # Get only the interaction term where type=="target" and level=="class2:low_ses2"
      int_row <- model_results[model_results$type == "target" & model_results$level == "class2:low_ses2", ]
      interaction_estimates[i] <- int_row$estimate
      interaction_se[i] <- int_row$se
    }

    # Get main effects from summary
    main_effects <- summary(pamce_list[[1]])

    # Create matrices for all coefficients
    n_rows <- nrow(main_effects)
    estimates <- matrix(nrow = n_rows + 1, ncol = 7)  # +1 for interaction
    std_errors <- matrix(nrow = n_rows + 1, ncol = 7)

    # Fill matrices
    for(i in 1:7) {
      summ <- summary(pamce_list[[paste0("y", i)]])
      estimates[1:n_rows, i] <- summ$Estimate
      std_errors[1:n_rows, i] <- summ$`Std. Error`
    }

    # Add interaction row
    estimates[n_rows + 1, ] <- interaction_estimates
    std_errors[n_rows + 1, ] <- interaction_se

    # Get row names from summary and add interaction
    row_names <- paste(main_effects$factor, main_effects$level, sep = " (")
    row_names <- paste0(row_names, ")")
    row_names <- c(row_names, "Accent: High Class × SES: High")

    # Create rows
    rows <- character(0)

    # Vector to track where to put midrules
    factor_changes <- c(which(head(main_effects$factor, -1) != tail(main_effects$factor, -1)), n_rows)

    for(i in 1:(n_rows + 1)) {
      est_row <- character(7)
      for(j in 1:7) {
        # Format estimate with stars based on p-value
        p_val <- 2 * (1 - pnorm(abs(estimates[i,j]/std_errors[i,j])))
        stars <- ifelse(p_val < 0.01, "^{***}",
                        ifelse(p_val < 0.05, "^{**}",
                               ifelse(p_val < 0.1, "^{*}", "")))

        est_row[j] <- sprintf("%.4f%s & (%.4f)",
                              estimates[i,j],
                              stars,
                              std_errors[i,j])
      }

      # Remove the last & from the last element
      est_row[7] <- sub(" & $", "", est_row[7])

      # Add row name and combine cells
      rows <- c(rows, sprintf("%s & %s \\\\",
                              row_names[i],
                              paste(est_row, collapse = " & ")))

      # Add midrule after each factor group and before interaction term
      if(i %in% factor_changes || i == n_rows) {  # Changed this condition
        rows <- c(rows, "\\midrule")
      }
  }

  # Rest of function remains the same
  n_obs_y1_y6 <- nrow(data)
  n_resp_y1_y6 <- length(unique(data$id))
  n_obs_y7 <- nrow(data_y7)
  n_resp_y7 <- length(unique(data_y7$id))

  sample_info <- c(
    "\\midrule",
    sprintf("Observations (y1-y6) & \\multicolumn{6}{c}{%d} & - \\\\", n_obs_y1_y6),
    sprintf("Respondents (y1-y6) & \\multicolumn{6}{c}{%d} & - \\\\", n_resp_y1_y6),
    sprintf("Observations (y7) & \\multicolumn{6}{c}{-} & %d \\\\", n_obs_y7),
    sprintf("Respondents (y7) & \\multicolumn{6}{c}{-} & %d \\\\", n_resp_y7)
  )

  footer <- c(
    "\\hline\\hline",
    "\\multicolumn{8}{l}{\\textit{Notes:} Standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1} \\\\",
    "\\end{tabular}",
    "\\end{table}"
  )

  latex_table <- c(header, rows, sample_info, footer)
  return(paste(latex_table, collapse = "\n"))
  }
}

# Save Table 3
ses_table <- create_ses_latex_table(pamce_ses_list, data, data_y7)
writeLines(ses_table, "output/tables/ses_heterogeneous_effects.tex")

# Deferential Letter Experiment -----------------------------------------------------------------

# Nuevas variables

data_y7$def_letter <- factor(data_y7$def_letter, levels = c(0, 1), labels = c("Colloquial", "Deferential"))
contrasts(data_y7$def_letter) <- contr.treatment(2)

data_y7$high_ses <- ifelse(data_y7$ses_pca < median(data$ses_pca, na.rm = TRUE), 1, 0)
data_y7$high_ses <- factor(data_y7$high_ses, levels = c(0, 1), labels = c("High", "Low"))
contrasts(data_y7$high_ses) <- contr.treatment(2)

data_y7 <- data_y7 %>%
  mutate(estrato = factor(ifelse(p2_3 %in% 0:3, 1, 0)))
data_y7$estrato <- factor(data_y7$estrato, levels = c(0, 1), labels = c("High", "Low"))
contrasts(data_y7$estrato) <- contr.treatment(2)

# Distribuciones para calcular pAMCEs

def_letter <- c(Colloquial = 0.5, Deferential = 0.5)
high_ses <- c(High = 0.5, Low = 0.5)
estrato <- c(High = 0.203, Low = 0.797)

# Distribución agregando deferential letter
target_dist_def <- list(
  tabla_contingencia,
  class = class,
  experience = experience,
  big5_translation = big5_translation,
  def_letter = def_letter
)

partial_joint_def <- list(c("income", "education", "sex"), "class",
                          "experience", "big5_translation", "def_letter")

# Distribución con marcador SES (Versión Estrato)
target_dist_def2 <- list(
  tabla_contingencia,
  class = class,
  experience = experience,
  big5_translation = big5_translation,
  def_letter = def_letter,
  estrato = estrato
)

partial_joint_def2 <- list(c("income", "education", "sex"), "class",
                           "experience", "big5_translation", "def_letter", "estrato")

# Distribución con marcador SES (Versión PCA)
target_dist_def3 <- list(
  tabla_contingencia,
  class = class,
  experience = experience,
  big5_translation = big5_translation,
  def_letter = def_letter,
  high_ses = high_ses
)

partial_joint_def3 <- list(c("income", "education", "sex"), "class",
                          "experience", "big5_translation", "def_letter", "high_ses")

# Corriendo los modelos del deferential letter, primero el modelo base
pamce_deferential1 <- design_pAMCE(formula = y7 ~ class + income + education + sex + experience + big5_translation,
                                  factor_name = c("class"),
                                  data = data_y7,
                                  cluster_id = data_y7$id,
                                  target_dist = target_dist,
                                  target_type = "partial_joint",
                                  partial_joint_name = partial_joint_name,
                                  cross_int = FALSE)

# Agregando indicador de deferential letter

pamce_deferential2 <- design_pAMCE(formula = y7 ~ class + def_letter + class*def_letter + income + education + sex + experience + big5_translation,
                                   factor_name = c("class", "def_letter", "class*def_letter"),
                                   data = data_y7,
                                   cluster_id = data_y7$id,
                                   target_dist = target_dist_def,
                                   target_type = "partial_joint",
                                   partial_joint_name = partial_joint_def,
                                   cross_int = FALSE)

# Agregando marcador SES (estrato)
pamce_deferential3 <- design_pAMCE(formula = y7 ~ class + def_letter + class*def_letter +
                                     estrato + class*estrato + def_letter*estrato + class*def_letter*estrato +
                                     income + education + sex + experience + big5_translation,
                                   factor_name = c("class", "def_letter","class*def_letter",
                                                     "estrato","class*estrato","def_letter*estrato",
                                                     "class*def_letter*estrato"),
                                   data = data_y7,
                                   cluster_id = data_y7$id,
                                   target_dist = target_dist_def2,
                                   target_type = "partial_joint",
                                   partial_joint_name = partial_joint_def2,
                                   cross_int = FALSE)

# Marcador SES (PCA)

pamce_deferential4 <- design_pAMCE(formula = y7 ~ class + def_letter + class*def_letter +
                                     high_ses + class*high_ses + def_letter*high_ses + class*def_letter*high_ses +
                                     income + education + sex + experience + big5_translation,
                                   factor_name = c("class", "def_letter","class*def_letter",
                                                   "high_ses","class*high_ses","def_letter*high_ses",
                                                   "class*def_letter*high_ses"),
                                   data = data_y7,
                                   cluster_id = data_y7$id,
                                   target_dist = target_dist_def3,
                                   target_type = "partial_joint",
                                   partial_joint_name = partial_joint_def3,
                                   cross_int = FALSE)

create_progressive_models_table <- function(pamce_list, data_y7) {
  # Encabezado
  header <- c(
    "\\begin{table}[!htbp]",
    "\\tiny",
    "\\centering",
    "\\caption{Progressive interaction models for letter response}",
    "\\begin{tabular}{l*{4}{c}}",
    "\\hline\\hline",
    "& Model 1 & Model 2 & Model 3 & Model 4 \\\\",
    "\\hline"
  )

  # Listas para sacar resultados
  all_estimates <- list()
  all_se <- list()

  # Iterar sobre cada modelo
  for(i in 1:4) {
    model <- pamce_list[[i]]
    results <- list()

    class_effect <- model$AMCE$class[model$AMCE$class$type == "target", ]
    results$class <- list(estimate = class_effect$estimate, se = class_effect$se)

    # Sacar resultados de nuevas variables e interacciones para cada modelo (i)
    if(i >= 2) {

      def_effect <- model$AMCE$def_letter[model$AMCE$def_letter$type == "target", ]
      results$def_letter <- list(estimate = def_effect$estimate, se = def_effect$se)

      int_effect <- model$AMCE$`class*def_letter`[
        model$AMCE$`class*def_letter`$type == "target" &
          model$AMCE$`class*def_letter`$level == "class2:def_letter2", ]
      results$class_def <- list(estimate = int_effect$estimate, se = int_effect$se)
    }

    if(i == 3) {
      estrato_effect <- model$AMCE$estrato[model$AMCE$estrato$type == "target", ]
      results$estrato <- list(estimate = estrato_effect$estimate, se = estrato_effect$se)

      class_estrato_int <- model$AMCE$`class*estrato`[
        model$AMCE$`class*estrato`$type == "target" &
          grepl("class2:", model$AMCE$`class*estrato`$level), ]
      results$class_estrato <- list(estimate = class_estrato_int$estimate, se = class_estrato_int$se)

      def_estrato_int <- model$AMCE$`def_letter*estrato`[
        model$AMCE$`def_letter*estrato`$type == "target" &
          grepl("def_letter2:", model$AMCE$`def_letter*estrato`$level), ]
      results$def_estrato <- list(estimate = def_estrato_int$estimate, se = def_estrato_int$se)

      triple_int <- model$AMCE$`class*def_letter*estrato`[
        model$AMCE$`class*def_letter*estrato`$type == "target" &
          grepl("class2:def_letter2:estrato2", model$AMCE$`class*def_letter*estrato`$level), ]
      results$triple_int <- list(estimate = triple_int$estimate, se = triple_int$se)
    }

    if(i == 4) {
      ses_effect <- model$AMCE$high_ses[model$AMCE$high_ses$type == "target", ]
      results$high_ses <- list(estimate = ses_effect$estimate, se = ses_effect$se)

      class_ses_int <- model$AMCE$`class*high_ses`[
        model$AMCE$`class*high_ses`$type == "target" &
          grepl("class2:", model$AMCE$`class*high_ses`$level), ]
      results$class_ses <- list(estimate = class_ses_int$estimate, se = class_ses_int$se)

      def_ses_int <- model$AMCE$`def_letter*high_ses`[
        model$AMCE$`def_letter*high_ses`$type == "target" &
          grepl("def_letter2:", model$AMCE$`def_letter*high_ses`$level), ]
      results$def_ses <- list(estimate = def_ses_int$estimate, se = def_ses_int$se)

      triple_int <- model$AMCE$`class*def_letter*high_ses`[
        model$AMCE$`class*def_letter*high_ses`$type == "target" &
          grepl("class2:def_letter2:high_ses2", model$AMCE$`class*def_letter*high_ses`$level), ]
      results$triple_int <- list(estimate = triple_int$estimate, se = triple_int$se)
    }

    all_estimates[[i]] <- results
    all_se[[i]] <- results
  }

  # Ir agregando coeficientes a las tablas
  rows <- character(0)

  rows <- c(rows, "\\textbf{Main Effects} \\\\")

  # Formatear coeficientes y SE's
  format_coef <- function(est, se, stars) {
    sprintf("%.3f%s \\\\ (%.3f)", est, stars, se)
  }

  class_row <- "High Class"
  for(i in 1:4) {
    est <- all_estimates[[i]]$class$estimate
    se <- all_estimates[[i]]$class$se
    p_val <- 2 * (1 - pnorm(abs(est/se)))
    stars <- ifelse(p_val < 0.01, "^{***}",
                    ifelse(p_val < 0.05, "^{**}",
                           ifelse(p_val < 0.1, "^{*}", "")))
    class_row <- paste0(class_row, " & ", format_coef(est, se, stars))
  }
  rows <- c(rows, paste0(class_row, " \\\\"))

  def_row <- "Deferential Letter"
  for(i in 1:4) {
    if(i >= 2 && !is.null(all_estimates[[i]]$def_letter)) {
      est <- all_estimates[[i]]$def_letter$estimate
      se <- all_estimates[[i]]$def_letter$se
      p_val <- 2 * (1 - pnorm(abs(est/se)))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))
      def_row <- paste0(def_row, " & ", format_coef(est, se, stars))
    } else {
      def_row <- paste0(def_row, " & -")
    }
  }
  rows <- c(rows, paste0(def_row, " \\\\"))

  ses_row <- "High SES"
  for(i in 1:4) {
    if(i == 3 && !is.null(all_estimates[[i]]$estrato)) {
      est <- all_estimates[[i]]$estrato$estimate
      se <- all_estimates[[i]]$estrato$se
      p_val <- 2 * (1 - pnorm(abs(est/se)))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))
      ses_row <- paste0(ses_row, " & ", format_coef(est, se, stars))
    } else if(i == 4 && !is.null(all_estimates[[i]]$high_ses)) {
      est <- all_estimates[[i]]$high_ses$estimate
      se <- all_estimates[[i]]$high_ses$se
      p_val <- 2 * (1 - pnorm(abs(est/se)))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))
      ses_row <- paste0(ses_row, " & ", format_coef(est, se, stars))
    } else {
      ses_row <- paste0(ses_row, " & -")
    }
  }
  rows <- c(rows, paste0(ses_row, " \\\\"))

  rows <- c(rows, "\\midrule")

  rows <- c(rows, "\\textbf{Two-way Interactions} \\\\")

  def_int_row <- "High Class × Deferential"
  for(i in 1:4) {
    if(i >= 2 && !is.null(all_estimates[[i]]$class_def)) {
      est <- all_estimates[[i]]$class_def$estimate
      se <- all_estimates[[i]]$class_def$se
      p_val <- 2 * (1 - pnorm(abs(est/se)))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))
      def_int_row <- paste0(def_int_row, " & ", format_coef(est, se, stars))
    } else {
      def_int_row <- paste0(def_int_row, " & -")
    }
  }
  rows <- c(rows, paste0(def_int_row, " \\\\"))

  ses_int_row <- "High Class × High SES"
  for(i in 1:4) {
    if(i == 3 && !is.null(all_estimates[[i]]$class_estrato)) {
      est <- all_estimates[[i]]$class_estrato$estimate
      se <- all_estimates[[i]]$class_estrato$se
      p_val <- 2 * (1 - pnorm(abs(est/se)))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))
      ses_int_row <- paste0(ses_int_row, " & ", format_coef(est, se, stars))
    } else if(i == 4 && !is.null(all_estimates[[i]]$class_ses)) {
      est <- all_estimates[[i]]$class_ses$estimate
      se <- all_estimates[[i]]$class_ses$se
      p_val <- 2 * (1 - pnorm(abs(est/se)))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))
      ses_int_row <- paste0(ses_int_row, " & ", format_coef(est, se, stars))
    } else {
      ses_int_row <- paste0(ses_int_row, " & -")
    }
  }
  rows <- c(rows, paste0(ses_int_row, " \\\\"))

  def_ses_int_row <- "Deferential × High SES"
  for(i in 1:4) {
    if(i == 3 && !is.null(all_estimates[[i]]$def_estrato)) {
      est <- all_estimates[[i]]$def_estrato$estimate
      se <- all_estimates[[i]]$def_estrato$se
      p_val <- 2 * (1 - pnorm(abs(est/se)))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))
      def_ses_int_row <- paste0(def_ses_int_row, " & ", format_coef(est, se, stars))
    } else if(i == 4 && !is.null(all_estimates[[i]]$def_ses)) {
      est <- all_estimates[[i]]$def_ses$estimate
      se <- all_estimates[[i]]$def_ses$se
      p_val <- 2 * (1 - pnorm(abs(est/se)))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))
      def_ses_int_row <- paste0(def_ses_int_row, " & ", format_coef(est, se, stars))
    } else {
      def_ses_int_row <- paste0(def_ses_int_row, " & -")
    }
  }
  rows <- c(rows, paste0(def_ses_int_row, " \\\\"))

  rows <- c(rows, "\\midrule")

  rows <- c(rows, "\\textbf{Triple Interaction} \\\\")

  triple_int_row <- "High Class × Deferential × High SES"
  for(i in 1:4) {
    if(i == 3 && !is.null(all_estimates[[i]]$triple_int)) {
      est <- all_estimates[[i]]$triple_int$estimate
      se <- all_estimates[[i]]$triple_int$se
      p_val <- 2 * (1 - pnorm(abs(est/se)))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))
      triple_int_row <- paste0(triple_int_row, " & ", format_coef(est, se, stars))
    } else if(i == 4 && !is.null(all_estimates[[i]]$triple_int)) {
      est <- all_estimates[[i]]$triple_int$estimate
      se <- all_estimates[[i]]$triple_int$se
      p_val <- 2 * (1 - pnorm(abs(est/se)))
      stars <- ifelse(p_val < 0.01, "^{***}",
                      ifelse(p_val < 0.05, "^{**}",
                             ifelse(p_val < 0.1, "^{*}", "")))
      triple_int_row <- paste0(triple_int_row, " & ", format_coef(est, se, stars))
    } else {
      triple_int_row <- paste0(triple_int_row, " & -")
    }
  }
  rows <- c(rows, paste0(triple_int_row, " \\\\"))

  sample_info <- c(
    "\\midrule",
    sprintf("Observations & %d & %d & %d & %d \\\\",
            nrow(data_y7), nrow(data_y7), nrow(data_y7), nrow(data_y7)),
    sprintf("Respondents & %d & %d & %d & %d \\\\",
            length(unique(data_y7$id)), length(unique(data_y7$id)),
            length(unique(data_y7$id)), length(unique(data_y7$id)))
  )

  # Notas al pie
  footer <- c(
    "\\hline\\hline",
    "\\multicolumn{5}{l}{\\textit{Notes:} Standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1} \\\\",
    "\\multicolumn{5}{l}{\\textit{Models:} (1) Class only, (2) + Deferential letter, (3) + Estrato, (4) + High SES (PCA)} \\\\",
    "\\end{tabular}",
    "\\end{table}"
  )

  latex_table <- c(header, rows, sample_info, footer)
  return(paste(latex_table, collapse = "\n"))
}

# Aplicando la función
progressive_table <- create_progressive_models_table(
  list(pamce_deferential1, pamce_deferential2, pamce_deferential3, pamce_deferential4),
  data_y7
)
writeLines(progressive_table, "output/tables/progressive_models.tex")











