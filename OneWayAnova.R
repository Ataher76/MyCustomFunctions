OneWayAnova <- function(data) {
  # Load necessary packages
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("emmeans", quietly = TRUE)) install.packages("emmeans")
  if (!requireNamespace("multcomp", quietly = TRUE)) install.packages("multcomp")
  
  library(devtools)
  library(readxl)
  library(ggplot2)
  library(emmeans)
  library(multcomp)
  
  # Check if data has the correct structure
  if (ncol(data) != 2) {
    stop("Data should have exactly two columns: a factor and a numeric variable.")
  }
  
  # Define column names for easy access
  factor_var <- names(data)[1]
  numeric_var <- names(data)[2]
  
  # Ensure the first column is a factor
  data[[factor_var]] <- as.factor(data[[factor_var]])
  
  # Check that the numeric variable is numeric
  if (!is.numeric(data[[numeric_var]])) {
    stop(paste(numeric_var, "must be a numeric variable."))
  }
  
  # Run ANOVA
  aov_model <- aov(as.formula(paste(numeric_var, "~", factor_var)), data = data) 
  aov_summary <- summary(aov_model) # Summary of the ANOVA model
  
  # Tukey's HSD post-hoc test
  TukeySHD <- TukeyHSD(aov_model)
  
  # Check for reference grid availability
  tryCatch({
    emmean <- emmeans(aov_model, specs = as.formula(paste("~", factor_var)))
    emmean_cld <- cld(emmean, Letters = letters)
  }, error = function(e) {
    stop("Error in calculating estimated marginal means: ", e$message)
  })
  
  # Plotting ANOVA results
  plot <- ggplot(data, aes_string(x = factor_var, y = numeric_var, fill = factor_var)) +
    geom_boxplot(show.legend = FALSE, outlier.shape = NA, alpha = 0.7) +
    geom_jitter(size = 2, width = 0.2, alpha = 0.6, color = "blue") +
    geom_violin(alpha = 0.2, show.legend = F)+ # Optional jitter for better visibility
    theme_bw() +
    labs(x = factor_var, y = numeric_var, title = "One-Way ANOVA with Tukey's HSD") +
    geom_text(data = emmean_cld, aes_string(x = factor_var, y = "upper.CL", label = ".group"),
              vjust = -0.5, size = 5) +
    theme(text = element_text(size = 12), plot.title = element_text(hjust = 0.5))
  
  # Return all relevant information as a list
  return(list(
    ANOVA_Summary = aov_summary,
    TukeyHSD = TukeySHD,
    Estimated_Marginal_Means = emmean,
    Compact_Letters_Display = emmean_cld,
    Plot = plot
  ))
}


