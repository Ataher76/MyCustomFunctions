#' Perform Three-Way ANOVA Analysis
#'
#' This function conducts a three-way ANOVA analysis on the given dataset, calculates Tukey's HSD post-hoc tests,
#' estimates marginal means, and generates a plot visualizing the results.
#'
#' @param data A data frame containing four columns: three factors (categorical variables) and one numeric variable.
#' The first three columns must be factors, and the fourth column must be numeric.
#'
#' @return A list containing:
#' \item{ANOVA_Summary}{Summary of the ANOVA model, including F-values and p-values.}
#' \item{TukeyHSD}{Results of Tukey's HSD post-hoc test for multiple comparisons.}
#' \item{Estimated_Marginal_Means}{Estimated marginal means for factor combinations.}
#' \item{Compact_Letters_Display}{A compact letter display of group differences for easy interpretation.}
#' \item{Plot}{A ggplot object showing the interaction effect and group comparisons.}
#'
#' @details The function performs a three-way ANOVA, tests for significant interactions, and provides estimated
#' marginal means for all factor combinations. It also includes a visualization of results, highlighting significant
#' group differences using compact letter displays.
#'
#' @examples
#' # Example dataset
#' data <- data.frame(
#'   Factor1 = rep(c("A", "B"), each = 12),
#'   Factor2 = rep(c("X", "Y"), each = 6, times = 2),
#'   Factor3 = rep(c("P", "Q"), times = 12),
#'   NumericVar = rnorm(24)
#' )
#'
#' results <- ThreeWayAnova(data)
#' print(results$ANOVA_Summary)
#' print(results$Plot)
#'
#' @import ggplot2 emmeans multcomp
#' @export
#' 

ThreeWayAnova <- function(data) {
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
  if (ncol(data) != 4) {
    stop("Data should have exactly four columns: three factors and a numeric variable.")
  }
  
  # Define column names for easy access
  factor1 <- names(data)[[1]]
  factor2 <- names(data)[[2]]
  factor3 <- names(data)[[3]]
  numeric_var <- names(data)[[4]]
  
  # Ensure the first three columns are factors
  data[[factor1]] <- as.factor(data[[factor1]])
  data[[factor2]] <- as.factor(data[[factor2]])
  data[[factor3]] <- as.factor(data[[factor3]])
  
  # Check that the numeric variable is numeric
  if (!is.numeric(data[[numeric_var]])) {
    stop(paste(numeric_var, "must be a numeric variable."))
  }
  
  # Run Three-Way ANOVA
  formula <- as.formula(paste(numeric_var, "~", factor1, "*", factor2, "*", factor3))
  aov_model <- aov(formula, data = data)
  aov_summary <- summary(aov_model) # Summary of the ANOVA model
  
  # Tukey's HSD post-hoc test for interaction and main effects
  TukeySHD <- TukeyHSD(aov_model)
  
  # Check for reference grid availability
  tryCatch({
    emmean <- emmeans(aov_model, specs = as.formula(paste("~", factor1, "*", factor2, "*", factor3)))
    emmean_cld <- cld(emmean, Letters = letters)
  }, error = function(e) {
    stop("Error in calculating estimated marginal means: ", e$message)
  })
  
  # Adjust CLD for plotting
  cld_data <- as.data.frame(emmean_cld)
  cld_data$group <- paste(cld_data[[factor1]], cld_data[[factor2]], cld_data[[factor3]], sep = "_")
  
  # Interaction Plot with Letters (for simplicity, only visualizing one factor against numeric_var)
  plot <- ggplot(data, aes_string(x = factor1, y = numeric_var, fill = factor2)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_text(data = cld_data, aes_string(x = factor1, y = "emmean", label = ".group"),
              position = position_dodge(width = 0.75), vjust = -0.5, size = 5) +
    theme_bw() +
    labs(x = factor1, y = numeric_var, title = "Three-Way ANOVA Results") +
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


# Generate synthetic data
set.seed(123)
data <- data.frame(
  Factor1 = rep(c("A", "B", "C"), each = 12),
  Factor2 = rep(rep(c("X", "Y"), each = 6), times = 3),
  Factor3 = rep(c("M", "N"), times = 18),
  NumericVar = rnorm(36, mean = 50, sd = 10)
)

# Check your function
result <- ThreeWayAnova(data)
print(result$ANOVA_Summary)
result$Plot


# Modify mtcars for testing
data <- mtcars
data$Factor1 <- as.factor(ifelse(data$cyl == 4, "Low", ifelse(data$cyl == 6, "Medium", "High")))
data$Factor2 <- as.factor(ifelse(data$gear == 4, "Manual", "Automatic"))
data$Factor3 <- as.factor(ifelse(data$am == 1, "Yes", "No"))
data$NumericVar <- data$mpg

data <- data[, c("Factor1", "Factor2", "Factor3", "NumericVar")]

# Check your function
result <- ThreeWayAnova(data)
print(result$ANOVA_Summary)
result$Plot










