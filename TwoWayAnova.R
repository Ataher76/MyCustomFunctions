#' Two-Way ANOVA Analysis with Visualization and Post-Hoc Testing
#'
#' This function performs a two-way ANOVA on a dataset with two categorical factors 
#' and one numeric response variable. It includes Tukey's HSD post-hoc tests, 
#' calculates estimated marginal means (EMMs), and visualizes the interaction effects.
#'
#' @param data A data frame containing exactly three columns: two categorical factors 
#'   (independent variables) and one numeric response variable (dependent variable).
#'   The first two columns must be factors, and the third column must be numeric.
#'
#' @return A list containing the following elements:
#' \item{ANOVA_Summary}{Summary of the two-way ANOVA analysis.}
#' \item{TukeyHSD}{Results of Tukey's HSD post-hoc test for main effects and interaction.}
#' \item{Estimated_Marginal_Means}{Estimated marginal means (EMMs) for factor combinations.}
#' \item{Compact_Letters_Display}{Compact letter display (CLD) for significant group differences.}
#' \item{Plot}{A `ggplot` object visualizing interaction effects with significant groupings.}
#'
#' @details 
#' The function checks the input data for appropriate structure: two categorical 
#' factors and one numeric variable. It performs two-way ANOVA to assess the effects 
#' of the factors and their interaction. Tukey's HSD test identifies pairwise differences 
#' among groups. EMMs provide insights into adjusted means for factor combinations.
#'
#' The output includes a customizable `ggplot` showing interaction effects, with 
#' statistical groupings represented by letters. This allows for easy interpretation of results.
#'
#' @examples
#' # Example dataset
#' set.seed(123)
#' data <- data.frame(
#'   Factor1 = rep(c("A", "B"), each = 10),
#'   Factor2 = rep(c("X", "Y"), times = 10),
#'   Response = rnorm(20)
#' )
#'
#' # Perform Two-Way ANOVA
#' result <- TwoWayAnova(data)
#' print(result$ANOVA_Summary)
#' print(result$TukeyHSD)
#' print(result$Estimated_Marginal_Means)
#' print(result$Compact_Letters_Display)
#' print(result$Plot)
#'
#' @import ggplot2
#' @import emmeans
#' @import multcomp
#' @export


TwoWayAnova <- function(data) {
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
  if (ncol(data) != 3) {
    stop("Data should have exactly three columns: two factors and a numeric variable.")
  }
  
  # Define column names for easy access
  factor1 <- names(data)[[1]]
  factor2 <- names(data)[[2]]
  numeric_var <- names(data)[[3]]
  
  # Ensure the first two columns are factors
  data[[factor1]] <- as.factor(data[[factor1]])
  data[[factor2]] <- as.factor(data[[factor2]])
  
  # Check that the numeric variable is numeric
  if (!is.numeric(data[[numeric_var]])) {
    stop(paste(numeric_var, "must be a numeric variable."))
  }
  
  # Run Two-Way ANOVA
  formula <- as.formula(paste(numeric_var, "~", factor1, "*", factor2))
  aov_model <- aov(formula, data = data)
  aov_summary <- summary(aov_model) # Summary of the ANOVA model
  
  # Tukey's HSD post-hoc test for interaction and main effects
  TukeySHD <- TukeyHSD(aov_model)
  
  # Check for reference grid availability
  tryCatch({
    emmean <- emmeans(aov_model, specs = as.formula(paste("~", factor1, "*", factor2)))
    emmean_cld <- cld(emmean, Letters = letters)
  }, error = function(e) {
    stop("Error in calculating estimated marginal means: ", e$message)
  })
  
  # Adjust CLD for plotting
  cld_data <- as.data.frame(emmean_cld)
  cld_data$group <- paste(cld_data[[factor1]], cld_data[[factor2]], sep = "_")
  
  # Interaction Plot with Letters
  plot <- ggplot(data, aes_string(x = factor1, y = numeric_var, fill = factor2)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_text(data = cld_data, aes_string(x = factor1, y = "emmean", label = ".group"),
              position = position_dodge(width = 0.75), vjust = -0.5, size = 5) +
    theme_bw() +
    labs(x = factor1, y = numeric_var, title = "Two-Way ANOVA Results") +
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




# Load the dataset
data("ToothGrowth")

# Inspect the data structure
str(ToothGrowth)

rdata <- ToothGrowth %>% 
  select(supp, dose, len)

# Prepare the dataset for Two-Way ANOVA
# `supp` and `dose` are factors, and `len` is the numeric variable
ToothGrowth$dose <- as.factor(ToothGrowth$dose) # Ensure dose is a factor

# Test the function
result <- TwoWayAnova(rdata)

# Display results
print(result$ANOVA_Summary)
print(result$TukeyHSD)
print(result$Compact_Letters_Display)
print(result$Plot)



TwoWayAnova(data = rdata)


sd <- iris %>% 
  pivot_longer(cols = 1:4) %>% 
  select(2,1,3)


TwoWayAnova(data = sd)

  











