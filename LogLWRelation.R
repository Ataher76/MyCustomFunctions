# Load required libraries
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(ggplot2)
library(readxl)

# Universal Length-Weight Regression Function
lw_reg <- function(data, length_col = "Length", weight_col = "Weight", 
                   xlab = "log Length (cm)", ylab = "log Weight (g)", 
                   main = "Log-Log Length-Weight Plot") {
  
  # Check if data has the specified length and weight columns
  if (!all(c(length_col, weight_col) %in% colnames(data))) {
    stop("The data must have the specified 'Length' and 'Weight' columns.")
  }
  
  # Rename the specified columns to standard names for easier use in the function
  lwdata <- data[, c(length_col, weight_col)]
  colnames(lwdata) <- c("Length", "Weight")
  
  # Check for any zero or negative values in Length or Weight
  if (any(lwdata$Length <= 0) | any(lwdata$Weight <= 0)) {
    stop("Length and Weight must be positive values for log transformation.")
  }
  
  # Fit the linear model (log-log transformation)
  mod <- lm(log(Weight) ~ log(Length), data = lwdata)
  
  # Print the summary of the model
  print(summary(mod))
  
  # Extract the coefficients (Intercept and Slope)
  intercept <- coef(mod)["(Intercept)"]
  slope <- coef(mod)["log(Length)"]
  
  # Calculate the actual values of a and b
  b <- slope  # b is the slope in the log-log model
  a <- exp(intercept)  # a is exp(intercept) because log(a) = intercept
  
  # Print the coefficients a and b
  cat("Estimated a (Intercept):", a, "\n")
  cat("Estimated b (Slope):", b, "\n")
  
  # Plot the original data in log-log scale
  plot(log(lwdata$Length), log(lwdata$Weight), pch=15, col="black", 
       xlab=xlab, ylab=ylab, main=main)
  
  # Generate a sequence of log(Length) values for plotting the fitted line
  length_range_log <- seq(min(log(lwdata$Length)), max(log(lwdata$Length)), length.out = 100)
  
  # Calculate the corresponding log(Weight) values for the fitted line
  fitted_log_weight <- intercept + slope * length_range_log
  
  # Plot the fitted line
  lines(length_range_log, fitted_log_weight, col="red", lwd=2)
  
  # Add legend
  legend("topleft", legend=c("Observed Data", "Fitted Line"), 
         col=c("black", "red"), pch=c(15, NA), lty=c(NA, 1))
  
  # Return the model object and coefficients
  return(list(model = mod, a = a, b = b))
}
