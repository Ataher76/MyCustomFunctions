
# Define the custom function using the first and second columns
LWRelation <- function(data) {
  
  # Check if the data has at least two columns
  if (ncol(data) < 2) {
    stop("The data must have at least two columns: one for length and one for weight.")
  }
  
  # Fit the linear model (log-log transformation)
  mod <- lm(log(data[[2]]) ~ log(data[[1]]))
  
  # Print the summary of the model
  print(summary(mod))
  
  # Extract the coefficients (Intercept and Slope)
  intercept <- coef(mod)["(Intercept)"]
  slope <- coef(mod)["log(data[[1]])"]
  
  # Calculate the actual values of a and b
  b <- slope  # b is the slope in the log-log model
  a <- exp(intercept)  # a is exp(intercept) because log(a) = intercept
  
  # Print the coefficients a and b
  cat("Estimated a (Intercept):", a, "\n")
  cat("Estimated b (Slope):", b, "\n")
  
  # Plot the original data in log-log scale
  plot(log(data[[1]]), log(data[[2]]), pch=15, col="black", 
       xlab="log Length (cm)", ylab="log Weight (g)", main="Log-Log Length-Weight Plot")
  
  # Generate a sequence of log(Length) values for plotting the fitted line
  length_range_log <- seq(min(log(data[[1]])), max(log(data[[1]])), length.out = 100)
  
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

