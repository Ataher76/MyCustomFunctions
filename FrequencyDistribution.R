# Load required package
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

# Function to create frequency distribution
FrequencyTable <- function(data, bin_width = 3) {
  # Create bins based on minimum and maximum lengths
  min_length <- min(data, na.rm = TRUE)
  max_length <- max(data, na.rm = TRUE)
  
  # Generate bin edges
  breaks <- seq(floor(min_length), ceiling(max_length) + bin_width, by = bin_width)
  
  # Create frequency table
  freq_table <- data.frame(
    Length_Range = cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE),
    Frequency = 1
  ) %>%
    group_by(Length_Range) %>%
    summarise(Frequency = sum(Frequency)) %>%
    ungroup()
  
  return(freq_table)
}

# Example usage
length_data <- c(12, 15, 22, 27, 29, 30, 32, 35, 40, 43, 45)  # Replace with your data
FrequencyTable(length_data, bin_width = 5)
