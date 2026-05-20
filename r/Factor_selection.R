# Load necessary libraries
library(readxl)

# Read data from Excel file
data <- read_excel("C:/Users/snega/OneDrive/Desktop/PROJECT/Datatsets/FDI processed data.xlsx")

# Normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_norm <- as.data.frame(lapply(data[, -1], normalize))
data_norm$year <- data$year  # Keeping the 'year' column as is

# Fit linear regression models and assess R-squared values
results <- data.frame(variable = names(data_norm)[-1], R_squared = numeric(length(names(data_norm)[-1])))
for (i in 1:(ncol(data_norm) - 1)) {
  model <- lm(dependent_variable ~ ., data = data.frame(dependent_variable = data_norm$FDI_Total, data_norm[, i]))
  results[i, "R_squared"] <- summary(model)$r.squared
}

# Sort the results dataframe by R-squared values
results <- results[order(-results$R_squared), ]

# Print the sorted list of variables
cat("Variables in order of their contribution to the dependent variable:\n")
for (i in 1:nrow(results)) {
  cat(i, ": ", results[i, "variable"], " (R-squared = ", results[i, "R_squared"], ")\n", sep = "")
}