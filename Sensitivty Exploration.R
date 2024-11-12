# Sensitivity exploration

## Morris Method
# Load necessary libraries
library(sensitivity)

# Assuming df is your real data
df <- data.frame(
  x1 = runif(100, 1, 10),  # This is your actual data
  x2 = runif(100, 1, 10),
  x3 = runif(100, 1, 10)
)

# Define the model function
model_function <- function(X) {
  w1 <- 0.4; w2 <- 0.3; w3 <- 0.3
  return(w1 * X[, 1] + w2 * X[, 2] + w3 * X[, 3])
}

# Define the factors (input variables)
factors <- c("x1", "x2", "x3")

# Perform Morris sensitivity analysis
morris_result <- morris(model = model_function, factors = factors, r = 100,
                        design = list(type = "oat", levels = 5, grid.jump = 2))

# Print and plot the results
print(morris_result)
plot(morris_result)

# Getting PCA weights? ####
library(compindPCA)
data("Data_sample")
compind(Data_sample, var_p=c("A", "B", "C", "D", "F", "G", "H", "I", "J"), var_n = c("E"))
