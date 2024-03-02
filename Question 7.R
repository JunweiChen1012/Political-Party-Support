# Set seed for reproducibility
set.seed(123)

# Number of observations
n <- 1000

# Generate independent variables: earth, fire, wind, water
earth <- rnorm(n, mean = 50, sd = 10)
fire <- rnorm(n, mean = 60, sd = 15)
wind <- rnorm(n, mean = 70, sd = 12)
water <- rnorm(n, mean = 55, sd = 8)

# Simulate "heart" variable depending on the other four variables
# Assuming heart = 0.5*earth + 0.3*fire + 0.2*wind + 0.1*water + epsilon
heart <- 0.5*earth + 0.3*fire + 0.2*wind + 0.1*water + rnorm(n, mean = 0, sd = 5)

# Create a data frame
data <- data.frame(earth, fire, wind, water, heart)

# Fit linear regression model
model <- lm(heart ~ earth + fire + wind + water, data = data)

# Print summary of the model
summary(model)