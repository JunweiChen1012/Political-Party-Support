# Load necessary libraries
library(MASS)

# Set seed for reproducibility
set.seed(123)

# Simulate data
n <- 1000  # Number of observations
age <- sample(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), n, replace = TRUE)
gender <- sample(c("Male", "Female", "Other"), n, replace = TRUE)
income <- sample(c("Low", "Medium", "High"), n, replace = TRUE)
education <- sample(c("High School", "Bachelor's Degree", "Master's Degree", "PhD"), n, replace = TRUE)


# Generate simulated binary response variable based on predictors
# Let's assume the support for Party X is influenced by age and education
support <- rep(NA, n)
for (i in 1:n) {
  if (age[i] %in% c("18-24", "25-34", "35-44") & education[i] %in% c("High School", "Bachelor's Degree")) {
    support[i] <- sample(c("Yes", "No"), 1, prob = c(0.7, 0.3))
  } else if (age[i] %in% c("45-54", "55-64", "65+") & education[i] %in% c("Master's Degree", "PhD")) {
    support[i] <- sample(c("Yes", "No"), 1, prob = c(0.6, 0.4))
  } else {
    support[i] <- sample(c("Yes", "No"), 1, prob = c(0.5, 0.5))
  }
}

# Convert data to data frame
data <- data.frame(Age = age, Gender = gender, Income = income, Education = education, Support = support)


# Conduct hypothesis tests
# Let's conduct chi-square tests of independence for each pair of categorical variables
tests <- list()
tests$count <- 0
for (i in 1:3) {
  for (j in (i+1):4) {
    tests$count <- tests$count + 1
    cat1 <- names(data)[i]
    cat2 <- names(data)[j]
    contingency_table <- table(data[[cat1]], data[[cat2]])
    chi_sq_test <- chisq.test(contingency_table)
    tests[[paste(cat1, cat2, sep = " vs ")]] <- chi_sq_test
  }
}


# Print results of hypothesis tests
for (i in 1:tests$count) {
  test_name <- names(tests)[i]
  cat("Hypothesis Test:", test_name, "\n")
  print(tests[[test_name]])
  cat("\n")
}
