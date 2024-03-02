# Load necessary libraries
library(ggplot2)
library(rstanarm)


# Create a simulated dataset
set.seed(123) # Set seed for reproducibility

# Define demographic variables
age <- sample(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"), 100, replace = TRUE)
gender <- sample(c("Male", "Female", "Other"), 100, replace = TRUE)
income <- sample(c("Low", "Medium", "High"), 100, replace = TRUE)
education <- sample(c("High School", "Bachelor's Degree", "Master's Degree", "PhD"), 100, replace = TRUE)

# Generate simulated binary response variable based on predictors
support <- rep(NA, 100)
for (i in 1:100) {
  if (age[i] %in% c("18-24", "25-34", "35-44") & education[i] %in% c("High School", "Bachelor's Degree")) {
    support[i] <- sample(c("Yes", "No"), 1, prob = c(0.7, 0.3))
  } else if (age[i] %in% c("45-54", "55-64", "65+") & education[i] %in% c("Master's Degree", "PhD")) {
    support[i] <- sample(c("Yes", "No"), 1, prob = c(0.6, 0.4))
  } else {
    support[i] <- sample(c("Yes", "No"), 1, prob = c(0.5, 0.5))
  }
}

# Create a data frame
data <- data.frame(Age = age, Gender = gender, Income = income, Education = education, Support = support)

# Display the first few rows of the dataset
head(data)


# Build the graph using ggplot2
graph <- ggplot(data, aes(x = Age, fill = Support)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Gender) +
  labs(title = "Support for Party X by Age Group and Gender",
       x = "Age Group",
       y = "Proportion") +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red")) +
  theme_minimal()


# Display the graph
print(graph)

# Build the model using rstanarm
model <- stan_glm(Support ~ Age + Gender + Income + Education,
                  data = data,
                  family = binomial(link = "logit"))

# Print model summary
print(summary(model))
