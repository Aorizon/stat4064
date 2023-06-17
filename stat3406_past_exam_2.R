# Import data for Question 1
data.q1 <- read.csv("data.q1.csv")

# Import data for Question 2 - Training Set
data.q2.train <- read.csv("data.q2.train.csv")

# Import data for Question 2 - Test Set
data.q2.test <- read.csv("data.q2.test.csv")

# Import data for Question 3
data.q3 <- read.csv("data.q3.csv")

# Simulating data for data.q1 (Billionaires)
set.seed(123)  # For reproducibility

n_billionaires <- 1000  # Number of billionaires to simulate

fortune <- rnorm(n_billionaires, mean = 5, sd = 2)
age <- sample(c(30:80), size = n_billionaires, replace = TRUE)
region <- sample(c("A", "E", "ME", "US", "O"), size = n_billionaires, replace = TRUE)

data.q1 <- data.frame(fortune, age, region)

# Simulating data for data.q2.test and data.q2.train (Wine Quality)
set.seed(123)  # For reproducibility

n_train <- 1000  # Number of training samples
n_test <- 500   # Number of test samples

# Simulating data for the training set
fixed_acidity_train <- rnorm(n_train, mean = 7, sd = 1)
volatile_acidity_train <- runif(n_train, min = 0.1, max = 1)
citric_acid_train <- runif(n_train, min = 0, max = 0.5)
residual_sugar_train <- rnorm(n_train, mean = 5, sd = 2)
chlorides_train <- runif(n_train, min = 0, max = 0.2)
free_sulfur_dioxide_train <- rnorm(n_train, mean = 30, sd = 10)
total_sulfur_dioxide_train <- rnorm(n_train, mean = 100, sd = 30)
density_train <- rnorm(n_train, mean = 0.99, sd = 0.001)
pH_train <- runif(n_train, min = 3, max = 4)
sulfates_train <- runif(n_train, min = 0.3, max = 1)
alcohol_train <- rnorm(n_train, mean = 12, sd = 1)
quality_train <- sample(c(3:8), size = n_train, replace = TRUE)

data.q2.train <- data.frame(
  fixed_acidity = fixed_acidity_train,
  volatile_acidity = volatile_acidity_train,
  citric_acid = citric_acid_train,
  residual_sugar = residual_sugar_train,
  chlorides = chlorides_train,
  free_sulfur_dioxide = free_sulfur_dioxide_train,
  total_sulfur_dioxide = total_sulfur_dioxide_train,
  density = density_train,
  pH = pH_train,
  sulfates = sulfates_train,
  alcohol = alcohol_train,
  quality = quality_train
)

# Simulating data for the test set
fixed_acidity_test <- rnorm(n_test, mean = 7, sd = 1)
volatile_acidity_test <- runif(n_test, min = 0.1, max = 1)
citric_acid_test <- runif(n_test, min = 0, max = 0.5)
residual_sugar_test <- rnorm(n_test, mean = 5, sd = 2)
chlorides_test <- runif(n_test, min = 0, max = 0.2)
free_sulfur_dioxide_test <- rnorm(n_test, mean = 30, sd = 10)
total_sulfur_dioxide_test <- rnorm(n_test, mean = 100, sd = 30)
density_test <- rnorm(n_test, mean = 0.99, sd = 0.001)
pH_test <- runif(n_test, min = 3, max = 4)
sulfates_test <- runif(n_test, min = 0.3, max = 1)
alcohol_test <- rnorm(n_test, mean = 12, sd = 1)
quality_test <- sample(c(3:8), size = n_train, replace = TRUE)

data.q2.test <- data.frame(
  fixed_acidity = fixed_acidity_test,
  volatile_acidity = volatile_acidity_test,
  citric_acid = citric_acid_test,
  residual_sugar = residual_sugar_test,
  chlorides = chlorides_test,
  free_sulfur_dioxide = free_sulfur_dioxide_test,
  total_sulfur_dioxide = total_sulfur_dioxide_test,
  density = density_test,
  pH = pH_test,
  sulfates = sulfates_test,
  alcohol = alcohol_test,
  quality = quality_test
)

# Simulating data for data.q3 (Contraceptive)
set.seed(123)  # For reproducibility

n_groups <- 5  # Number of age groups

age_groups <- paste0("Age Group ", 1:n_groups)
education <- sample(c("low", "high"), size = n_groups, replace = TRUE)
wantsMore <- sample(c("Yes", "No"), size = n_groups, replace = TRUE)
notUsing <- sample(100:500, size = n_groups, replace = TRUE)
using <- sample(500:1000, size = n_groups, replace = TRUE)

data.q3 <- data.frame(age = age_groups, education, wantsMore, notUsing, using)


# QUESTION 1
# Create age categories based on the intervals
data.q1$age_categories <- cut(data.q1$age, breaks = c(0, 20, 40, 60, 80, 100, 120), labels = c("[0, 20)", "[20, 40)", "[40, 60)", "[60, 80)", "[80, 100)", "[100, 120)"), right = FALSE)

library(ggplot2)
library(dplyr)

# Create age categories based on the intervals
data.q1 <- data.q1 %>%
  mutate(age_categories = cut(age, breaks = c(0, 20, 40, 60, 80, 100, 120), labels = c("[0, 20)", "[20, 40)", "[40, 60)", "[60, 80)", "[80, 100)", "[100, 120)"), right = FALSE))

# Number of billionaires heatmap
billionaires_heatmap <- data.q1 %>%
  group_by(region, age_categories) %>%
  summarise(number_of_billionaires = n()) %>%
  ungroup()

ggplot(billionaires_heatmap, aes(x = region, y = age_categories, fill = number_of_billionaires)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Number of Billionaires") +
  labs(x = "Region", y = "Age Categories") +
  ggtitle("Number of Billionaires by Region and Age Categories") +
  theme_minimal()

# Mean fortune heatmap
mean_fortune_heatmap <- data.q1 %>%
  group_by(region, age_categories) %>%
  summarise(mean_fortune = mean(fortune, na.rm = TRUE)) %>%
  ungroup()

ggplot(mean_fortune_heatmap, aes(x = region, y = age_categories, fill = mean_fortune)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Mean Fortune") +
  labs(x = "Region", y = "Age Categories") +
  ggtitle("Mean Fortune by Region and Age Categories") +
  theme_minimal()

# QUESTION 2
library(caret)

# Create an empty vector to store selected variables
selected_vars <- c()

# Function to train and evaluate models using 5-fold cross-validation
train_and_evaluate <- function(variables) {
  formula <- as.formula(paste("quality ~", paste(variables, collapse = "+")))
  model <- train(formula, data = data.q2.train, method = "lm", trControl = trainControl(method = "cv", number = 5))
  predictions <- predict(model, newdata = data.q2.train)
  mse <- mean((data.q2.train$quality - predictions)^2)
  return(mse)
}

# Forward selection
while (length(selected_vars) < ncol(data.q2.train) - 1) {
  available_vars <- setdiff(names(data.q2.train)[-ncol(data.q2.train)], selected_vars)
  results <- sapply(available_vars, function(variable) train_and_evaluate(c(selected_vars, variable)))
  best_variable <- available_vars[which.min(results)]
  selected_vars <- c(selected_vars, best_variable)
}

# Train the final model using the selected variables (if any)
if (length(selected_vars) > 0) {
  final_formula <- as.formula(paste("quality ~", paste(selected_vars, collapse = "+")))
  final_model <- lm(final_formula, data = data.q2.train)
  
  # Make predictions on the test dataset (data.q2.test)
  predictions <- predict(final_model, newdata = data.q2.test)
  
  # Calculate the Mean Squared Error (MSE) on the test dataset
  mse <- mean((data.q2.test$quality - predictions)^2)
  cat("MSE:", mse)
} else {
  cat("No variables selected.")
}

# QUESTION 3
# Load the required library
library(dplyr)

# Transform the response variable
data.q3$response <- data.q3$notUsing / (data.q3$notUsing + data.q3$using)

# Create separate variables for each age group
data.q3_encoded <- data.q3 %>%
  mutate(age_group1 = ifelse(age == "Age Group 1", 1, 0),
         age_group2 = ifelse(age == "Age Group 2", 1, 0),
         age_group3 = ifelse(age == "Age Group 3", 1, 0),
         age_group4 = ifelse(age == "Age Group 4", 1, 0),
         age_group5 = ifelse(age == "Age Group 5", 1, 0),
         educationHigh = ifelse(education == "high", 1, 0),
         wantsMoreYes = ifelse(wantsMore == "Yes", 1, 0)) %>%
  select(age_group1, age_group2, age_group3, age_group4, age_group5, educationHigh, wantsMoreYes, response)

# Fit the logistic regression model
model <- glm(data = data.q3_encoded, formula = response ~ ., family = binomial)

# Print the model summary
summary(model)





# # Train a linear regression model
# model <- lm(quality ~ ., data = data.q2.train)
# 
# # Print the model summary
# summary(model)
# 
# # High corr variables
# data.q2.train_subset <- data.q2.train[, c("sulfates", "density","quality")]
# 
# # Create the pairwise plot
# pairs(data.q2.train_subset)
# 
# # Train a linear regression model
# model2 <- lm(quality ~ ., data = data.q2.train_subset)
# 
# # Print the model summary
# summary(model2)





