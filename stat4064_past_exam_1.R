library(ISLR)

# QUESTION 1
# (a) Check for missing values and remove them if any
Auto <- na.omit(Auto)
dim(Auto)  # Dimension of the data
# The data has 392 observations and 9 variables

# (b) Parallel coordinate plot
parcoord(Auto[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")], main = "Parallel Coordinate Plot")

# (c) Compute correlation matrix
cor_matrix <- cor(Auto[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")])
cor_matrix
# The highest absolute correlation is between displacement and weight (0.9328)
# The lowest absolute correlation is between mpg and acceleration (-0.5061)

# (d) Subset data for year >= 73
Auto73 <- subset(Auto, year >= 73)
nrow(Auto73)  # Number of observations in Auto73

# (e) Fit simple linear regression model
lm_model <- lm(acceleration ~ horsepower, data = Auto73)
summary(lm_model)

# Scatterplot with regression line
plot(Auto73$horsepower, Auto73$acceleration, xlab = "Horsepower", ylab = "Acceleration", main = "Scatterplot with Regression Line")
abline(lm_model, col = "red")

# (f) Residual plot
plot(lm_model, which = 1)

# (g) Predict acceleration for given horsepower values
horsepower_values <- c(93, 175)
predictions <- predict(lm_model, newdata = data.frame(horsepower = horsepower_values), interval = "confidence", level = 0.95)
prediction_intervals <- predict(lm_model, newdata = data.frame(horsepower = horsepower_values), interval = "prediction", level = 0.95)

# Print predicted acceleration and intervals
data.frame(Horsepower = horsepower_values, Predicted_Acceleration = predictions[, "fit"],
           Confidence_Intervals = paste(predictions[, "lwr"], predictions[, "upr"], sep = ", "),
           Prediction_Intervals = paste(prediction_intervals[, "lwr"], prediction_intervals[, "upr"], sep = ", "))


# QUESTION 2
# (a) Create mpgclass variable
Auto$mpgclass <- ifelse(Auto$mpg <= 23, 0, 1)

# (a) Count observations in each class
class_counts <- table(Auto$mpgclass)
class_counts

# (b) Confusion matrix for a 2-class classification problem
actual_class <- Auto$mpgclass
predicted_class <- ifelse(Auto$mpg > 23, 1, 0)
confusion_matrix <- table(actual_class, predicted_class)
confusion_matrix

# (c) Fit logistic regression model and calculate classification error
model <- glm(mpgclass ~ weight + acceleration + horsepower, data = Auto, family = binomial)
predicted_class <- ifelse(predict(model, type = "response") > 0.5, 1, 0)
confusion_matrix <- table(predicted_class, Auto$mpgclass)
classification_error <- 1 - sum(diag(confusion_matrix)) / sum(confusion_matrix)
confusion_matrix
classification_error

# (d) Description of the validation set approach

# (e) Use observations 85:184 as validation set, fit logistic regression using training data, and present confusion matrix
validation_indices <- 85:184
training_indices <- setdiff(1:nrow(Auto), validation_indices)

training_data <- Auto[training_indices, ]
validation_data <- Auto[validation_indices, ]

model <- glm(mpgclass ~ weight + acceleration + horsepower, data = training_data, family = binomial)
training_predictions <- ifelse(predict(model, type = "response") > 0.5, 1, 0)
training_confusion_matrix <- table(training_predictions, training_data$mpgclass)
training_confusion_matrix

# (f) Test the model on the validation set and present the confusion matrix
validation_predictions <- ifelse(predict(model, newdata = validation_data, type = "response") > 0.5, 1, 0)
validation_confusion_matrix <- table(validation_predictions, validation_data$mpgclass)
validation_confusion_matrix

# (g) Explanation of how the choice of validation set can affect test results

# (h) Comparison of the three confusion matrices and interpretation

# QUESTION 3
# (a)
data(Carseats)

Carseats10 <- Carseats[, !(names(Carseats) %in% c("ShelveLoc"))]

# (c)
set.seed(123)  # For reproducibility
Carseats10 <- na.omit(Carseats10)

# Convert factor variables to numeric using one-hot encoding
Carseats10 <- model.matrix(~., data = Carseats10[, -which(sapply(Carseats10, is.factor))])
kmeans_result <- kmeans(Carseats10, centers = 2, nstart = 20)

# Cluster means and sizes
cluster_means <- kmeans_result$centers
cluster_sizes <- table(kmeans_result$cluster)

# Sum of squares
total_ss <- kmeans_result$tot.withinss + kmeans_result$betweenss
between_ss <- kmeans_result$betweenss
within_ss <- kmeans_result$tot.withinss
total_within_ss <- sum(kmeans_result$tot.withinss)

cluster_means
cluster_sizes
total_ss
between_ss
within_ss
total_within_ss

# (f)
# Create empty vectors to store the results
tot_within_ss <- vector("numeric", length = 5)
between_ss_ratio <- vector("numeric", length = 5)
between_within_ss_ratio <- vector("numeric", length = 5)

# Perform cluster analysis for each value of K
for (k in 3:7) {
  kmeans_result <- kmeans(Carseats10, centers = k, nstart = 20)
  
  # Calculate and store the result quantities
  tot_within_ss[k-2] <- kmeans_result$tot.withinss
  between_ss_ratio[k-2] <- kmeans_result$betweenss / (kmeans_result$betweenss + kmeans_result$tot.withinss)
  between_within_ss_ratio[k-2] <- kmeans_result$betweenss / kmeans_result$tot.withinss
}

# Plot the results
par(mfrow = c(1, 3))

# (i) Total within-cluster sum of squares versus K
plot(3:7, tot_within_ss, type = "b", xlab = "K", ylab = "Total Within-Cluster SS",
     main = "Total Within-Cluster SS vs. K")

# (ii) Ratio of between-cluster sum of squares and total sum of squares versus K
plot(3:7, between_ss_ratio, type = "b", xlab = "K", ylab = "Between-Cluster SS / Total SS",
     main = "Between-Cluster SS / Total SS vs. K")

# (iii) Ratio of between-cluster sum of squares and total within-cluster sum of squares versus K
plot(3:7, between_within_ss_ratio, type = "b", xlab = "K",
     ylab = "Between-Cluster SS / Total Within-Cluster SS",
     main = "Between-Cluster SS / Total Within-Cluster SS vs. K")

par(mfrow = c(1, 1))




