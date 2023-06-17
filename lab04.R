library(ISLR2) ## For some datasets
library(MASS) ## For lda()
library(class) ## For knn()
library(nnet) ## for multnom()
library(matrixStats) ## for tapply()
library(graphics)
library(ggplot2)
library(e1071) ## for naive Bayes (added 8April 2022)

data("Smarket")

# PART 1A
# Fit an LDA model to all data
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket)

# Predict class labels for all data
lda.predict <- predict(lda.fit)
class_labels <- lda.predict$class

# Create a confusion table
confusion_table <- table(class_labels, Smarket$Direction)

# Display the confusion table
confusion_table

# Calculate classification error
classification_error <- sum(class_labels != Smarket$Direction) / nrow(Smarket)
classification_error

# PART 1B
# Fit an LDA model to all data
qda.fit <- qda(Direction ~ Lag1 + Lag2 + I(Lag1^2) + I(Lag2^2) + Lag1:Lag2, data = Smarket)

# Predict class labels for all data
qda.predict <- predict(qda.fit)
class_labels <- qda.predict$class

# Create a confusion table
confusion_table <- table(class_labels, Smarket$Direction)

# Display the confusion table
confusion_table

# Calculate classification error
classification_error <- sum(class_labels != Smarket$Direction) / nrow(Smarket)
classification_error

# PART 2A
# Split the data into training and test subsets
train_data <- subset(Smarket, Year < 2004)
test_data <- subset(Smarket, Year >= 2004)

# Fit linear discriminant model
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = train_data)
lda.predict <- predict(lda.fit, newdata = test_data)$class

# Calculate misclassification rate for linear discriminant model
lda_misclassification_rate <- sum(lda.predict != test_data$Direction) / nrow(test_data)
lda_confusion_table <- table(lda.predict, test_data$Direction)

# Fit quadratic discriminant model
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = train_data)
qda.predict <- predict(qda.fit, newdata = test_data)$class

# Calculate misclassification rate for quadratic discriminant model
qda_misclassification_rate <- sum(qda.predict != test_data$Direction) / nrow(test_data)
qda_confusion_table <- table(qda.predict, test_data$Direction)

# Display misclassification rates and confusion tables
cat("Misclassification rate for Linear Discriminant Model:", lda_misclassification_rate, "\n")
print(lda_confusion_table)
cat("\nMisclassification rate for Quadratic Discriminant Model:", qda_misclassification_rate, "\n")
print(qda_confusion_table)

# PART 2B
# Split the data into training and test subsets
train_data <- subset(Smarket, Year < 2004)
test_data <- subset(Smarket, Year >= 2004)

# Fit linear discriminant model
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = train_data)
lda.predict <- predict(lda.fit, newdata = train_data)$class

# Calculate misclassification rate for linear discriminant model
lda_misclassification_rate <- sum(lda.predict != train_data$Direction) / nrow(train_data)
lda_confusion_table <- table(lda.predict, train_data$Direction)

# Fit quadratic discriminant model
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = train_data)
qda.predict <- predict(qda.fit, newdata = train_data)$class

# Calculate misclassification rate for quadratic discriminant model
qda_misclassification_rate <- sum(qda.predict != train_data$Direction) / nrow(train_data)
qda_confusion_table <- table(qda.predict, train_data$Direction)

# Display misclassification rates and confusion tables
cat("Misclassification rate for Linear Discriminant Model:", lda_misclassification_rate, "\n")
print(lda_confusion_table)
cat("\nMisclassification rate for Quadratic Discriminant Model:", qda_misclassification_rate, "\n")
print(qda_confusion_table)

# PART 3A
olive <- read.table("olive.dat", header=TRUE)
nrows <- nrow(olive)
cat("Number of rows:",nrows,"\n")
n_classes <- length(unique(olive$group.id))
cat("Unique classes:",n_classes,"\n")

# PART 3B
# Fit the multinomial regression model
multinom_model <- multinom(group.id ~ ., data = olive)

# Predict class labels on the training data
train_predictions <- predict(multinom_model, newdata = olive, type ="class")

# PART 3C
# Create a confusion matrix on the training set
confusion_matrix <- table(train_predictions, olive$group.id)

# Display the confusion matrix
print(confusion_matrix)

# Calculate the misclassification rate on the training data
misclassification_rate <- 1 - sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Misclassification rate on the training data:", misclassification_rate, "\n")

# PART 3D
# Fit the LDA model
lda_model <- lda(group.id ~ ., data = olive)
# Predict class labels on the training data
lda_predictions <- predict(lda_model, newdata = olive)$class
# Create the confusion table
confusion_table <- table(lda_predictions, olive$group.id)
# Calculate the classification error
classification_error <- 1 - sum(diag(confusion_table)) / sum(confusion_table)
# Display the confusion table
print(confusion_table)
# Display the classification error
cat("Classification error using LDA:", classification_error, "\n")

# PART E
# Fit the QDA model
qda_model <- qda(group.id ~ ., data = olive)

# Predict class labels on the training data
qda_predictions <- predict(qda_model, newdata = olive)$class

# Create the confusion table
confusion_table <- table(qda_predictions, olive$group.id)

# Calculate the classification error
classification_error <- 1 - sum(diag(confusion_table)) / sum(confusion_table)

# Display the confusion table
print(confusion_table)

# Display the classification error
cat("Classification error using QDA:", classification_error, "\n")

# PART 4
# Fit the Naive Bayes classifier
nb_model <- naiveBayes(group.id ~ ., data = olive)

# Predict class labels on the training data
nb_predictions <- predict(nb_model, newdata = olive)

# Create the confusion table
confusion_table <- table(nb_predictions, olive$group.id)

# Calculate the classification error
classification_error <- 1 - sum(diag(confusion_table)) / sum(confusion_table)

# Display the confusion table
print(confusion_table)

# Display the classification error
cat("Classification error using Naive Bayes:", classification_error, "\n")
