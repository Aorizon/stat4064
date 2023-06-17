set.seed(42)  # for reproducibility

# Step 1: Simulating the Data
X <- matrix(rnorm(50*5000), nrow=50, ncol=5000)  # Simulated predictors
y <- sample(0:1, 50, replace=TRUE)  # Simulated class labels

# Step 2: Finding Predictors with Largest Correlation
correlations <- abs(cor(X, y))  # Compute correlations
top_indices <- tail(order(correlations), 100)  # Select top 100 predictors

# Step 3: Modeling with Logistic Regression
X_selected <- X[, top_indices]  # Extract selected predictors
set.seed(42)  # for reproducibility
train_indices <- sample(1:nrow(X_selected), nrow(X_selected)*0.8)  # Train-test split indices
X_train <- X_selected[train_indices, ]  # Training set
y_train <- y[train_indices]  # Training labels
X_test <- X_selected[-train_indices, ]  # Test set
y_test <- y[-train_indices]  # Test labels

model <- glm(y_train ~ ., data=data.frame(y_train, X_train), family=binomial)  # Logistic regression model
predictions <- predict(model, newdata=data.frame(X_test), type="response")  # Predict class probabilities

# Evaluation
threshold <- 0.5  # Threshold for class prediction
predicted_labels <- ifelse(predictions > threshold, 1, 0)  # Convert probabilities to class labels
accuracy <- sum(predicted_labels == y_test) / length(y_test)  # Compute accuracy

