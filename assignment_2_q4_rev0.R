# Load library and data
library(ISLR2)
data(Hitters)
#PART A# Hitters_clean <- Hitters[!is.na(Hitters$Salary), ]
Hitters_clean <- na.omit(Hitters)

# Convert categorical variable into binary columns
League_OHE <- model.matrix(~ League - 1, data = Hitters_clean)
NewLeague_OHE <- model.matrix(~ NewLeague - 1, data = Hitters_clean)
Division_OHE <- model.matrix(~ Division - 1, data = Hitters_clean)
#Drop original columns
Hitters_clean <- Hitters_clean[, !colnames(Hitters_clean) %in% c("League", "NewLeague", "Division")]

# Bind the binary columns to the original dataset
Hitters_clean <- cbind(Hitters_clean, League_OHE, NewLeague_OHE, Division_OHE)

# Set seed for reproducibility
set.seed(1234)

# Split the data into training and test sets
train <- sample(nrow(Hitters_clean), nrow(Hitters_clean) / 2)
Hitters.train <- Hitters_clean[train, ]
Hitters.test <- Hitters_clean[-train, ]

### PART A ###
# # Fit a linear model using least squares on the training set
# lm.fit <- lm(Salary ~ ., data = Hitters.train)
# 
# # Compute the test error
# test.error <- mean((Hitters.test$Salary - predict(lm.fit, newdata = Hitters.test))^2)
# test.error


### PART B ###
library(glmnet)

# # Standardize the predictors
# x.train <- as.matrix(scale(Hitters.train[, -which(names(Hitters.train) == "Salary")]))
# y.train <- Hitters.train$Salary
# 
# x.test <- as.matrix(scale(Hitters.test[, -which(names(Hitters.test) == "Salary")]))
# y.test <- Hitters.test$Salary

# Remove all NAs
# Hitters.train <- na.omit(Hitters.train)
# Hitters.test <- na.omit(Hitters.test)

# Convert to numeric
Hitters.train <- apply(Hitters.train,2,as.numeric)
Hitters.test <- apply(Hitters.test,2,as.numeric)

# Convert to matrices
Hitters.train <- as.matrix(Hitters.train)
Hitters.test <- as.matrix(Hitters.test)

# Standardize the predictors
x.train <- Hitters.train[, -which(colnames(Hitters.train) == "Salary")]
y.train <- Hitters.train[,"Salary",drop=FALSE]

x.test <- Hitters.test[, -which(colnames(Hitters.test) == "Salary")]
y.test <- Hitters.test[,"Salary",drop=FALSE]

# Fit a ridge regression model for different values of lambda
lambdas <- 10^seq(0, 5, by = 1)

train.errors <- c()
test.errors <- c()
#
for (lambda in lambdas) {
  ridge.fit <- glmnet(x.train, y.train, alpha = 0, lambda = lambda)
  train.pred <- predict(ridge.fit, s = lambda, newx = x.train)
  test.pred <- predict(ridge.fit, s = lambda, newx = x.test)
  
  train.error <- mean((y.train - train.pred)^2)
  test.error <- mean((y.test - test.pred)^2)

  train.errors <- c(train.errors, train.error)
  test.errors <- c(test.errors, test.error)
}

# Report the training and test errors for different values of lambda
results <- data.frame(lambda = lambdas, train_error = train.errors, test_error = test.errors)
print(results)


# # Fit a lasso regression model for different values of lambda
# lambdas <- 10^seq(0, 5, by = 1)
# 
# train.errors <- c()
# test.errors <- c()
# num.nonzero.coef <- c()
# 
# for (lambda in lambdas) {
#   lasso.fit <- glmnet(x.train, y.train, alpha = 1, lambda = lambda)
#   train.pred <- predict(lasso.fit, s = lambda, newx = x.train)
#   test.pred <- predict(lasso.fit, s = lambda, newx = x.test)
#   
#   train.error <- mean((y.train - train.pred)^2)
#   test.error <- mean((y.test - test.pred)^2)
#   
#   train.errors <- c(train.errors, train.error)
#   test.errors <- c(test.errors, test.error)
#   
#   # Count no. non-zero coefficients
#   num.nonzero.coef <- c(num.nonzero.coef, sum(coef(lasso.fit, s = lambda) != 0))
# }
# 
# # Report the training and test errors for different values of lambda
# results <- data.frame(lambda = lambdas, train_error = train.errors, test_error = test.errors, num_nonzero_coef = num.nonzero.coef)
# print(results)
