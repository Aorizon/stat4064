library(ISLR2)
library(leaps)

# Load data and remove observations with missing Salary values
data("Hitters")
Hitters <- na.omit(Hitters)

# Set up the predictor and response variables
y <- Hitters$Salary
X <- model.matrix(Salary ~ ., data = Hitters)[,-1]

# Perform best subset selection
best.subset <- regsubsets(X, y, nvmax = ncol(X), method = "exhaustive")

# Identify the optimal number of predictors
summary(best.subset)$which

which(summary(best.subset)$which[5,-1])

