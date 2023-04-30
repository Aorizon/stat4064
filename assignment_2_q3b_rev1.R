library(leaps)
library(ISLR2)

# Load and preprocess data
data(Hitters)
Hitters <- na.omit(Hitters)
y <- Hitters$Salary
X <- model.matrix(Salary ~ ., data = Hitters)[,-1]

# Create empty vectors to store results
num.vars <- 1:(ncol(X)-1)
RSS <- rep(NA, length(num.vars))
BIC <- rep(NA, length(num.vars))

# Perform best subset selection for each model size
for (i in num.vars) {
  regfit.full <- regsubsets(X, y, nvmax = i, method = "exhaustive")
  RSS[i] <- sum(regfit.full$rss)
  BIC[i] <- log(nrow(X))*i + log(RSS[i]/nrow(X))
}

# Find the best model based on RSS and BIC
best.RSS <- which.min(RSS)
best.BIC <- which.min(BIC)

# Print results
cat("Best subset selection based on RSS:\n")
summary(regsubsets(X, y, nvmax = best.RSS))

cat("\nBest subset selection based on BIC:\n")
summary(regsubsets(X, y, nvmax = best.BIC))
