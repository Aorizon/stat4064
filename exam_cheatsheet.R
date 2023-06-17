# MODEL TEMPLATES
# Load necessary libraries
library(MASS)
library(caret)
library(splines)
library(mgcv)

# Importing data
df = read.table('irisdata.csv', header = TRUE, sep = ',') # specify delimiter
df = read.csv('irisdata.csv') # .csv

# Data frame navigation
df[3] # All rows, specified column
df['Sepal.Length'] # All rows, specified column
df$Sepal.Length
df$Sepal.Length[1:20] # Specified column $<INSERT> and rows [<INSERT>]
df[['Sepal.Length']][2:4]
df[3, 2] # [row,column]
df[3,] # LEave blank for all
df[2:4, 3]
df[3, 2:4]
df[3, 'Sepal.Width']
df[c(2:4, 90:92), c('Sepal.Length', 'Species')]

# Variables
# Qualitative variables


# Training and validation/test dataset
set.seed(4064) # Very important for reproducibility
library(ISLR) #library(ISLR2)
idx =sample(NROW(Auto), NROW(Auto)/2) # Can use any %, typically 80/20
Auto.training = Auto[idx, c("mpg", "horsepower")]
Auto.validation= Auto[-idx, c("mpg", "horsepower")]
# Hyperparameter tuning iteration
res =rep(NA, 10) # res = results
for (degree in 1:10 ){
  fm = lm(mpg ~ poly(horsepower, degree), Auto.training)
  res[degree] = mean((Auto.validation$mpg - predict(fm, newdata = Auto.validation))^2)
}
plot(res, xlab = "Degree of Polynomial", ylab = "Mean Squared Error", type = "b")

# Basic R
# For loop
for (i in 1:5) {
  print(i)
}

# Linear Regression
model_lr <- lm(y ~ x, data = df)
# model_lr <- lm(y ~ x + z + a, data = df) #Multiple LR
# model_lr <- lm(y ~ ., data = df) #Multiple LR with all vars
summary(model_lr)

# Non-linear Regression
model_nlr <- lm(y ~ x + I(x^2), data = df)
predict(model_nlr, newdata = data.frame(balance = c(1000, 2000)), type = "response") #where 1000, 2000 are inputs we are trying to predict with
summary(model_nlr)

# Logistic Regression
model_logr <- glm(y ~ x, data = df, family = "binomial") #could be binomial without ""
summary(model_logr)

# Multiclass Logistical Regression
model_mlogr <- multinom(y ~ x, data = df)

# Discriminant Analysis
model_da <- lda(y ~ x, data = df)
print(model_da)

# Metrics (model quality)
predicted <- predict(model_lr, newdata = df) 
actual <- df$y
confusionMatrix(data = predicted, reference = actual)
postResample(pred = predicted, obs = actual)

# Resample and Cross-validation
fitControl <- trainControl(method = "cv", number = 10)
model_cv <- train(y ~ x, data = df, trControl = fitControl)

# LOOCV 
logit_model <- glm(default ~ income + balance + student, data = Default, family = binomial)
# Compute the LOOCV test error estimate
cv_error <- cv.glm(Default, logit_model)

# Model (variable) selection
results <- rfe(df[, -ncol(df)], df[, ncol(df)], sizes = c(1:5))
predictors(results)

# Splines
model_spline <- lm(y ~ bs(x, df = 4), data = df) #ns() not bs() for Natural Splines
model_smooth_spline <- smooth.spline(x,y)
#model_smooth_spline <- smooth.spline(x,y,df=10)
summary(model_spline)

# Local regression
fit = loess(mcycleaccel mcycletimes, span = .2)
prefit = predict(fit, data.frame(times = mcycle$times))
lines(mcycle$times, prefit, col = "red", lwd = 2)

# Kernel function
kernel function ksmooth
lines(ksmooth(mcycle$times, mcycle$accel, "normal",
              bandwidth = 2), col = 2, lwd = 2)

# Generalized Additive Model (GAM)
model_gam <- gam(y ~ s(x), data = df)
gam(wage∼s(year,df=5) + lo(age,span=.5) + education) #Use smoothing splines or local regression
#ns(age,df=5):ns(year,df=5) # for interaction between predictors
summary(model_gam)
# Perform ANOVA to compare models
anova_results <- anova(model1, model2)

# Clustering
result_kmeans <- kmeans(df, centers = 3)
result_hclust <- hclust(dist(df))
clusters <- cutree(result_hclust, k = 3)


# Bootstrap
# Load the 'boot' package
library(boot)
# Set the number of bootstrap samples
num_samples <- 1000
# Create a function to compute the statistic of interest
stat_func <- function(data, index) {
  # Extract the bootstrap sample using the index
  bootstrap_sample <- data[index]
  # Compute and return the statistic
  mean(bootstrap_sample)
}
# Perform the bootstrap resampling
bootstrap_results <- boot(data, statistic = stat_func, R = num_samples)
# Access the bootstrap sample statistics
bootstrap_statistics <- bootstrap_results$t


# PLOTTING
library(GGally)
library(ggplot2)

# Variable pairs
ggpairs(df)
plot(df)
#Multi relational i.e. more than pairs 
MASS::parcoord(df[, 1:4], 
               col = c('green', 'blue', 'black')[as.numeric(factor(df$Species))])

# Bar Plot
# ggplot2
ggplot(data, aes(x = x_variable, y = y_variable)) +
  geom_bar(stat = "identity")
# Standard Plot
barplot(y_variable, names.arg = x_variable)

# Histogram
# ggplot2
ggplot(data, aes(x = x_variable)) +
  geom_histogram()
# Standard Plot
hist(x_variable)

# Boxplot
# ggplot2
ggplot(data, aes(x = x_variable, y = y_variable)) +
  geom_boxplot()
# Standard Plot
boxplot(x_variable, y_variable)

# Scatter Plot
# ggplot2
ggplot(data, aes(x = x_variable, y = y_variable)) +
  geom_point()
# Standard Plot
plot(x_variable, y_variable)

# Line Plot
# ggplot2
ggplot(data, aes(x = x_variable, y = y_variable)) +
  geom_line()
# Standard Plot
plot(x_variable, y_variable, type = "l")

# ggplot template
ggplot(data, aes(x = x_variable, y = y_variable, color = group_variable, size = value_variable)) +
  geom_point(shape = 16) +
  labs(title = "Scatter Plot", x = "X-Axis", y = "Y-Axis") +
  theme(plot.title = element_text(color = "blue", size = 16),
        axis.text = element_text(size = 12)) +
  xlim(0, 10) +
  ylim(0, 20)

