library(ISLR2)
library(ggplot2)

# Load the Auto data
data(Auto)

# Remove observations with missing values
auto_clean <- na.omit(Auto)

# Subset of cars with Year >= 76
Auto76 <- subset(auto_clean, year >= 76)

# Scale the variables in Auto76
scaled_data <- scale(Auto76[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")])

# Convert the scaled data to a data frame
scaled_data <- as.data.frame(scaled_data)

# Rename the columns
colnames(scaled_data) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")


