#Load bigcity data
bigcity <- read.csv("C:/repos/stat4064/data/raw/bigcity.csv")

### PART A ###

#Scatter plot of 1920 vs 1930
plot(bigcity$u, bigcity$x, xlab = "1920 population (in 1000s)", ylab = "1930 population (in 1000s)", main = "Population Changes in big US cities")

### PART B ###
## (i) ##

# Set the random seed
set.seed(1234)

# Number of bootstrap samples
B <- 500

# Vector to store the mean values and ratios
m_20_df <- numeric(B)
m_30_df <- numeric(B)
T <- numeric(B)

# Loop over the bootstrap samples
for(k in 1:B) {
  # Generate a bootstrap sample with replacement
  sample_k <- sample(nrow(bigcity), replace = TRUE)
  
  # Calculate the means for 1920 and 1930
  m_20 <- mean(bigcity$u[sample_k])
  m_30 <- mean(bigcity$x[sample_k])
  
  # Score the values
  m_20_df[k] <- m_20
  m_30_df[k] <- m_30
  
  # Calculate the ratio
  T[k] <- m_30 / m_20
}

# Plot m20 and m30 pairs on scatter
plot(m_20_df, m_30_df, xlab = "1920 pairs", ylab = "1930 pairs", main = "Pairs of 1920 vs 1930 Bootstrap Means")

# Summary statistics of the ratios
#summary(T)

## (ii-iv) ##

# Load the ggplot2 package
library(ggplot2)

# # Change numeric vectors to DFs
# m_20_DF <- data.frame(as.numeric(m_20_df))
# m_30_DF <- data.frame(m_30_df)
# T_DF <- data.frame(T)

# create a data frame with your numeric vector
m_20_DF <- data.frame(x = m_20_df)
m_30_DF <- data.frame(x = m_30_df)
T_DF <- data.frame(x = T)

# plot the smoothed histogram
ggplot(m_20_DF, aes(x)) + 
  geom_histogram(aes(y=..density..), binwidth=2, color="black", fill="white") +
  geom_density(alpha=.2, fill="#EE7454") +
  labs(x="Population (in 1000s)", y="Density", title="1920 Bootstrap Means")

# plot the smoothed histogram
ggplot(m_30_DF, aes(x)) + 
  geom_histogram(aes(y=..density..), binwidth=2, color="black", fill="white") +
  geom_density(alpha=.2, fill="#EE7454") +
  labs(x="Population (in 1000s)", y="Density", title="1930 Bootstrap Means")

# plot the smoothed histogram
ggplot(T_DF, aes(x)) + 
  geom_histogram(aes(y=..density..), binwidth=2, color="black", fill="white") +
  geom_density(alpha=.2, fill="#EE7454") +
  labs(x="Population (in 1000s)", y="Density", title="1920/1930 Bootstrap Means Ratios")

# Calculate bootstrap estimate of standard error of T
SE_T <- sd(T)










# Create a histogram with density curve using ggplot2

# ggplot(m_20_DF, aes(x = m_20_DF)) +
#   geom_histogram()+
#   scale_x_discrete()


# # Create ggplot object with m_20_df data
# ggplot(m_20_DF, aes(x = m_20_DF)) +
#   
#   # Add histogram layer with density calculation
#   geom_histogram(aes(y=..density..), binwidth=2, color="black", fill="white") +
#   
#   # Add density layer with fill and alpha settings
#   geom_density(alpha=.2, fill="#FF6666") +
#   
#   # Add x-axis and y-axis labels and plot title
#   labs(x="m_20_df", y="Density", title="Smoothed Histogram")




