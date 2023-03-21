library(MASS)
library(ISLR)
library(ISLR2)
library(graphics)
library(ggplot2)
library(FNN)
library(DAAG)

#Load Carseats dataset
data(Carseats)
str(Carseats)

#Remove unwanted columns
Carseats <- Carseats[, -c(which(names(Carseats) %in% c('Advertising','Education','Population')))]

# #Checking table class types
# Carseats_interim <- Carseats
# for (a in 1:length(Carseats)) { 
#   if (class(Carseats[[a]]) == "factor") {
#     #Encoding
#     for (b in 1:nrow(Carseats)) {
#       Carseats[b,a] <- if(Carseats[[a]][b] == "Yes"){
#         1
#       } else if (Carseats[[a]][b] == "No") {
#         0
#     }
#   }
#   }
# }


#Multiple regression
model <- lm(Carseats$Sales ~ .,data = Carseats)
summary(model)