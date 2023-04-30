# Load library and data
library(ISLR2)
data(Hitters)
Hitters_clean <- Hitters[!is.na(Hitters$Salary), ]

# Forward selection
#RSS_model <- step(lm(Salary~1,data=Hitters_clean),criteria=RSS,direction ="forward",scope=formula(lm(Salary~.,data=Hitters_clean)))
BIC_model <- step(lm(Salary~CRBI,data=Hitters_clean),criteria="BIC",direction ="forward",scope=formula(lm(Salary~.,data=Hitters_clean)))
#subset_model <- step(lm(Salary~.,data=Hitters_clean),criteria="BIC",direction ="both")

# # Generate predictors DF
# Hitters_pred <- subset(Hitters_clean, select = -c(Salary))
# 
# # Generate list of DF indices
# indices <- c(1:length(Hitters_pred))
# 
# # Generate all combinations of variables
# subsets <- unlist(lapply(1:length(indices), function(i) combn(indices, i, simplify = FALSE)), recursive = FALSE)
# subsets_string <- paste(subsets, collapse = ",")
# 
# # Best subset
# for (i in 1:1#length(subsets) 
#      model<-lm(Salary~Hitters_pred,data=Hitters_clean)
#      # Calculate the RSS
#      rss <- sum(resid(model)^2)
#      )
# best_subset <- step()
# 
# # Example data
# subsets <- as.list(names(Hitters_clean))
# 
# lm(Salary~1,data=Hitters_clean)










# # Forward selection using RSS criterion
# RSS_model <- step(lm(Salary ~ 1, data = Hitters_clean), 
#                   direction = "forward", 
#                   trace = 1, 
#                   k = log(nrow(Hitters_clean)))
# 
# # Find the model with the lowest RSS
# RSS <- sapply(RSS_model$anova, "[[", "RSS")
# best_model_RSS <- RSS_model[[which.min(RSS)]]
# 
# # Forward selection using BIC criterion
# BIC_model <- step(lm(Salary ~ 1, data = Hitters_clean), 
#                   direction = "forward", 
#                   trace = 1, 
#                   k = log(nrow(Hitters_clean)), 
#                   criterion = "bic")
# 
# # Find the model with the lowest BIC
# BIC <- sapply(BIC_model$anova, "[[", "BIC")
# best_model_BIC <- BIC_model[[which.min(BIC)]]
