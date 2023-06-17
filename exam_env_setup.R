aux <- version
if (((as.numeric(aux$major) >= 3) && (as.numeric(aux$minor) >= 6)) ||
    (as.numeric(aux$major) >= 4)) {
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion",
          sample.kind = "Rounding")
} else {
  RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion")
}
set.seed(12345)
cat("1st check: 5 = ",sample(1:6,1),"\n",sep="")
cat("2nd check: 9 = ",ceiling(runif(1,0,10)),"\n",sep="")
cat("If the statements above are right, then it is ok.\n",sep="")
cat("\n",sep="")
cat("If you get a warning message as below, this is ok. \n",
    " 'Warning message in RNGkind(kind = ",'"',"Mersenne-Twister",'"',
    ", normal.kind = ",'"',"Inversion",'"',", :\n",' "',
    "non-uniform 'Rounding' sampler used",'"',"\n",sep="")

# Global environment separation
rm(list = ls())
if (!is.null(sessionInfo()$otherPkgs)) {
  invisible(
    lapply(paste0('package:', names(sessionInfo()$otherPkgs)), 
           detach, character.only=TRUE, unload=TRUE)
  )
}

# Prevent strings being imported as factors, unless specified
options(stringsAsFactors = FALSE)

# Set working directory
setwd("C:/repos/stat4064")
# getwd() #to check

library(MASS)
library(leaps)