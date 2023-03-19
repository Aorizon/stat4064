#Clean global environment and packages
rm(list = ls())

if (!is.null(sessionInfo()$otherPkgs)) {
    invisible(
        lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
            detach, character.only=TRUE, unload=TRUE)
    )
}

#Prevent strings from being imported as 'factors'
options(stringsAsFactors = FALSE)

#Data loading, exploring and visualising
library(ggplot2)
library(patchwork)
library(reshape2)
library(ISLR)
