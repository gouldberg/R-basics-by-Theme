rm(list = ls())



# set working directory
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\00_data_handling_overview_and_visualization")


# show current working directory
getwd()



# ----------
library(dplyr)




# ------------------------------------------------------------------------------
# data:  mammalsleep
# ------------------------------------------------------------------------------


# read data from file
mammalsleep <- read.csv(".\\data\\mammalsleep.txt", header = T, stringsAsFactors = F, sep = "\t")



# read data from package
# data("mammalsleep", package = "faraway")


data <- mammalsleep



# ------------------------------------------------------------------------------
# Data exploration:  multi-panel Cleveland dotplot
# ------------------------------------------------------------------------------


library(lattice)


Mydotplot <- function(DataSelected){
  
  P <- dotplot(as.matrix(as.matrix(DataSelected)), groups=FALSE,
               strip = strip.custom(bg = 'white', par.strip.text = list(cex = 1.2)),
               scales = list(x = list(relation = "free", draw = TRUE), y = list(relation = "free", draw = FALSE)),
               col=1, cex  = 0.5, pch = 16,
               xlab = list(label = "Value of the variable", cex = 1.5),
               ylab = list(label = "Order of the data from text file", cex = 1.5))
  
  print(P)  
}




# ----------
graphics.off()


MyVar1 <- c("body", "brain", "dream", "lifespan", "gestation", "predation", "exposure", "danger")


Mydotplot(as.matrix(data[,MyVar1]))



