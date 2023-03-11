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
# continuous Y   vs.  continous X
# ------------------------------------------------------------------------------


with(data, plot(lifespan, dream / sleep, pch = 20, col = "black", cex = 1.2))

with(na.omit(data), lines(lowess(lifespan, dream / sleep), col = "blue", lty = 2))



# ----------
lattice::xyplot(dream / sleep ~ lifespan, data = data, type = c("p", "smooth"), col = "black")




# ------------------------------------------------------------------------------
# continuous Y   vs.  continous X
# ------------------------------------------------------------------------------

library(ggplot2)


ggplot(data, aes(lifespan, dream / sleep)) + geom_point() +
  stat_smooth(method = "loess", size = 2, fill = "blue", alpha = 0.25) +
  stat_smooth(method = "lm", color = "red", size = 1.25, se = FALSE)



