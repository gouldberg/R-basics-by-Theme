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
# summary by category
# ------------------------------------------------------------------------------

with(data, by(dream, predation, FUN = summary))


with(data, by(dream, exposure, FUN = summary))




# ------------------------------------------------------------------------------
# histogram by category
# ------------------------------------------------------------------------------

graphics.off()

lattice::histogram(~ lifespan | as.factor(predation), data = data)

lattice::histogram(~ lifespan, data = data)



# ------------------------------------------------------------------------------
# boxplot by category
# ------------------------------------------------------------------------------


graphics.off()

boxplot(lifespan ~ as.factor(exposure), data = data)

boxplot(lifespan ~ as.factor(danger), data = data)



