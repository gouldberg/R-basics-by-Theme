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
# data transformation for "body"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ body, data = data)



# transforming for symmetry  --> log transform is good !!
car::symbox(~ body, data = data)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(body ~ 1, data = data, family = "bcPower")


summary(p1)




# ----------
# check transformed data
tmp <- data %>% mutate(body2 = log10(body))

car::densityPlot( ~ body2, data = tmp)




# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "lifespan"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ lifespan, data = data)



# transforming for symmetry  --> log transform is good !!
car::symbox(~ lifespan, data = data)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "lifespan" untransformed is inconsistent with the goal of making the variabel normally distributed.

p1 <- car::powerTransform(lifespan ~ 1, data = data, family = "bcPower")

summary(p1)




# ----------
# check transformed data
tmp <- data %>% mutate(lifespan2 = log10(lifespan))

car::densityPlot( ~ lifespan2, data = tmp)



