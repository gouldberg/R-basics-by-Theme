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



# ----------
# ######  Calculate dream / sleep  ######

data$pdr <- with(data, dream / sleep)




# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots by scatterplotMatrix
# show all bivariate marginal relations
#
#   - by-group representation
#   - ellipse:  shows SDs and correation direction, the concentration is contolled by "levels"
#   - outliers:  by "id = list(n = x)"
#   - formula type:  can apply data transformation
# ------------------------------------------------------------------------------
# By default:
#   - diagonal panels:  nonparametric density estimates, using an adaptive-kernel estimator, with a rug-plot
#   - solid line:  marginal linear least-squares fit, ignoring the other variables
#   - central broken line:  nonparametric regression smooth
#   - outer broken lines:  smooths of the conditional variation of the y values givne x in each panel, like running quartiles
#   - size of the ellipse in the vertical and horizontal directions reflects the SDs of the two variables. 
# ------------------------------------------------------------------------------
#   - ellipse = list(levels = 0.5):  to get separate 50% concentration ellipses for the groups
#     If the data in a panel are bivariately normally distributed, then the ellipse encloses approximately 50% of the points
# ------------------------------------------------------------------------------

library(car)

formula <- ~ log10(body) + log10(brain) + sleep + log10(lifespan) + log10(gestation) + predation + exposure + danger + pdr


scatterplotMatrix(formula, data = data,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)




# ----------
# by group

formula <- ~ log10(body) + log10(brain) + log10(lifespan) + log10(gestation)  | (pdr >= 0.3)

formula <- ~ log10(body) + log10(brain) + log10(lifespan) | as.factor(predation)

scatterplotMatrix(formula, data = data,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = 1:5, pch = 1:5)


scatterplotMatrix(formula, data = data,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black"), pch = 20)





# ------------------------------------------------------------------------------
# Data Exploration:  Marginal plots by psych::describe
# show all bivariate marginal relations
#
#   - stars:  shows the significance of correlations
# ------------------------------------------------------------------------------

library(psych)


var_obj <- c("pdr", "body", "brain", "lifespan", "gestation")


# here we apply method = "spearman" due to terrible skewness
pairs.panels(data[, var_obj], ci = TRUE, smoother = TRUE, stars = TRUE, method = "spearman")


