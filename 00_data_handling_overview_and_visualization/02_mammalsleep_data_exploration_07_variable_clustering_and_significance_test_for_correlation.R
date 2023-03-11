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
# data exploration:  Correlation plot and variable clustering
# ------------------------------------------------------------------------------


library(corrplot)


var_obj <- c("pdr", "lifespan", "gestation", "body", "brain")


corrplot(cor(na.omit(data[,var_obj])), method = "color", cl.length = 20, order = "AOE", addcoef.col = "grey")


corrplot(cor(na.omit(data[,var_obj])), order = "hclust", hclust.method = "ward.D2", addrect = 3)




# ------------------------------------------------------------------------------
# data exploration:  Variable Clustering
#   - Hierarchical cluster analysis on an similatrity matrix (such as squared correlations)
#   - It is often advisable to use robust (e.g., rank-based) measures for continuous variables if they are skewed, as skewed variables can
#     greatly affect ordinary correlation coefficeients.
#   - Pairwise deletion of missing values is also advisable for thie procedure:  casewise deletion can result in a small biased sample.
#   - When variables are not monotonically related to each other, Pearson or Spearman squared correlations can miss important associations and thus
#     are not always good similarity measures.
#     General and robust similarity measure is Hoeffding's D:  D statistic will detect a wide variety of dependencies between two variables.
# ------------------------------------------------------------------------------

library(Hmisc)


# We use Hoeffding's D: D will detect nonmonotonic associations
vc <- varclus(~ ., data = data[,var_obj], sim="hoeffding")

plot(vc)



