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
# tabulation
# ------------------------------------------------------------------------------

table(data$predation, useNA = "always")


table(data$exposure, useNA = "always")


table(data$danger, useNA = "always")




# ------------------------------------------------------------------------------
# cross tabulation
# ------------------------------------------------------------------------------

xtabs(~ predation + exposure, data = data)


xtabs(~ predation + exposure + danger, data = data)




# ----------
# multiway table

tab <- xtabs(~ predation + exposure + danger, data = data)


tab


str(tab)




# ------------------------------------------------------------------------------
# change the order of dimension
# ------------------------------------------------------------------------------

aperm(tab, c(3,2,1))




# ------------------------------------------------------------------------------
# collapse table
# ------------------------------------------------------------------------------


margin.table(tab, c(1,3))




# ------------------------------------------------------------------------------
# flat table
# ------------------------------------------------------------------------------

ftable(tab, row.vars = c(1,2))




# ------------------------------------------------------------------------------
# numerical values in category and cross tabluation
# ------------------------------------------------------------------------------

tmp <- data


tmp$sleep_cat <- cut(tmp$sleep, 5)


tmp$lifespan_cat <- cut(tmp$lifespan, 5)


str(tmp)




# ----------
xtabs(~ sleep_cat + lifespan_cat, data = tmp)



