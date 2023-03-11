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
# summary by each variable
# ------------------------------------------------------------------------------


summary(data)



# -->
# note that there are some NA's



# ----------
# min, mean, median, max, quantile
min(data$body)

mean(data$body)

median(data$body)

max(data$body)

quantile(data$body, c(0.25, 0.5, 0.75))




# ------------------------------------------------------------------------------
# data overview
# ------------------------------------------------------------------------------

Hmisc::describe(data)




# ------------------------------------------------------------------------------
# distribution by each variable
# ------------------------------------------------------------------------------

psych::describe(data)



# ------------------------------------------------------------------------------
# histogram
# ------------------------------------------------------------------------------

# help
help(hist)


args("hist.default")



hist(data$lifespan)



# ----------
seq(0, 100, by = 10)


hist(data$lifespan, 
     breaks = seq(0, 100, by = 10), include.lowest = TRUE, right = FALSE, col = "gray", 
     main = "LIFESPAN", xlab = "lifespan",
     ylim = c(0, 30))




# ----------
# histogram of all variable
graphics.off()

par(mfrow = c(3,4), mar = c(2,2,2,2))


var_name <- colnames(data)

lapply(1:ncol(data), 
       function(x) hist(data[,var_name[x]], xlab = "", ylab = "", main = var_name[x])
       )



# ------------------------------------------------------------------------------
# density 
# ------------------------------------------------------------------------------

par(mfrow = c(1,1), mar = c(2,2,2,2))


car::densityPlot(~ lifespan, data = data)



# ----------
# histogram and density  (note that na.exclude is required)

hist(data$lifespan, prob = TRUE)

lines(density(na.exclude(data$lifespan)))



