rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# central limit theorem:  from tri-modal distribution
# generate the distribution
# ------------------------------------------------------------------------------

n <- 1000



# ----------

# mixture of 3 different normal distribution
mu <- -100
sd <- 5

ts1 <- rnorm(n = n, mean = mu, sd = sd)


mu <- 200
sd <- 5

ts2 <- rnorm(n = n, mean = mu, sd = sd)


mu <- 700
sd <- 10

ts3 <- rnorm(n = n, mean = mu, sd = sd)


ts4 <- c(ts1, ts2, ts3)


par(mfrow = c(1,1))

car::densityPlot(ts4)


psych::describe(ts4)



# ----------
( val_mean <- round(mean(ts4), 3) )

( val_var <- round(var(ts4), 3) )




# ------------------------------------------------------------------------------
# central limit theorem:  take samples and compute average
#   - the numbers of samples of 1 set are different
# ------------------------------------------------------------------------------



# draw "draw_sample" samples from ts4 and take average
# repeate "sets"

sets <- 100



# ----------
draw_sample1 <- 5

ave1 <- sapply(1:sets, function(a) mean(sample(x = ts4, size = draw_sample1, replace = TRUE)))

mean1 <- round(mean(ave1), 3)
var1 <- round(var(ave1), 0)
mean_diff1 <- mean1 - val_mean


# ----------
draw_sample2 <- 10

ave2 <- sapply(1:sets, function(a) mean(sample(x = ts4, size = draw_sample2, replace = TRUE)))

mean2 <- round(mean(ave2), 3)
var2 <- round(var(ave2), 0)
mean_diff2 <- mean2 - val_mean



# ----------
draw_sample3 <- 25

ave3 <- sapply(1:sets, function(a) mean(sample(x = ts4, size = draw_sample3, replace = TRUE)))

mean3 <- round(mean(ave3), 3)
var3 <- round(var(ave3), 0)
mean_diff3 <- mean3 - val_mean



# ----------
draw_sample4 <- 100

ave4 <- sapply(1:sets, function(a) mean(sample(x = ts4, size = draw_sample4, replace = TRUE)))

mean4 <- round(mean(ave4), 3)
var4 <- round(var(ave4), 0)
mean_diff4 <- mean4 - val_mean



# ----------
par(mfrow = c(2,2))

hist(ave1, main = paste0("ave of ", draw_sample1, " smps * ", sets, " sets", " var:", var1))
abline(v = val_mean, lty = 2, col = gray(0.3), lwd = 2)
abline(v = mean1, lty = 1, col = "blue", lwd = 2)

hist(ave2, main = paste0("ave of ", draw_sample2, " smps * ", sets, " sets", " var:", var2))
abline(v = val_mean, lty = 2, col = gray(0.3), lwd = 2)
abline(v = mean2, lty = 1, col = "blue", lwd = 2)

hist(ave3, main = paste0("ave of ", draw_sample3, " smps * ", sets, " sets", " var:", var3))
abline(v = val_mean, lty = 2, col = gray(0.3), lwd = 2)
abline(v = mean3, lty = 1, col = "blue", lwd = 2)

hist(ave4, main = paste0("ave of ", draw_sample4, " smps * ", sets, " sets", " var:", var4))
abline(v = val_mean, lty = 2, col = gray(0.3), lwd = 2)
abline(v = mean4, lty = 1, col = "blue", lwd = 2)


c(mean_diff1, mean_diff2, mean_diff3, mean_diff4)


# -->
# NOTE THAT: the variance will be reduced by proportion to the number of one time samples
# the variance of "25 samples at 1 time" is 1/5 of the variance of "5 samples at 1 time"




# ------------------------------------------------------------------------------
# central limit theorem:  take samples and compute average
#   - the numbers of samples of 1 set are fixed, but sets are increasing
# ------------------------------------------------------------------------------

# draw "draw_sample" samples from ts4 and take average
# repeate "sets"


draw_sample <- 10



# ----------
sets1 <- 10

ave1 <- sapply(1:sets1, function(a) mean(sample(x = ts4, size = draw_sample, replace = TRUE)))

mean1 <- round(mean(ave1), 3)
var1 <- round(var(ave1), 0)
mean_diff1 <- mean1 - val_mean



# ----------
sets2 <- 100

ave2 <- sapply(1:sets2, function(a) mean(sample(x = ts4, size = draw_sample, replace = TRUE)))

mean2 <- round(mean(ave2), 3)
var2 <- round(var(ave2), 0)
mean_diff2 <- mean2 - val_mean



# ----------
sets3 <- 1000

ave3 <- sapply(1:sets3, function(a) mean(sample(x = ts4, size = draw_sample, replace = TRUE)))

mean3 <- round(mean(ave3), 3)
var3 <- round(var(ave3), 0)
mean_diff3 <- mean3 - val_mean



# ----------
sets4 <- 10000

ave4 <- sapply(1:sets4, function(a) mean(sample(x = ts4, size = draw_sample, replace = TRUE)))

mean4 <- round(mean(ave4), 3)
var4 <- round(var(ave4), 0)
mean_diff4 <- mean4 - val_mean




# ----------
par(mfrow = c(2,2))

hist(ave1, main = paste0("ave of ", draw_sample, " smps * ", sets1, " sets", " var:", var1))
abline(v = val_mean, lty = 2, col = gray(0.3), lwd = 2)
abline(v = mean1, lty = 1, col = "blue", lwd = 2)

hist(ave2, main = paste0("ave of ", draw_sample, " smps * ", sets2, " sets", " var:", var2))
abline(v = val_mean, lty = 2, col = gray(0.3), lwd = 2)
abline(v = mean2, lty = 1, col = "blue", lwd = 2)

hist(ave3, main = paste0("ave of ", draw_sample, " smps * ", sets3, " sets", " var:", var3))
abline(v = val_mean, lty = 2, col = gray(0.3), lwd = 2)
abline(v = mean3, lty = 1, col = "blue", lwd = 2)

hist(ave4, main = paste0("ave of ", draw_sample, " smps * ", sets4, " sets", " var:", var4))
abline(v = val_mean, lty = 2, col = gray(0.3), lwd = 2)
abline(v = mean4, lty = 1, col = "blue", lwd = 2)


c(mean_diff1, mean_diff2, mean_diff3, mean_diff4)


psych::describe(data.frame(ave1, ave2, ave3, ave4))

( val_var <- round(var(ts4), 3) )


# -->
# NOTE that the mean of distribution is closing to true means and closing to normal distribution
# kurtosis is closing to 0

