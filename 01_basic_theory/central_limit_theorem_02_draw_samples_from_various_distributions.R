rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# distribution of sample mean, samples from skewed various distributions
# ------------------------------------------------------------------------------

n <- 10000


# ----------
# binomial disribution
prob <- 1/6

sets <- 120

ts1 <- rbinom(n = n, size = sets, prob = prob)
mean1 <- mean(ts1)
var1 <- var(ts1)



# ----------
# normal variable ^ 2
mu <- 10

sd <- 3

ts2 <- rnorm(n = n, mean = mu, sd = sd)^2
mean2 <- mean(ts2)
var2 <- var(ts2)



# ----------
# 3 / normal variable 
mu <- 0.1

sd <- 0.01

ts3 <- 3 / rnorm(n = n, mean = mu, sd = sd)
mean3 <- mean(ts3)
var3 <- var(ts3)



# ----------
# gamma distribution
shape <- 0.4

scale <- 100

ts4 <- rgamma(n = n, shape = shape, scale = scale)
mean4 <- mean(ts4)
var4 <- var(ts4)



# ----------
# all in one distribution
ts5 <- c(ts1, ts2, ts3, ts4)
mean5 <- mean(ts5)
var5 <- var(ts5)



# ----------
graphics.off()

par(mfrow = c(2,2))

hist(ts1)

hist(ts2)

hist(ts3)

hist(ts4)

# hist(ts5)



# ----------
# in one data frame

# dat_mat <- matrix(c(ts1, ts2, ts3, ts4), ncol = 4, nrow = n)
dat_mat <- matrix(c(ts1, ts2, ts4), ncol = 3, nrow = n)

nrow(dat_mat)



# ----------
# take average by each data

ave <- apply(dat_mat, 1, FUN = mean)

# ave <- apply(dat_mat, 1, FUN = prod)


# ----------
# the distribution of the average
par(mfrow = c(1,1))

hist(ave)

psych::describe(data.frame(ave, ts1, ts2, ts3, ts4))

round(c(mean(ave), mean1, mean2, mean3, mean4, mean(c(mean1, mean2, mean3, mean4))), 0)

round(c(var(ave), var1, var2, var3, var4, sum(c(var1, var2, var3, var4))), 0)



# car::densityPlot(ave)



# -->
# Here the some of the original distribution is restriced as the values are positive
# so the distribution of average is close to log normal type distribution




# ------------------------------------------------------------------------------
# distribution of sample mean, samples from skewed various distributions
# ------------------------------------------------------------------------------

n <- 100000


# ----------
# binomial disribution
prob <- 1/6

sets <- 120

ts1 <- apply(matrix(rbinom(n = n, size = sets, prob = prob), ncol = 100), 1, FUN = mean)
mean1 <- mean(ts1)
var1 <- var(ts1)



# ----------
# normal variable ^ 2
mu <- 10

sd <- 3

ts2 <- apply(matrix(rnorm(n = n, mean = mu, sd = sd)^2, ncol = 100), 1, FUN = mean)
mean2 <- mean(ts2)
var2 <- var(ts2)



# ----------
# 3 / normal variable 
mu <- 0.1

sd <- 0.01

ts3 <- apply(matrix(3 / rnorm(n = n, mean = mu, sd = sd), ncol = 100), 1, FUN = mean)
mean3 <- mean(ts3)
var3 <- var(ts3)



# ----------
# gamma distribution
shape <- 0.4

scale <- 100

ts4 <- apply(matrix(rgamma(n = n, shape = shape, scale = scale), ncol = 100), 1, FUN = mean)
mean4 <- mean(ts4)
var4 <- var(ts4)



# ----------
# all in one distribution
ts5 <- c(ts1, ts2, ts3, ts4)
mean5 <- mean(ts5)
var5 <- var(ts5)



# ----------
graphics.off()

par(mfrow = c(2,2))

hist(ts1)

hist(ts2)

hist(ts3)

hist(ts4)

# hist(ts5)



# ----------
# in one data frame

# dat_mat <- matrix(c(ts1, ts2, ts3, ts4), ncol = 4, nrow = n)
dat_mat <- matrix(c(ts1, ts2, ts4), ncol = 3, nrow = n)

nrow(dat_mat)



# ----------
# take average by each data

ave <- apply(dat_mat, 1, FUN = mean)

# ave <- apply(dat_mat, 1, FUN = prod)


# ----------
# the distribution of the average
par(mfrow = c(1,1))

hist(ave)


psych::describe(data.frame(ave, ts1, ts2, ts3, ts4))

round(c(mean(ave), mean1, mean2, mean3, mean4, mean(c(mean1, mean2, mean3, mean4))), 0)

round(c(var(ave), var1, var2, var3, var4, sum(c(var1, var2, var3, var4))), 0)



# car::densityPlot(ave)



# -->
# Here the some of the original distribution is restriced as the values are positive
# so the distribution of average is close to log normal type distribution


