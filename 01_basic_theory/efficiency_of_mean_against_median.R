rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Efficiency of "mean" against "median"
# ------------------------------------------------------------------------------


# generate samples

samp <- sample(seq(1:100), size = 1000, replace = TRUE)
# samp <- rnorm(n = 1000, mean = 20, sd = 5)


hist(samp)



# mean
( mean_val <- mean(samp) )




# ----------
# take samples from "samp" and compute mean
# size should be small

gen_mean <- function(x){
  val <- mean(sample(samp, size = 10, replace = FALSE))
  return(val)
}



# repeat n1 times
n1 <- 1000

ts1 <- sapply(1:n1, gen_mean)




# ----------
# take samples from "samp" and compute median

# times <- 1
times <- 1 * 1.57^2

gen_median <- function(x){
  val <- median(sample(samp, size = 10 * times, replace = FALSE))
  return(val)
}



# repeat n2 times times
n2 <- n1

ts2 <- sapply(1:n2, gen_median)




# ----------
ts1_mean <- round(mean(ts1), 2)
ts1_var <- round(var(ts1), 2)


ts2_mean <- round(mean(ts2), 2)
ts2_var <- round(var(ts2), 2)



# the sqrt of variance ratio  --> this value is close to pi / 2 = 1.57
( varratio_sqrt <- round(sqrt(ts2_var / ts1_var), 3) )




# ----------
graphics.off()
par(mfrow = c(2,1))


hist(ts1, breaks = seq(0, 100, by = 5), ylim = c(0, n1 / 2), main = paste0("by mean:  variance : ", ts1_var))
abline(v = mean_val, lty = 2, col = gray(0.3), lwd = 2)
abline(v = ts1_mean, lty = 1, col = "blue", lwd = 2)


hist(ts2, breaks = seq(0, 100, by = 5), ylim = c(0, n2 / 2), main = paste0("by median  variance : ", ts2_var, "   sqrt of var ratio : ", varratio_sqrt))
abline(v = mean_val, lty = 2, col = gray(0.3), lwd = 2)
abline(v = ts2_mean, lty = 1, col = "blue", lwd = 2)


