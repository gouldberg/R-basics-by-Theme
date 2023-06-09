rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Generate data: one sample
# ------------------------------------------------------------------------------

x1 <- rnorm(n = 100, mean = 10, sd = 10)
x2 <- rnorm(n = 100, mean = 4, sd = 2)
x3 <- rnorm(n = 100, mean = 20, sd = 5)
x4 <- rnorm(n = 100, mean = 20, sd = 10)
x5 <- rnorm(n = 100, mean = 5, sd = 5)


intcpt <- 20
a <- 10
b <- 20
c <- 30
d <- 5
e <- 3


y <- intcpt + a * x1 + b * x2 + c * x3 + d * x4 + e * x5


data <- data.frame(cbind(y, x1, x2, x3, x4, x5))



# ----------
library(car)

formula <- ~ x1 + x2 + x3 + x4 + x5 + y


scatterplotMatrix(formula, data = data,
                  smooth = FALSE,
                  id = list(n = 3), ellipse = TRUE, col = gray(0.3), pch = 20)



# ------------------------------------------------------------------------------
# linear regression
# ------------------------------------------------------------------------------

formula1 <- y ~ x1 + x2
formula2 <- y ~ x1 + x2 + x3

lmod1 <- lm(formula = formula1, data = data)
lmod2 <- lm(formula = formula2, data = data)


summary(lmod1)

summary(lmod2)


resid1 <- sum(resid(lmod1)^2)
resid2 <- sum(resid(lmod2)^2)


# -----------
# AIC

( stat1 <- nrow(data) * log(resid1 / nrow(data)) )
( stat2 <- nrow(data) * log(resid2 / nrow(data)) )


( AIC1 <- stat1 + 2 * (length(coef(lmod1)) + 1) )
( AIC2 <- stat2 + 2 * (length(coef(lmod2)) + 1) )


-2 * logLik(lmod1) + 2 * (length(coef(lmod1)) + 1)
AIC(lmod1)

-2 * logLik(lmod2) + 2 * (length(coef(lmod1)) + 1)
AIC(lmod2)


# difference of AIC
AIC1 - AIC2

AIC(lmod1) - AIC(lmod2)



# -----------
# AIC

( BIC1 <- stat1 + log(nrow(data)) * (length(coef(lmod1)) + 1) )
( BIC2 <- stat2 + log(nrow(data)) * (length(coef(lmod2)) + 1) )


-2 * logLik(lmod1) + log(nrow(data)) * (length(coef(lmod1)) + 1)
BIC(lmod1)

-2 * logLik(lmod2) + log(nrow(data)) * (length(coef(lmod2)) + 1)
BIC(lmod2)


# difference of BIC
BIC1 - BIC2

BIC(lmod1) - BIC(lmod2)



# ------------------------------------------------------------------------------
# Simulate AIC and BIC
# ------------------------------------------------------------------------------

# note that e * x5 is only noize (mean = 0, sd = 1)
# so the y ~ x1 + x2 + x3 + x4 is almost clost to true model


intcpt <- 20

a <- 0.1
b <- 0.1
c <- 0.1
d <- 0.1
e <- 1


# ----------
samp_n <- 1000

stat1 <- stat2 <- stat3 <- stat4 <- stat5 <- stat6 <- c()
AIC1 <- AIC2 <- AIC3 <- AIC4 <- AIC5 <- AIC6 <- c()
BIC1 <- BIC2 <- BIC3 <- BIC4 <- BIC5 <- BIC6 <- c()


for(i in 1:samp_n){
  
  x1 <- rnorm(n = 10, mean = 40, sd = 20)
  x2 <- rnorm(n = 10, mean = 20, sd = 10)
  x3 <- rnorm(n = 10, mean = 7, sd = 4)
  x4 <- rnorm(n = 10, mean = 5, sd = 2)
  x6 <- rbinom(size = 10, n = 20, prob = 0.5)
  x7 <- rbinom(size = 10, n = 10, prob = 0.2)
  
  # x5 is mean = 0 and sd = 1 and coef e = 1:  only noize
  x5 <- rnorm(n = 10, mean = 0, sd = 1)
  
  # y <- intcpt + a * x1 + b * x2 + c * x3 + d * x4 + e * x5
  y <- intcpt + a * x1 + b * x2 + c * x3 + e * x5
  
  data <- data.frame(cbind(y, x1, x2, x3, x4, x5, x6, x7))
  
  
  formula1 <- y ~ x1
  formula2 <- y ~ x1 + x2
  formula3 <- y ~ x1 + x2 + x3
  formula4 <- y ~ x3 + x4 + x6
  formula4 <- y ~ x3 + x6 + x7
  formula5 <- y ~ x1 + x2 + x3 + x4
  formula6 <- y ~ x1 + x2 + x4 + x6 + x7
  
  lmod1 <- lm(formula = formula1, data = data)
  lmod2 <- lm(formula = formula2, data = data)
  lmod3 <- lm(formula = formula3, data = data)
  lmod4 <- lm(formula = formula4, data = data)
  lmod5 <- lm(formula = formula5, data = data)
  lmod6 <- lm(formula = formula6, data = data)
  
  resid1 <- sqrt(sum(resid(lmod1)^2))
  resid2 <- sqrt(sum(resid(lmod2)^2))
  resid3 <- sqrt(sum(resid(lmod3)^2))
  resid4 <- sqrt(sum(resid(lmod4)^2))
  resid5 <- sqrt(sum(resid(lmod5)^2))
  resid6 <- sqrt(sum(resid(lmod6)^2))
  
  stat1[i] <- nrow(data) * log(resid1 / nrow(data))
  stat2[i] <- nrow(data) * log(resid2 / nrow(data))
  stat3[i] <- nrow(data) * log(resid3 / nrow(data))
  stat4[i] <- nrow(data) * log(resid4 / nrow(data))
  stat5[i] <- nrow(data) * log(resid5 / nrow(data))
  stat6[i] <- nrow(data) * log(resid6 / nrow(data))
  
  AIC1[i] <- stat1[i] + 2 * (length(coef(lmod1)) + 1)
  AIC2[i] <- stat2[i] + 2 * (length(coef(lmod2)) + 1)
  AIC3[i] <- stat3[i] + 2 * (length(coef(lmod3)) + 1)
  AIC4[i] <- stat4[i] + 2 * (length(coef(lmod4)) + 1)
  AIC5[i] <- stat5[i] + 2 * (length(coef(lmod5)) + 1)
  AIC6[i] <- stat6[i] + 2 * (length(coef(lmod6)) + 1)

  BIC1[i] <- stat1[i] + log(nrow(data)) * (length(coef(lmod1)) + 1)
  BIC2[i] <- stat2[i] + log(nrow(data)) * (length(coef(lmod2)) + 1)
  BIC3[i] <- stat3[i] + log(nrow(data)) * (length(coef(lmod3)) + 1)
  BIC4[i] <- stat4[i] + log(nrow(data)) * (length(coef(lmod4)) + 1)
  BIC5[i] <- stat5[i] + log(nrow(data)) * (length(coef(lmod5)) + 1)
  BIC6[i] <- stat6[i] + log(nrow(data)) * (length(coef(lmod6)) + 1)
}



# ------------------------------------------------------------------------------
# plot stat, AIC, and BIC for each model
# ------------------------------------------------------------------------------

min_all <- round(min(stat1, stat2, stat3, stat4, stat5, stat6, AIC1, AIC2, AIC3, AIC4, AIC5, AIC6, BIC1, BIC2, BIC3, BIC4, BIC5, BIC6), 0)
max_all <- round(max(stat1, stat2, stat3, stat4, stat5, stat6, AIC1, AIC2, AIC3, AIC4, AIC5, AIC6, BIC1, BIC2, BIC3, BIC4, BIC5, BIC6), 0)

( brk <- seq(min_all - 1, max_all + 1, by = 1) )


graphics.off()
par(mfcol = c(6,3), mar = c(2,2,2,2))

hist(stat1, breaks = brk)
hist(stat2, breaks = brk)
hist(stat3, breaks = brk)
hist(stat4, breaks = brk)
hist(stat5, breaks = brk)
hist(stat6, breaks = brk)

hist(AIC1, breaks = brk)
hist(AIC2, breaks = brk)
hist(AIC3, breaks = brk)
hist(AIC4, breaks = brk)
hist(AIC5, breaks = brk)
hist(AIC6, breaks = brk)

hist(BIC1, breaks = brk)
hist(BIC2, breaks = brk)
hist(BIC3, breaks = brk)
hist(BIC4, breaks = brk)
hist(BIC5, breaks = brk)
hist(BIC6, breaks = brk)



# -->
# Note that
# stat5 < stat3 but AIC5 > AIC3




# ------------------------------------------------------------------------------
# plot stat, AIC, and BIC for model 3 and model 5
# ------------------------------------------------------------------------------

min_all <- round(min(stat3, stat5, AIC3, AIC5, BIC3, BIC5), 0)
max_all <- round(max(stat3, stat5, AIC3, AIC5, BIC3, BIC5), 0)

( brk <- seq(min_all - 1, max_all + 1, by = 1) )


graphics.off()
par(mfcol = c(2,3), mar = c(2,2,2,2))

hist(stat3, breaks = brk)
hist(stat5, breaks = brk)

hist(AIC3, breaks = brk)
hist(AIC5, breaks = brk)

hist(BIC3, breaks = brk)
hist(BIC5, breaks = brk)


