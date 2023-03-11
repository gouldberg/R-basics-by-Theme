rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------

setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\00_basics\\02_correlation")


item6_quant <- read.csv("item6_quant.csv", header = T, sep = ",")


item6_quali <- read.csv("item6_quali.csv", header = T, sep = ",")


str(item6_quant)


str(item6_quali)



# ----------
# convert to factors
dat <- item6_quali
dat$���㕶 <- as.factor(dat$���㕶)
dat$�ÓT <- as.factor(dat$�ÓT)
dat$���� <- as.factor(dat$����)
dat$�n�w <- as.factor(dat$�n�w)
str(dat)



# ----------
# Correlation among all variables, selecting appropriate method automatically
cor_all <- polycor::hetcor(data = dat, std.err = TRUE, ML = TRUE, use = "complete.obs")
cor_all



# ----------
cor_cont <- cor(item6_quant, method = "pearson")
cor_disc <- cor_all$correlations





# ------------------------------------------------------------------------------
# Determining the number of factors by nfactors()
#   - nfactors() fits a sequence of EFA models with varying the number of factors
#     and prints out several criteria.
# ------------------------------------------------------------------------------

resnf1 <- psych::nfactors(item6_quant, n = 4, fm = "ml", cor = "cor")

resnf2 <- psych::nfactors(item6_quali, n = 4, fm = "ml", cor = "cor")


resnf1

resnf2

