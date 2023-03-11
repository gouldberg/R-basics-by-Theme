rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# central limit theorem
#   - fixed trials in 1 set  +  change sets
# ------------------------------------------------------------------------------

prob <- 1/6

trials <- 12

# trials <- 1200



# ----------
sets1 <- 10


# "trials" with probability = prob
# repeat "sets"

ts1 <- rbinom(size = trials, prob = prob, n = sets1)



# ----------
sets2 <- 100

ts2 <- rbinom(size = trials, prob = prob, n = sets2)




# ----------
sets3 <- 1000

ts3 <- rbinom(size = trials, prob = prob, n = sets3)




# ----------
sets4 <- 10000

ts4 <- rbinom(size = trials, prob = prob, n = sets4)




# ----------
graphics.off()
par(mfrow = c(2,2))

barplot(table(ts1), main = paste0(trials, ":trials", " * ", sets1, ":sets"))

barplot(table(ts2), main = paste0(trials, ":trials", " * ", sets2, ":sets"))

barplot(table(ts3), main = paste0(trials, ":trials", " * ", sets3, ":sets"))

barplot(table(ts4), main = paste0(trials, ":trials", " * ", sets4, ":sets"))


