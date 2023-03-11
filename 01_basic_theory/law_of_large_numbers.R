rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Law of large numbers
# ------------------------------------------------------------------------------

# normal distribution

n <- 200
mu <- 100
sd <- 30


ts1 <- rnorm(n = n, mean = mu, sd = sd)


plot(ts1, type = "l")


# ----------
# gamma distribution

# n <- 200
# shape <- 20
# scale <- 10

# ts1 <- rgamma(n = n, shape = shape, scale = scale)



# ----------
ts2 <- cumsum(ts1)

ts3 <- cumsum(ts1) / 1:n


dat_mat <- matrix(c(ts1, ts2, ts3), ncol = 3, nrow = n)



# ----------
# each value, cumsum, average
par(mar = c(2,2,2,2))

MTS::MTSplot(dat_mat)


# note that
# normal distribution:  mean = mu
# gamma distribution:  mean = shape * scale



# ----------
car::densityPlot(ts1)


