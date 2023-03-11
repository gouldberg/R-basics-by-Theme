rm(list = ls())

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# mapply
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

ts.plot(econ5, col = 1:5)


MTSplot(econ5)



# -->
# Note that all the series has some upward trend




# ----------
apply(econ5, MARGIN = 2, FUN = forecast::ndiffs)



# -->
# Note that consum has 2 order difference




# ----------
# 1st difference
MTSplot(mapply(FUN = diff, econ5))



# 2nd order difference
MTSplot(mapply(FUN = diff, MoreArgs = list(diff = 2), econ5))




# ----------
psych::pairs.panels(mapply(FUN = diff, econ5))





# ------------------------------------------------------------------------------
# apply
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")


# growth rate
qr <- diff(log(ts(econ5, start = 1948, frequency = 4)))


plot(100 * qr, main = "Growth Rate (%)")



# ----------
# Scale to variance 1


# scale each series to have variance 1
# scaling strips ts attributes

qr <- ts(apply(qr, 2, scale), freq = 4)



