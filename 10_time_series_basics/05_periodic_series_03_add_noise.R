
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# Simulate periodic series:  3 time series  (cos + sine)
# ------------------------------------------------------------------------------


# time length 100
n <- 100


t <- 1:n


# omega:  cycels per unit time
omega <- c(0.06, 0.1, 0.4)



# a:  cosine coefficients for 3 time series
a <- c(2, 4, 6)



# b:  sine coefficients for 3 time series
b <- c(3, 5, 7)




# ----------
# generate time series
# different frequency and amplitude

# x1 <- a[1] * cos(2 * pi * omega[1] * t) + b[1] * sin(2 * pi * omega[1] * t)
x1 <- a[1] * cos(2 * pi * omega[1] * t) + b[1] * sin(2 * pi * omega[1] * t) + rnorm(n = n, mean = 0, sd = 1)


# x2 <- a[2] * cos(2 * pi * omega[2] * t) + b[2] * sin(2 * pi * omega[2] * t)
x2 <- a[2] * cos(2 * pi * omega[2] * t) + b[2] * sin(2 * pi * omega[2] * t) + rnorm(n = n, mean = 0, sd = 5)


# x3 <- a[3] * cos(2 * pi * omega[3] * t) + b[3] * sin(2 * pi * omega[3] * t)
x3 <- a[3] * cos(2 * pi * omega[3] * t) + b[3] * sin(2 * pi * omega[3] * t) + rnorm(n = n, mean = 0, sd = 5)




# ----------
# mixtures of periodic series with multiple frequencies and amplitudes

######### NOW WE ADD NOISE


# xall <- x1 + x2 + x3
xall <- x1 + x2 + x3 + rnorm(n = n, mean = 0, sd = 10)




# ----------
# A = sqrt(a^2 + b^2):  hight of amplitude for each time series

( A <- sqrt(a^2 + b^2) )



# ----------
# plot x1, x2, x3 and xall

graphics.off()

par(mfrow = c(2,2))

plot.ts(x1, ylim = c(-10, 10), main = paste0("omega: ", omega[1], "  A: ", round(A[1], 2)))

plot.ts(x2, ylim = c(-10, 10), main = paste0("omega: ", omega[2], "  A: ", round(A[2], 2)))

plot.ts(x3, ylim = c(-10, 10), main = paste0("omega: ", omega[3], "  A: ", round(A[3], 2)))

plot.ts(xall, ylim = c(-16, 16), main = "sum")




# ----------
# sum of A^2 / 2 = time series variance  -->  this equation does not hold here.
# HERE the variance is larger than sum(A^2)

sum(A^2 / 2)


var(xall) * (n - 1) / n





# ------------------------------------------------------------------------------
# plot time domain and frequency domain
# ------------------------------------------------------------------------------


graphics.off()

par(mfcol = c(2,4))

plot.ts(x1, ylim = c(-10, 10), main = paste0("omega: ", omega[1], "  A^2: ", A[1]^2))
abline(v = seq(0, n, 1 / 0.06), lty = 2, col = "gray")

mvspec(x1, log = "no", type = "h")


plot.ts(x2, ylim = c(-10, 10), main = paste0("omega: ", omega[2], "  A^2: ", A[2]^2))
abline(v = seq(0, n, 1 / 0.1), lty = 2, col = "gray")

mvspec(x2, log = "no", type = "h")


plot.ts(x3, ylim = c(-10, 10), main = paste0("omega: ", omega[3], "  A^2: ", A[3]^2))
abline(v = seq(0, n, 1 / 0.4), lty = 2, col = "gray")

mvspec(x3, log = "no", type = "h")

plot.ts(xall, ylim = c(-16, 16), main = "sum")

mvspec(xall, log = "no", type = "h")




# ------------------------------------------------------------------------------
# Fast Discrete Fourier Transform by fft
# ------------------------------------------------------------------------------

# ----------
# Distcrete Fourier Transform (DFT)

dft1 <- fft(x1) / sqrt(n)

dft2 <- fft(x2) / sqrt(n)

dft3 <- fft(x3) / sqrt(n)

dftall <- fft(xall) / sqrt(n)




# ----------
# Periodogram = vector length ^ 2

P1 <- abs(dft1)^2

P2 <- abs(dft2)^2

P3 <- abs(dft3)^2

Pall <- abs(dftall)^2




# ----------
# sum of periodogram  =  time series energy (sum of time series ^ 2)

sum(x1^2)

sum(abs(dft1)^2)


sum(x2^2)

sum(abs(dft2)^2)


sum(x3^2)

sum(abs(dft3)^2)


sum(xall^2)

sum(abs(dftall)^2)




# ------------------------------------------------------------------------------
# Scaled Periodgram
# ------------------------------------------------------------------------------


sP1 <- abs(dft1)^2 * 4 / n

sP2 <- abs(dft2)^2 * 4 / n

sP3 <- abs(dft3)^2 * 4 / n

sPall <- abs(dftall)^2 * 4 / n



# ----------
graphics.off()

par(mfrow = c(2,2))

Fr <- 0:(n-1)/n

plot(Fr, sP1, type = "h", xlab = "frequency", ylab = "scaled periodogram")

plot(Fr, sP2, type = "h", xlab = "frequency", ylab = "scaled periodogram")

plot(Fr, sP3, type = "h", xlab = "frequency", ylab = "scaled periodogram")

plot(Fr, sPall, type = "h", xlab = "frequency", ylab = "scaled periodogram")




# ----------
# sum of scaled periodogram > time series variance * 4

sum(sP1)
A[1]^2 * 2
var(x1) * { (n - 1) / n } * 4


sum(sP2)
A[2]^2 * 2
var(x2) * { (n - 1) / n } * 4


sum(sP3)
A[3]^2 * 2
var(x3) * { (n - 1) / n } * 4


sum(sPall)
sum(A^2) * 2
var(xall) * { (n - 1) / n } * 4





# ------------------------------------------------------------------------------
# parametric spectral estimation
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(2,4))


mvspec(x1, type = "h")
spec.ar(x1, method = "burg")


mvspec(x2, type = "h")
spec.ar(x2, method = "burg")


mvspec(x3, type = "h")
spec.ar(x3, method = "burg")


mvspec(xall, type = "h")
spec.ar(xall, method = "burg")


