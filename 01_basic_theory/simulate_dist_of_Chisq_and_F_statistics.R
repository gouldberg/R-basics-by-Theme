rm(list=ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Generate X^2 (Chi-squre) statistics distribution
# ------------------------------------------------------------------------------

stat <- data.frame()

for(df in 1:10){
  for(i in 1:10000){
    samp <- rnorm(n = df, mean = 0, sd = 1)
    stat[i,df] <- sum(samp^2)
  }
}



# ----------
min_all <- round(min(stat), 0)
max_all <- round(max(stat), 0)

( brk <- seq(min_all - 1, max_all + 1, by = 1) )


graphics.off()

par(mfrow = c(3,4), mar = c(2,2,2,2))

for(df in 1:10){ hist(stat[,df], breaks = brk, main = paste0("dist of Chisq: df = ", df), cex.main = 2) }



# ------------------------------------------------------------------------------
# Generate F statistics distribution
# ------------------------------------------------------------------------------

df1 <- 8

df2 <- 10

stat[,df1]

stat[,df2]


# ----------
# compute F statistics
fstat <- (stat[,df1] / df1) / (stat[,df2] / df2)


min_all <- round(min(fstat), 0)
max_all <- round(max(fstat), 0)

( brk <- seq(min_all - 1, max_all + 1, by = 1) )


graphics.off()

par(mfrow = c(1,1), mar = c(2,2,2,2))

hist(fstat, breaks = brk, main = paste0("dist of F stats: df1 = ", df1, " df2 = ", df2), cex.main = 2)
