rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\qtdbsel102")



# ------------------------------------------------------------------------------
# data:  qtdbsel102
# ------------------------------------------------------------------------------


data <- read.csv(file = "qtdbsel102.txt", header = FALSE, sep = "")



str(data)


head(data)




# ------------------------------------------------------------------------------
# HOT-SAX algorithm for time series discord discovery
# demonstration
# ------------------------------------------------------------------------------



# PAA (Piecewise Aggregate Approximation) reduce the input time series dimensionality
# by splitting it into equally-sized segments (PAA size) and averaging values of points within each segment. 

# for example

y <- c(-1, -2, -1, 0, 2, 1, 1, 0)

y_paa3 <- paa(y, 3)

plot(y, type = "l", col = "blue", main = "8-points time series and it PAA transform into 3 points")
points(y, pch = 16, lwd = 5, col = "blue")
abline(v = c(1, 1+7/3, 1+7/3*2, 8), lty = 3, lwd = 2, col = "gray50")


segments(1,y_paa3[1],1+7/3,y_paa3[1],lwd=1,col="red")
points(x=1+7/3/2,y=y_paa3[1],col="red",pch=23,lwd=5)
segments(1+7/3,y_paa3[2],1+7/3*2,y_paa3[2],lwd=1,col="red")
points(x=1+7/3+7/3/2,y=y_paa3[2],col="red",pch=23,lwd=5)
segments(1+7/3*2,y_paa3[3],8,y_paa3[3],lwd=1,col="red")
points(x=1+7/3*2+7/3/2,y=y_paa3[3],col="red",pch=23,lwd=5)

y <- seq(-2,2, length=100)
x <- dnorm(y, mean=0, sd=1) + 1
lines(x,y, type="l", lwd=5, col="magenta")
abline(h = alphabet_to_cuts(3)[2:3], lty=2, lwd=2, col="magenta")
text(1.7,-1,"a",cex=2,col="magenta")
text(1.7, 0,"b",cex=2,col="magenta")
text(1.7, 1,"c",cex=2,col="magenta")
series_to_string(y_paa3, 3)
series_to_chars(y_paa3, 3)




# ------------------------------------------------------------------------------
# HOT-SAX algorithm for time series discord discovery for V2
# ------------------------------------------------------------------------------

dat <- data[,2]



library(jmotif)




# ----------
# window size is important !!!

w_size <- 100


# paa_size:  the PAA size (Piecewise Aggregate Approximation)
# a_size:  the alphabet size
# discords_num:  number of discords to report

discords <- find_discords_hotsax(dat, w_size = w_size, paa_size = 4, a_size = 4, n_threshold = 0.01, discords_num = 5)


discords



# -->
# The best discord is the first one at 4233:



# ----------

( idx <- discords$position[which.max(discords$nn_distance)] )



# ----------

graphics.off()

par(mfrow = c(1,1))

plot(dat, type = "l", col = "cornflowerblue")

lines(x = c(idx:(idx + w_size)), y = dat[idx:(idx + w_size)], col = "red", lwd = 2)




# ------------------------------------------------------------------------------
# HOT-SAX algorithm for time series discord discovery for V3
# ------------------------------------------------------------------------------


dat <- data[,3]

plot(dat, type = "l")



# window size is important !!!

w_size <- 100


# paa_size:  the PAA size (Piecewise Aggregate Approximation)
# a_size:  the alphabet size
# discords_num:  number of discords to report

discords <- find_discords_hotsax(dat, w_size = w_size, paa_size = 4, a_size = 4, n_threshold = 0.01, discords_num = 5)


discords


# ----------

( idx <- discords$position[which.max(discords$nn_distance)] )



# ----------

graphics.off()

par(mfrow = c(1,1))

plot(dat, type = "l", col = "cornflowerblue")

lines(x = c(idx:(idx + w_size)), y = dat[idx:(idx + w_size)], col = "red", lwd = 2)





# ------------------------------------------------------------------------------
# Grammatical inference with RePair
#   - RePair is a dictionary-based compression method proposed in 1999 by Larsson and Moffat.
#     In contrast with Sequitur, Repair is an off-line algorithm that requires the whole input sequence
#     to be accessible before building a grammar.
#     Similar to Sequitur, RePair also can be utilized as a grammar-based compressor able to discover a compact grammar
#     that generates the text. It is a remarkably simple algorithm which is known for its very fast decompression.
#   - In short, RePair performs a recursive pairing step
#      -- finding the most frequent pair of symbols in the input sequence and replacing it with a new symbol
#      -- until every pair appears only once.
# ------------------------------------------------------------------------------


dat <- data[,2]

# dat <- data[,3]




# ----------
# discretize the data 

# discretization parameters
w <- 100

p <- 8

a <- 8


dat_sax <- sax_via_window(dat, w, p, a, "none", 0.01)


# time point = 1,2
dat_sax[[1]]
dat_sax[[2]]




# ----------
# get the string representation of time series
dat_str <- paste(dat_sax, collapse = " ")

dat_str



# infer the grammar
dat_grammar <- str_to_repair_grammar(dat_str)   


# 1st rule of the grammar (second list element)
str(dat_grammar[[2]])

dat_grammar[[2]]




# ----------
# initialize the density curve
density_curve <- rep(0, length(dat))


rule$rule_interval_starts



# account for all the rule intervals

for(i in 2:length(dat_grammar)){
  
  rule <- dat_grammar[[i]]
  
  for(j in 1:length(rule$rule_interval_starts)){
    
    xs <- rule$rule_interval_starts[j]
    
    xe <- rule$rule_interval_ends[j] + w
    
    density_curve[xs:xe] <- density_curve[xs:xe] + 1
  }
}




# ----------
# plot the density curve

graphics.off()

par(mfrow = c(2,1))

plot(dat, type = "l")

plot(density_curve, type = "l")


