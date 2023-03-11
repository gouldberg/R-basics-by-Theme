
rm(list = ls())



# set working directory
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\00_data_handling_overview_and_visualization")


# show current working directory
getwd()



# ----------
library(dplyr)




# ------------------------------------------------------------------------------
# data:  mammalsleep
#   - In Allison and Cicchetti (1976), data on the sleep behavior of 62 mammals is presented.
#     Suppose we are interested in modeling the proportion of sleep spent dreaming as a function of the other predictors:
#       - the weight of the body and the brain
#       - the gestation period
#       - the lifespan
#       - the three constrcted indices measuring vulnerability to predation, exposure while sleeping and overall danger
# ------------------------------------------------------------------------------


# read data from file
mammalsleep <- read.csv(".\\data\\mammalsleep.txt", header = T, stringsAsFactors = F, sep = "\t")



# read data from package
# data("mammalsleep", package = "faraway")



# ----------
# show objects on memory
ls()



# ----------
# copy

data <- mammalsleep




# ----------
# check the structure

str(data)



# ----------
# check the dimension

dim(data)


nrow(data)


ncol(data)




# ----------
# head and tail

head(data, 10)


tail(data, 10)



# ----------
# row.names and column names

rownames(data)


colnames(data)



# ----------
# show sample

car::some(data)




# ------------------------------------------------------------------------------
# access the elements
# ------------------------------------------------------------------------------

# 1st row

data[1,]


data["African.elephant",]




# ----------
# 1st column

data[,1]


data[,"body"]


data$body




# ----------
# 1st row and 1st column

data[1,1]


data[1,"body"]


data$body[1]




# ----------
# 1st and 5th row

data[c(1,5),]



# ----------
# 1st to 10th row

data[1:10,]



# ----------
# 1st to 10th column

data[,1:10]





# ------------------------------------------------------------------------------
# change row names
# ------------------------------------------------------------------------------

colnames(data)[1] <- "body1"


head(data)



# back to original
colnames(data)[1] <- "body"




# ----------
# save original column name
original_colname <- colnames(data)


change_colname <- paste0("X_", 1:10)


colnames(data) <- change_colname


head(data)



# ----------
# back to original column name
colnames(data) <- original_colname




# ------------------------------------------------------------------------------
# change order of column
# ------------------------------------------------------------------------------


var_obj <- c("body", "brain", "nondream", "dream", "sleep", "lifespan", "gestation", "predation", "exposure", "danger")



data <- data[,var_obj]




# ------------------------------------------------------------------------------
# sort by "body"
# ------------------------------------------------------------------------------

order(data$body)


order(data$body, decreasing = TRUE)


head(data[order(data$body, decreasing = TRUE), ])



# ----------
# use dplyr funciton
data %>% arrange(desc(body)) %>% head()



data %>% arrange(desc(body), lifespan) %>% head()




# ------------------------------------------------------------------------------
# filter and get index
# ------------------------------------------------------------------------------


# body >= 10
head(data[data$body >= 100,])


# index
which(data$body >= 100)




# -----------
# user "dplyr" function

data %>% filter(body >= 100, brain >= 1000)


tmp <- data %>% filter(body >= 100, brain >= 1000)


tmp



# ----------
#() prints simultaneously

( tmp <- data %>% filter(body >= 100, brain >= 1000) )



# ------------------------------------------------------------------------------
# filter and select
# ------------------------------------------------------------------------------

# get as data frame
data[data$body >= 100, "lifespan"]



# get as data frame
data %>% filter(body >= 100) %>% dplyr::select(lifespan)
                                               


# get as vector                                               
data %>% filter(body >= 100) %>% dplyr::select(lifespan) %>% pull()




# ------------------------------------------------------------------------------
# counts of NA
# ------------------------------------------------------------------------------

is.na(data$dream)


# counting "TRUE"
sum(is.na(data$dream))



# ----------
# counts of NA by each column

myfunc <- function(df){ sum(is.na(df)) }

apply(data, MARGIN = 2, FUN = myfunc)



# ----------
# counts of NA by each row

apply(data, MARGIN = 1, FUN = myfunc)




# ------------------------------------------------------------------------------
# NA omit, NA exclude
# ------------------------------------------------------------------------------

tmp <- na.exclude(data)

# tmp <- na.omit(data)



dim(data)

dim(tmp)


sum(is.na(tmp))




# ------------------------------------------------------------------------------
# col-sum and row-sum
# ------------------------------------------------------------------------------


colSums(data)


rowSums(data)




# ------------------------------------------------------------------------------
# remove "tmp" from memory
# ------------------------------------------------------------------------------

rm(tmp)



# ----------
# show objects on memory
ls()



# remove all object from memory
# rm(list=ls())




# ------------------------------------------------------------------------------
# save data frame as text file
# ------------------------------------------------------------------------------

write.table(data, file = ".\\data\\tmp.txt", row.names = TRUE, quote = FALSE, sep = "\t")




# ------------------------------------------------------------------------------
# save as R object
# ------------------------------------------------------------------------------

save(data, file = ".\\data\\tmp.R")


load(".\\data\\tmp.R")



# the data name is "data" (the name of the object when saved)
ls()



