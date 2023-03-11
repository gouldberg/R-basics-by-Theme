setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\00_basics\\10_time_series_basics")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------


hakusan <- as.ts(read.csv(".\\data\\HAKUSAN.txt", header = T, sep = "\t"))

sunspot <- as.ts(read.csv(".\\data\\Sunspot.txt", header = T, sep = "\t"))

maxtemp <- as.ts(read.csv(".\\data\\maxtemp.txt", header = T, sep = "\t"))

blsfood <- as.ts(read.csv(".\\data\\BLSALLFOOD.txt", header = T, sep = "\t"))

whard <- as.ts(read.csv(".\\data\\WHARD.txt", header = T, sep = "\t"))

nikkei225 <- as.ts(read.csv(".\\data\\nikkei225.txt", header = T, sep = "\t"))

mye1f <- as.ts(read.csv(".\\data\\MYE1F.txt", header = T, sep = "\t"))

haibara <- as.ts(read.csv(".\\data\\haibara.txt", header = T, sep = "\t"))

linx <- as.ts(read.csv(".\\data\\Lynx.txt", header = T, sep = "\t"))

temperature <- as.ts(read.csv(".\\data\\Temperature.txt", header = T, sep = "\t"))

# average in 2 years
rainfall <- as.ts(read.csv(".\\data\\rainfall.txt", header = T, sep = "\t") / 2)



# ----------
head(hakusan)

head(sunspot)

head(maxtemp)

head(blsfood)

head(whard)

head(nikkei225)

head(mye1f)

head(haibara)

head(linx)

head(temperature)

head(rainfall)




# ------------------------------------------------------------------------------
# plot all data:  various characteristics
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(2,3))


# stationary, periodic
plot(hakusan[,"YawRate"], main = "�D���̕����p���x", ylab = "")


# positive, asymmetric (upper, lower, right, left), Periodic
plot(sunspot, main = "���z���_���f�[�^", ylab = "")


# trend (long-term Periodic),  stationary around trend
plot(maxtemp, main = "�����̓��ō��C���f�[�^", ylab = "")


# yearly periodic, trend
plot(blsfood, main = "�A�����J�̐H�i�Y�Ƃɏ]������J���Ґl��", ylab = "")



# yearly periodic, positive, trend + variance increasing
plot(whard, main = "�H�Ɛ��i�̉����荂", ylab = "")



# No trend, variance non-stationary, covariance non-stationary, locally stationary
plot(mye1f, main = " �n�k�f�[�^(��������)", ylab = "")




# ----------
# trend, variance change, increasing in down trending
plot(nikkei225, main = "���o225", ylab = "")



# mean stationary, periodic, asymmetric (left, right)
plot(linx, main = "�J�i�_�I�I���}�l�R������", ylab = "")



# discrete, mean probability non-stationary
plot(rainfall, main = "�����~���ʃf�[�^", ylab = "")




# ----------
# multi-variate, negative cross-correlation, outliers, missing values
plot(haibara[,c("�n������", "�C��")], main = "�Y���n�����ʃf�[�^")



# ----------
# multi-variate
# Input:  Rudder
# Output: Rolling and Pitching
plot(hakusan[,c("Rolling", "Pitching", "Rudder")], main = "�D���f�[�^ �c�h�� ���h�� �Ǌp")




# ------------------------------------------------------------------------------
# Data Transformation for WHARD
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(3,2))



# ----------
plot(whard, main = "original WHARD")

plot(log(whard), main = "log")

plot(diff(log(whard)), main = "log and diff")




# ----------
plot(whard, main = "original WHARD")

plot(diff(log(whard)), main = "log and diff")

plot(diff(log(whard), 12), main = "log and diff12 (seasonalily removed)")




# ----------
plot(whard, main = "original WHARD")

plot(whard / stats::lag(whard, -1), main = "ratio to previous")

plot(whard / stats::lag(whard, -12), main = "ratio to previous year")




# ------------------------------------------------------------------------------
# Data Transformation for nikkei225
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(3,1))

plot(nikkei225, main = "original nikkei225")

plot(log(nikkei225), main = "log")

plot(diff(log(nikkei225)), main = "log and diff")


