
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# raw periodogram
# ------------------------------------------------------------------------------

library(astsa)


graphics.off()

par(mfcol = c(2,3))


mvspec(hakusan[,"YawRate"], main = "�D���̕����p���x", ylab = "", log = "yes", type = "h")


mvspec(sunspot, main = "���z���_���f�[�^", ylab = "", log = "yes", type = "h")


mvspec(maxtemp, main = "�����̓��ō��C���f�[�^", ylab = "", log = "yes", type = "h")


mvspec(blsfood, main = "�A�����J�̐H�i�Y�Ƃɏ]������J���Ґl��", ylab = "", log = "yes", type = "h")


mvspec(whard, main = "�H�Ɛ��i�̉����荂", ylab = "", log = "yes", type = "h")


mvspec(mye1f, main = " �n�k�f�[�^(��������)", ylab = "", log = "yes", type = "h")




# ----------
mvspec(nikkei225, main = "���o225", ylab = "", log = "yes", type = "h")


mvspec(linx, main = "�J�i�_�I�I���}�l�R������", ylab = "", log = "yes", type = "h")


mvspec(rainfall, main = "�����~���ʃf�[�^", ylab = "", log = "yes", type = "h")




# ----------
# mvspec(haibara[,c("�n������", "�C��")], log = "yes", type = "h")




# ----------
graphics.off()

par(mfcol = c(2,2))

mvspec(hakusan[,c("Rolling")], log = "yes", type = "h")

mvspec(hakusan[,c("Pitching")], log = "yes", type = "h")

mvspec(hakusan[,c("Rudder")], log = "yes", type = "h")




# ------------------------------------------------------------------------------
# smoothed spetral
# ------------------------------------------------------------------------------

( ker <- kernel("modified.daniell", c(9,9)) )

plot(ker)




# ----------
graphics.off()

par(mfcol = c(2,3))


mvspec(hakusan[,"YawRate"], main = "�D���̕����p���x", ylab = "", log = "yes", type = "h")
mvspec(hakusan[,"YawRate"], main = "�D���̕����p���x", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(sunspot, main = "���z���_���f�[�^", ylab = "", log = "yes", type = "h")
mvspec(sunspot, main = "���z���_���f�[�^", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(maxtemp, main = "�����̓��ō��C���f�[�^", ylab = "", log = "yes", type = "h")
mvspec(maxtemp, main = "�����̓��ō��C���f�[�^", ylab = "", log = "yes", type = "l", kernel = ker)



# ----------
mvspec(blsfood, main = "�A�����J�̐H�i�Y�Ƃɏ]������J���Ґl��", ylab = "", log = "yes", type = "h")
mvspec(blsfood, main = "�A�����J�̐H�i�Y�Ƃɏ]������J���Ґl��", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(whard, main = "�H�Ɛ��i�̉����荂", ylab = "", log = "yes", type = "h")
mvspec(whard, main = "�H�Ɛ��i�̉����荂", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(mye1f, main = " �n�k�f�[�^(��������)", ylab = "", log = "yes", type = "h")
mvspec(mye1f, main = " �n�k�f�[�^(��������)", ylab = "", log = "yes", type = "l", kernel = ker)




# ----------
mvspec(nikkei225, main = "���o225", ylab = "", log = "yes", type = "h")
mvspec(nikkei225, main = "���o225", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(linx, main = "�J�i�_�I�I���}�l�R������", ylab = "", log = "yes", type = "h")
mvspec(linx, main = "�J�i�_�I�I���}�l�R������", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(rainfall, main = "�����~���ʃf�[�^", ylab = "", log = "yes", type = "h")
mvspec(rainfall, main = "�����~���ʃf�[�^", ylab = "", log = "yes", type = "l", kernel = ker)




# ----------
# mvspec(haibara[,c("�n������", "�C��")], log = "yes", type = "h")




# ----------
graphics.off()

par(mfcol = c(2,3))

mvspec(hakusan[,c("Rolling")], log = "yes", type = "h")
mvspec(hakusan[,c("Rolling")], log = "yes", type = "l", kernel = ker)

mvspec(hakusan[,c("Pitching")], log = "yes", type = "h")
mvspec(hakusan[,c("Pitching")], log = "yes", type = "l", kernel = ker)

mvspec(hakusan[,c("Rudder")], log = "yes", type = "h")
mvspec(hakusan[,c("Rudder")], log = "yes", type = "l", kernel = ker)