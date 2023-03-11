
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# auto-correlation
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(2,3))


acf(hakusan[,"YawRate"], main = "�D���̕����p���x", ylab = "", lag.max = 50)


acf(sunspot, main = "���z���_���f�[�^", ylab = "", lag.max = 50)


acf(maxtemp, main = "�����̓��ō��C���f�[�^", ylab = "", lag.max = 50)


acf(blsfood, main = "�A�����J�̐H�i�Y�Ƃɏ]������J���Ґl��", ylab = "", lag.max = 50)


acf(whard, main = "�H�Ɛ��i�̉����荂", ylab = "", lag.max = 50)


acf(mye1f, main = " �n�k�f�[�^(��������)", ylab = "", lag.max = 50)




# ----------
acf(nikkei225, main = "���o225", ylab = "", lag.max = 50)


acf(linx, main = "�J�i�_�I�I���}�l�R������", ylab = "", lag.max = 50)


acf(rainfall, main = "�����~���ʃf�[�^", ylab = "", lag.max = 50)




# ----------
# acf(haibara[,c("�n������", "�C��")], lag.max = 50)

car::scatterplotMatrix(formula = ~ �n������ + �C��, data = haibara, smooth = FALSE)




# ------------------------------------------------------------------------------
# autocorrelation and cross correlation
# ------------------------------------------------------------------------------

acf(hakusan[,c("Rolling", "Pitching", "Rudder")], lag.max = 50)


# -->
# ACF of Rolling and Rudder are very similar


car::scatterplotMatrix(formula = ~ Rolling + Pitching + Rudder, data = hakusan, smooth = FALSE)


