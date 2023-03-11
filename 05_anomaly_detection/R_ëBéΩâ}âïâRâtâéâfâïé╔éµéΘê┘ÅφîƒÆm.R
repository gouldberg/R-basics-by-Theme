## R　隠れマルコフモデルによる異常検知
## ----------------------------------------------------------------------------

setwd("C:/users/R_work")
library("RHmm")


## ----------------------------------------------------------------------------
##　基本チュートリアル
## ----------------------------------------------------------------------------
#　平均と分散の異なる2つの正規分布を用意する
set.seed(1)
size <- 100
mean.1 <- 0.1
var.1 <- 0.02
mean.2 <- -0.05
var.2 <- 0.09
x.1 <- rnorm(size, mean.1, sqrt(var.1))
x.2 <- rnorm(size, mean.2, sqrt(var.2))
x.3 <- rnorm(size, mean.1, sqrt(var.1))
x <- c(x.1, x.2, x.3)


#　観測変数が正規分布、潜在状態=2 の隠れマルコフモデル　パラメータの推定
hmm.fitted <- HMMFit(x, dis = "NORMAL", nStates = 2)


#　見事に、分布のパラメータを推定している
hmm.fitted$HMM


#　観測変数系列（データ）が与えられた条件付の下での潜在状態変数の確率(gamma)を計算
#　フォワードα・バックワードβ再帰による確率計算
fb <- forwardBackward(hmm.fitted, x)
fb


#　各データごとに状態2にいる条件付き確率 gamma を表示
color.state.1 <- rep("deepskyblue", size)
color.state.2 <- rep("firebrick1", size)
color.state <- c(color.state.1, color.state.2, color.state.1)
barplot(rep(1, 3 * size), col = color.state, border = color.state, space = 0, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
par(new = TRUE)
matplot(fb$Gamma[, 1], type = "l", main = "Probability being in State 2", ylab = "Probability", lwd = 3)
legend(x = "topright", "State2", fill = 1, bty = "n")


## ----------------------------------------------------------------------------
##　株価の時系列データで会社の状態を推定し異常時を確認する　隠れマルコフモデル　（ある状態が異常時であると判断し、その状態にいる確率をみる）
##
## 1.　会社の状態を表現する特徴量を作成する -->　会社の株価収益率
## 2.　会社の状態数を設定する　（アドホックだが3）
## 3.　特徴量を隠れマルコフでモデリング、潜在状態変数の確率を計算
## 4.　潜在状態の分布パラメータを確認し、状態1,2,3のうち、異常状態であるものを特定する
## 5.　異常状態にいる確率を時系列に沿って確認する
## ----------------------------------------------------------------------------
library(RFinanceYJ)

# 株価取得＆終値のみに変更
stock.price <- quoteStockTsData("2432.t")
stock.price <- quoteStockTsData("2432.t", since = "2012-01-04", date.end = "2012-10-31")
stock.price <- stock.price[, c("date", "close")]
stock.price$date <- as.Date(stock.price$date)

# 株価収益率に変換
stock.return <- with(stock.price, close[-1]/close[-length(close)] - 1)


#　隠れマルコフモデル
#　会社の状態は3状態をとる（3状態にしたのは完全にアドホック）
# 観測データである株価収益率は正規分布に従う
hmm.fitted <- HMMFit(stock.return, dis = "NORMAL", nStates = 3)
probabilities <- forwardBackward(hmm.fitted, stock.return)


#　潜在状態の分布パラメータを確認する -->　状態3がマイナス値なので、この状態3を会社の異常状態であると判断
hmm.fitted$HMM$distribution


# 観測変数が与えられた条件付の下での潜在状態（会社状態）の確率 gamma を取得
probability.anomaly <- probabilities$Gamma[, 3]


#　時系列に沿って株価、背景色で異常状態(="3")にいる確率を表示する
library(scales)
library(ggplot2)
library(reshape2)
df <- cbind(stock.price[-1, ], prob = probability.anomaly)
df <- as.xts(read.zoo(merge(data.frame(date = seq(min(df$date), max(df$date), 1)), df, all.x = TRUE)))
df <- as.data.frame(na.locf(as.xts(df)))
df <- cbind(date = as.Date(rownames(df)), df)
ggplot(data = df, aes(x = date, y = close)) + geom_rect(aes(fill = prob, xmin = date -
    0.5, xmax = date + 0.5, ymin = 0, ymax = max(close) + 100)) + geom_line(colour = "red",
    size = 3) + scale_x_date(expand = c(0, 0), labels = date_format("%y/%m/%d")) +
    scale_y_continuous(expand = c(0, 0))
