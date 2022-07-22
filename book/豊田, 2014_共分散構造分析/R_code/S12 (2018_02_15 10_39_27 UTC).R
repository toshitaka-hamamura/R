library(lavaan)
height<-read.csv("dat/chapter12-1.csv")
#成長曲線モデルの基本形
growth.curve.1 <- '
 i =~ 1*h1 + 1*h2 + 1*h3 + 1*h4
 s =~ 0*h1 + 1*h2 + 2*h3 + 3*h4'
growth.fit.1 <- growth(growth.curve.1,
 data=height)
summary(growth.fit.1, fit.measures=TRUE)

#因子スコアの推定
growth.fscore.1 <- predict(growth.fit.1)
head(growth.fscore.1)

#予測変数のあるモデル
growth.curve.2 <- '
 i =~ 1*h1 + 1*h2 + 1*h3 + 1*h4
 s =~ 0*h1 + 1*h2 + 2*h3 + 3*h4
 # 予測変数からの回帰
 i ~ w1
 s ~ w1'
growth.fit.2 <- growth(growth.curve.2,
 data=height)
summary(growth.fit.2, fit.measures=TRUE)

#2次の成長曲線モデル
apartment<-read.csv("dat/chapter12-2.csv")
growth.curve.3 <- '
 i =~ 1*h1 + 1*h2 + 1*h3 + 1*h4
 s =~ 0*h1 + 1*h2 + 2*h3 + 3*h4
 #2次の項
 t =~ 0*h1 + 1*h2 + 4*h3 + 9*h4'
growth.fit.3 <- growth(growth.curve.3,
 data=apartment)
summary(growth.fit.3, fit.measures=TRUE)