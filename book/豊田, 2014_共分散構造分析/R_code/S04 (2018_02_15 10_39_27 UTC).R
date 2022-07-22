## パッケージの読み込み
library(lavaan)
library(semTools)

## データの読み込み
semidata<-read.csv("dat/semidata.csv")
head(semidata)


### 第4.1節 ###
## 歪度の計算
skew(semidata[,1])

## 尖度の計算
kurtosis(semidata[,1])


### 第4.2節 ###
## 「セミナーデータ」のモデルの指定と実行
model1 <- ' f1 =~ x1 + x2 + x3 + x4 
            f2 =~ x5 + x6 +x7
            f2 ~ f1 '
fit1 <- sem(model1, data=semidata)
summary(fit1)

## パラメータテーブルの出力
parTable(fit1)

## lavaan()を用いる場合のモデルの指定と実行
model2 <- ' f1 =~ 1*x1 + x2 + x3 + x4 
            f2 =~ 1*x5 + x6 +x7
            f2 ~ f1 
            x1 ~~ x1
            x2 ~~ x2
            x3 ~~ x3
            x4 ~~ x4
            x5 ~~ x5
            x6 ~~ x6
            x7 ~~ x7
            f1 ~~ f1
            f2 ~~ f2 '
fit2 <- lavaan(model2, data=semidata)
summary(fit2)
parTable(fit2)

## 引数を利用してlavaan()を用いる場合の実行
fit3 <- lavaan(model1, data=semidata, 
               auto.var=TRUE, 
               auto.fix.first=TRUE)
summary(fit3)
parTable(fit3)


### 第4.4節 ###
## モデルの共分散行列
fitted(fit1)

## 残差行列
resid(fit1, type="raw")

## 母数推定値の共分散行列
vcov(fit1)


### 第4.5節 ###
## パラメータテーブルの出力
inspect(fit1, what="list")

## パラメータテーブルをLISRELの行列形式で出力
inspect(fit1, what="free")


### 第4.6節 ###
## 初期値の出力
inspect(fit1, what="start")

## 観測変数の共分散行列
inspect(fit1, what="sampstat")

## モデルの相関行列
inspect(fit1, what="cor.all")

## モデルの共分散行列（観測変数・潜在変数の両方）
inspect(fit1, what="cov.all")

## 決定係数の出力
inspect(fit1, what="rsquare")

## 決定係数の出力（summary()を用いて）
summary(fit1, rsquare=TRUE)


### 第4.8節 ###
## 間接効果のモデルの実行
model.ind <- ' x5 ~ c*x2
               x6 ~ a*x2
               x5 ~ b*x6
               ab := a*b
            total := c + (a*b)'
fit.ind <- sem(model.ind, data=semidata)
summary(fit.ind)

# 間接効果の式
med <- 'a*b'

# パス係数の指定
co1<-coef(fit.ind)[["a"]]
co2<-coef(fit.ind)[["b"]]

# 母数の漸近的共分散行列
AC <- vcov(fit.ind)[c(2,3),c(2,3)]

# 信頼区間の算出
monteCarloMed(expression=med, 
    coef1=co1, coef2=co2, ACM=AC,
    outputValues=FALSE, plot=TRUE)


### 第4.9節 ###
## 修正指標の出力
(MI <- modificationIndices(fit1))
# 修正指標が3以上のもののみ取り出す。
subset(MI, mi>3)
# 因子負荷に関する修正指標のみ取り出す。
subset(MI, op == "=~")

## ワルド検定
model.con <- ' f1 =~ con1*x1 + con1*x2 + con1*x3 + con1*x4 
               f2 =~ con2*x5 + con2*x6 + con2*x7
               f2 ~ f1 '
fit.con <- sem(model.con, std.lv=TRUE, data=semidata)
summary(fit.con, fit.measure=TRUE)

wald(fit.con,"con2-con1")


### 練習問題 ###
## 実践問題1
lower_A <- '
1.850 
0.787 1.428 
0.363 0.488 1.423 
1.181 0.832 1.495 10.491 
1.220 1.170 0.912  1.453 15.210 
0.980 0.965 0.759  1.427  6.734 7.393'
A.cov <- getCov(lower_A, 
         names=paste("x",1:6,sep=""))
A.cov


model3 <- 'f1 =~ x1 + x2
           f2 =~ x3 + x4
           f3 =~ x5 + x6
           f3 ~ f1 + f2'


fit.A<-sem(model=model3, sample.cov=A.cov,
           sample.nobs=200,estimator="ML")
summary(fit.A,fit.measure=T)
standardizedSolution(fit.A)

## 実践問題2（引数省略あり）
model3.lav1 <- 'f1 =~ x1 + x2
                f2 =~ x3 + x4
                f3 =~ x5 + x6
                f3 ~ f1 + f2'
fit.A.lav1 <- lavaan(model=model3.lav1, sample.cov=A.cov,
                     sample.nobs=200, estimator="ML",
                     auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE)
summary(fit.A.lav1,fit.measure=T)

## 実践問題2（引数省略なし）
model3.lav2 <- 'f1 =~ 1*x1 + x2
                f2 =~ 1*x3 + x4
                f3 =~ 1*x5 + x6
                f3 ~    f1 + f2
                x1 ~~ x1
                x2 ~~ x2
                x3 ~~ x3
                x4 ~~ x4
                x5 ~~ x5
                x6 ~~ x6
                f1 ~~ f1
                f2 ~~ f2
                f3 ~~ f3
                f1 ~~ f2'
fit.A.lav2 <- lavaan(model=model3.lav2, sample.cov=A.cov,
                     sample.nobs=200, estimator="ML")
summary(fit.A.lav2,fit.measure=T)

## 実践問題3
# ラベル付きのスクリプト
model3.con <- 'f1 =~ x1 + x2
               f2 =~ x3 + x4
               f3 =~ x5 + x6
               f3 ~ a*f1 + b*f2
　　　　　　　　　　　　　　　dif:=a-b'
fit.A.con<-sem(model=model3.con, sample.cov=A.cov,
               sample.nobs=200,estimator="ML")
summary(fit.A.con,fit.measure=T)
standardizedSolution(fit.A.con)
# ワルド検定の実行
wald(fit.A.con,"a-b")