######################################################################
# 共分散構造分析R編
# 第5章　平均共分散構造分析
######################################################################


#########################################################################
# 第5.1節　平均共分散構造分析とは

# 重回帰分析
# 変数名
# x1:テキスト, x2:プレゼン, x3:ペース, x4:講師対処, x5:満足度, x6:理解度, x7:目的一致

dat <- read.csv("dat/semidata.csv")
glm(x5~x2+x4+x6, data=dat)



# lavaanを利用した重回帰分析
library(lavaan); library(semPlot)

# 変数名
# x1:テキスト, x2:プレゼン, x3:ペース, x4:講師対処, x5:満足度, x6:理解度, x7:目的一致
# 重回帰モデル
model <- '
  x5 ~ 1 + x2 + x4 + x6
  x5 ~~ x5
'

fit <- lavaan(model, data=dat)
summary(fit)




#########################################################################
# 第5.2節　平均構造のあるパス解析
# library(lavaan)

# 観測変数のみの場合
# 変数名
# x1:テキスト, x2:プレゼン, x3:ペース, x4:講師対処, x5:満足度, x6:理解度, x7:目的一致

# パス解析モデル
model <- '
x6 ~ 1 + x2 + x4
x5 ~ 1 + x2 + x4 + x6
x5 ~~ x5; x6 ~~ x6
'

fit <- lavaan(model, data=dat)
summary(fit)

2.04 + 0.23*mean(dat$x2) + 0.11*mean(dat$x4)



# 構成概念間のパス解析
# 変数名
# x1:テキスト, x2:プレゼン, x3:ペース, x4:講師対処, x5:満足度, x6:理解度, x7:目的一致

# パス解析モデル
model <- '
f1 =~ x1 + x2 + x3 + 1*x4 
x1 + x2 + x3 + x4 ~ 0*1
x1 ~~ x1; x2 ~~ x2; x3 ~~ x3; x4 ~~ x4
f1 ~ 1
f1 ~~ f1

f2 =~ x5 + x6 + 1*x7
x5 + x6 + x7 ~ 0*1
x5 ~~ x5; x6 ~~ x6; x7 ~~ x7
f2 ~~ f2

f2 ~ 1 + f1
'

fit <- lavaan(model, data=dat)
summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")




#################################################
# 第5.3節　縦断データの因子分析1
# 使用データ

dat <- read.table("dat/IQtest.csv", sep=",", header=T, row.names=1)
dat <- as.matrix(dat)

IQ.cov <- dat[1:8,]
IQ.mean <- dat[9,]

rownames(IQ.cov) <- colnames(IQ.cov)




#################################################
# 第5.4節　縦断データの因子分析2

# 変数名
# V1:迷路（中1）, V2:立方体（中1）, V3:幾何図形（中1）, V4:異同弁別（中1）
# V5:迷路（高1）, V6:立方体（高1）, V7:幾何図形（高1）, V8:異同弁別（高1）

########################################################################
########################################################################
# モデル1：配置不変モデル
model <- '
f1 =~ V1 + V2 + V3 + V4
f1 ~ 0*1; 
f1 ~~ 1*f1

f2 =~ V5 + V6 + V7 + V8
f2 ~ 0*1; 
f2 ~~ 1*f2

f1 ~~ f2
V1 ~~ V5; V2 ~~ V6; V3 ~~ V7; V4 ~~ V8

V1 + V2 + V3 + V4 ~ 1
V5 + V6 + V7 + V8 ~ 1

V1 ~~ V1; V2 ~~ V2; V3 ~~ V3; V4 ~~ V4
V5 ~~ V5; V6 ~~ V6; V7 ~~ V7; V8 ~~ V8
'

fit <- lavaan(model, sample.cov=IQ.cov, sample.mean=IQ.mean, sample.nobs=1472)
summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")



########################################################################
########################################################################
# モデル2：弱測定不変モデル
model <- '
f1 =~ a1*V1 + a2*V2 + a3*V3 + a4*V4
f1 ~ 0*1; 
f1 ~~ 1*f1

f2 =~ a1*V5 + a2*V6 + a3*V7 + a4*V8
f2 ~ 0*1; 
f2 ~~ f2

f1 ~~ f2
V1 ~~ V5; V2 ~~ V6; V3 ~~ V7; V4 ~~ V8

V1 + V2 + V3 + V4 ~ 1
V5 + V6 + V7 + V8 ~ 1

V1 ~~ V1; V2 ~~ V2; V3 ~~ V3; V4 ~~ V4
V5 ~~ V5; V6 ~~ V6; V7 ~~ V7; V8 ~~ V8
'

fit <- lavaan(model, sample.cov=IQ.cov, sample.mean=IQ.mean, sample.nobs=1472)
summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")



########################################################################
########################################################################
# モデル3：強測定不変モデル
model <- '
f1 =~ a1*V1 + a2*V2 + a3*V3 + a4*V4
f1 ~ 0*1; 
f1 ~~ 1*f1

f2 =~ a1*V5 + a2*V6 + a3*V7 + a4*V8
f2 ~ 1; 
f2 ~~ f2

f1 ~~ f2
V1 ~~ V5; V2 ~~ V6; V3 ~~ V7; V4 ~~ V8

V1 + V5 ~ b1*1
V2 + V6 ~ b2*1
V3 + V7 ~ b3*1
V4 + V8 ~ b4*1

V1 ~~ V1; V2 ~~ V2; V3 ~~ V3; V4 ~~ V4
V5 ~~ V5; V6 ~~ V6; V7 ~~ V7; V8 ~~ V8
'

fit <- lavaan(model, sample.cov=IQ.cov, sample.mean=IQ.mean, sample.nobs=1472)
summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")




########################################################################
########################################################################
# モデル4：厳密な測定不変モデル
model <- '
f1 =~ a1*V1 + a2*V2 + a3*V3 + a4*V4
f1 ~ 0*1; 
f1 ~~ 1*f1

f2 =~ a1*V5 + a2*V6 + a3*V7 + a4*V8
f2 ~ 1; 
f2 ~~ f2

f1 ~~ f2
V1 ~~ V5; V2 ~~ V6; V3 ~~ V7; V4 ~~ V8

V1 + V5 ~ b1*1
V2 + V6 ~ b2*1
V3 + V7 ~ b3*1
V4 + V8 ~ b4*1

V1 ~~ c1*V1
V2 ~~ c2*V2
V3 ~~ c3*V3
V4 ~~ c4*V4
V5 ~~ c1*V5
V6 ~~ c2*V6
V7 ~~ c3*V7
V8 ~~ c4*V8
'

fit <- lavaan(model, sample.cov=IQ.cov, sample.mean=IQ.mean, sample.nobs=1472)
summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")




########################################################################
########################################################################
# モデル5：全母数が等しいモデル
model <- '
f1 =~ a1*V1 + a2*V2 + a3*V3 + a4*V4
f1 ~ 0*1; 
f1 ~~ 1*f1

f2 =~ a1*V5 + a2*V6 + a3*V7 + a4*V8
f2 ~ 0*1; 
f2 ~~ f2

f1 ~~ f2
V1 ~~ V5; V2 ~~ V6; V3 ~~ V7; V4 ~~ V8

V1 + V5 ~ b1*1
V2 + V6 ~ b2*1
V3 + V7 ~ b3*1
V4 + V8 ~ b4*1

V1 ~~ c1*V1
V2 ~~ c2*V2
V3 ~~ c3*V3
V4 ~~ c4*V4
V5 ~~ c1*V5
V6 ~~ c2*V6
V7 ~~ c3*V7
V8 ~~ c4*V8
'

fit <- lavaan(model, sample.cov=IQ.cov, sample.mean=IQ.mean, sample.nobs=1472)
summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")








#################################################
# 第5.5節　縦断データの因子分析3

# 変数名
# V1:迷路（中1）, V2:立方体（中1）, V3:幾何図形（中1）, V4:異同弁別（中1）
# V5:迷路（高1）, V6:立方体（高1）, V7:幾何図形（高1）, V8:異同弁別（高1）


# モデル比較
library(semTools)

model <- '
f1 =~ V1 + V2 + V3 + V4
f2 =~ V5 + V6 + V7 + V8
'

# 変数群の指定
time1 <- c("V1", "V2", "V3", "V4")
time2 <- c("V5", "V6", "V7", "V8")
constrainedVar <- list(time1, time2)

longInvariance(model, sample.cov=IQ.cov, sample.mean=IQ.mean, sample.nobs=1472,
               auto=1, constrainAuto=F, varList=constrainedVar, strict=T)



# 分析の実行(厳密な測定不変モデル)
model <- '
# 弱測定不変
f1 =~ a1*V1 + a2*V2 + a3*V3 + a4*V4
f1 ~ 0*1; f1 ~~ 1*f1

f2 =~ a1*V5 + a2*V6 + a3*V7 + a4*V8
f2 ~ 1; f2 ~~ f2

f1 ~~ f2
V1 ~~ V5; V2 ~~ V6; V3 ~~ V7; V4 ~~ V8

# 強測定不変
V1 + V5 ~ b1*1
V2 + V6 ~ b2*1
V3 + V7 ~ b3*1
V4 + V8 ~ b4*1

# 厳密な測定不変
V1 ~~ c1*V1
V2 ~~ c2*V2
V3 ~~ c3*V3
V4 ~~ c4*V4
V5 ~~ c1*V5
V6 ~~ c2*V6
V7 ~~ c3*V7
V8 ~~ c4*V8
'

fit <- lavaan(model, sample.cov=IQ.cov, sample.mean=IQ.mean, sample.nobs=1472)
summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")


























