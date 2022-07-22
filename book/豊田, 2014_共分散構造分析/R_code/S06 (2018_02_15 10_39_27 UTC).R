# 共分散構造分析R編
# 第6章　多母集団分析

# 大学生活充実感尺度
# 変数名
# F1:在籍課程, F2:在籍年数, F3:サークル, F4:年齢, F5:性別
# V41:今やりたいことがある
# V42:今の自分に自信がある
# V43:大学に行くのが楽しい
# V44:毎日の生活にハリがある
# V45:努力していることがある
# V46:自分は価値のある大学生活を送っていると思う
# V47:大学生としての自分が好きである
# V48:自分に誇りがある
# V49:人生について真面目に考えている


library(semTools); library(semPlot)


###########################################################################
# 第6.2節　等値制約
dat <- read.csv("dat/collegelife.csv")


###########################################################################
###########################################################################
# モデル1：配置不変モデル
model <- '
# 基本モデル
f1 =~ V1 + V2 + V3 + V4
V1 +  V2  +  V3  +  V4 ~ 1
V1 ~~ V1; V2 ~~ V2; V3 ~~ V3; V4 ~~ V4

# 因子平均と因子の分散を1に固定する
f1 ~ c(fm11, fm21)*1
fm11 == 0; fm21 == 0

f1 ~~ c(fv11, fv21)*f1
fv11 == 1; fv21 == 1
'

fit <- lavaan(model, data=dat, group="C")

summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")



###########################################################################
###########################################################################
# モデル2：弱測定不変モデル
model <- '
# 基本モデル
f1 =~ V1 + V2 + V3 + V4
V1 +  V2  +  V3  +  V4 ~ 1
V1 ~~ V1; V2 ~~ V2; V3 ~~ V3; V4 ~~ V4

# 因子平均と因子の分散を1に固定する
f1 ~ c(fm11, fm21)*1
fm11 == 0; fm21 == 0

f1 ~~ c(fv11, fv21)*f1
fv11 == 1
'

fit <- lavaan(model, data=dat, group="C", 
              group.equal=c("loadings"))

summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")



###########################################################################
###########################################################################
# モデル3：強測定不変モデル
model <- '
# 基本モデル
f1 =~ V1 + V2 + V3 + V4
V1 +  V2  +  V3  +  V4 ~ 1
V1 ~~ V1; V2 ~~ V2; V3 ~~ V3; V4 ~~ V4

# 因子平均と因子の分散を1に固定する
f1 ~ c(fm11, fm21)*1
fm11 == 0

f1 ~~ c(fv11, fv21)*f1
fv11 == 1
'

fit <- lavaan(model, data=dat, group="C", 
              group.equal=c("loadings", "intercepts"))

summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")



###########################################################################
###########################################################################
# モデル4：厳密な測定不変モデル
model <- '
# 基本モデル
f1 =~ V1 + V2 + V3 + V4
V1 +  V2  +  V3  +  V4 ~ 1
V1 ~~ V1; V2 ~~ V2; V3 ~~ V3; V4 ~~ V4

# 因子平均と因子の分散を1に固定する
f1 ~ c(fm11, fm21)*1
fm11 == 0

f1 ~~ c(fv11, fv21)*f1
fv11 == 1
'

fit <- lavaan(model, data=dat, group="C", 
              group.equal=c("loadings", "intercepts", "residuals"))

summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")



###########################################################################
###########################################################################
# モデル5：全母数が等しいモデル
model <- '
# 基本モデル
f1 =~ V1 + V2 + V3 + V4
V1 +  V2  +  V3  +  V4 ~ 1
V1 ~~ V1; V2 ~~ V2; V3 ~~ V3; V4 ~~ V4

# 因子の分散を1に固定する
f1 ~~ c(fv11, fv21)*f1
fv11 == 1
'

fit <- lavaan(model, data=dat, group="C", 
              group.equal=c("loadings", "intercepts", "residuals", "means"))

summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")






###########################################################################
# 第6.3節　1因子モデルの分析例

# モデル比較
model <- '
f1 =~ V1 + V2 + V3 + V4
'

out <- measurementInvariance(model, data=dat, group="C", strict=T, quiet=T)
compareFit(out)


# 母数の推定
model <- '
# 基本モデル
f1 =~ V1 + V2 + V3 + V4
V1 +  V2  +  V3  +  V4 ~ 1
V1 ~~ V1; V2 ~~ V2; V3 ~~ V3; V4 ~~ V4

# 母集団1の因子の分散を1に固定する
f1 ~~ c(fv11, fv21)*f1
fv11 == 1
'

fit <- lavaan(model, data=dat, group="C", 
              group.equal=c("loadings","intercepts","residuals","means"))

summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")




##########################################################
# 第6.4節　2因子モデルの分析例

# モデル比較
model <- '
f1 =~ V1 + V2 + V3 + V4
f2 =~ V5 + V6 + V7 + V8
f1 ~ f2
'

out <- measurementInvariance(model, data=dat, group="C", strict=T, quiet=T)
compareFit(out)


# 母数の推定
model <- '
# 基本モデル
f1 =~ V1 + V2 + V3 + V4
V1 + V2 + V3 + V4 ~ 1
V1 ~~ V1; V2 ~~ V2; V3 ~~ V3; V4 ~~ V4
f1 ~~ c(fv11, fv21)*f1

f2 =~ V5 + V6 + V7 + V8
V5 + V6 + V7 + V8 ~ 1
V5 ~~ V5; V6 ~~ V6; V7 ~~ V7; V8 ~~ V8
f2 ~ c(fm12, fm22)*1
f2 ~~ c(fv12, fv22)*f2

f1 ~ 1 + f2

# 因子平均と因子の分散を1に固定する
fm12 == 0
fv11 == 1; fv12 == 1; fv22 == 1
'

fit <- lavaan(model, data=dat, group="C", 
              group.equal=c("loadings","intercepts"))

summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")




##############################################################
# 第6.5節　共分散行列からの分析
# 語彙学習データに関する統制群と実験群の比較

# 統制群
varnames<- c("V1","V2","V3","V4")
lower1  <- '
37.626
24.933 34.680
26.639 24.236 32.013
23.649 27.760 23.565 33.443
'
full1.cov <- getCov(lower1, names=varnames)
means1<-c(18.381, 20.229, 20.400, 21.343)

# 実験群
lower2  <- '
50.084
42.373 49.872
40.760 36.094 51.237
37.343 40.396 39.890 53.641
'
full2.cov <- getCov(lower2, names=varnames)
means2<-c(20.556, 21.241, 25.667, 25.870)


# データのリストを作成
N<-list(統制群=105, 実験群=108)
full<-list(統制群=full1.cov, 実験群=full2.cov)
means<-list(統制群=means1, 実験群=means2)


# 母数推定
model <- '
# 基本モデル
f1 =~ V1 + V2
V1 ~ c1*1; V2 ~ c2*1
V1 ~~ V1; V2 ~~ V2
f1 ~ c(fm11, fm21)*1
f1 ~~ 1*f1

f2 =~ V3 + V4
V3 ~ c1*1; V4 ~ c2*1
V3 ~~ V3; V4 ~~ V4
f2 ~~ c(fv12, fv22)*f2

f2 ~ 1 + c(a1,a1)*f1

# 因子平均と因子の分散を1に固定する
fm11 == 0
fv12 == 1

V2 ~~ V4
'

fit <- lavaan(model, sample.cov=full, sample.mean=means, sample.nobs=N, 
              group.equal=c("loadings","intercepts"))

summary(fit, standardized=T, fit.measure=T)
semPaths(fit, optimizeLatRes=T, whatLabels="est")




