### 第7.1節
modelReg <- ' len ~ 1 + supp '

library(lavaan)
dataTooth <- read.csv('dat/tooth.csv')
fitReg <- sem(modelReg, dataTooth)
summary(fitReg, standardized=TRUE)

### 表7.1
### 母集団

lower <- '
  0.9983
  0.4901 1.0031
  0.4203 0.4235 1.0016
  0.4187 0.4180 0.3585 0.9960
  0.1496 0.1492 0.1286 0.1264 1.0008
  0.1461 0.1465 0.1275 0.1242 0.4929 1.0033
  0.1234 0.1238 0.1057 0.1060 0.4195 0.4217 1.0007
  0.1246 0.1230 0.1071 0.1050 0.4196 0.4226 0.3603 1.0062
'
popCov <- getCov(lower, names=paste("x",1:8,sep=""))

modelFA <- '
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
'
fitFA_Pop <- cfa(modelFA, sample.cov=popCov, sample.nobs=300000, std.lv=TRUE)
summary(fitFA_Pop, fit.measures=TRUE, standardized=TRUE)

### 潜在的連続変数のデータ
dataCont <- read.csv('dat/latentContinuous.csv')

### 潜在的連続変数を閾値で2値にカテゴリー化したデータ
dataCat2 <- read.csv('dat/cat2.csv')

### 通常（順序カテゴリーを無視して通常の因子分析）
fitCat2Normal <- cfa(modelFA, dataCat2, std.lv=TRUE)
summary(fitCat2Normal, standardized=TRUE)

### 連続（連続データに通常の因子分析）
fitCont <- cfa(modelFA, dataCont, std.lv=TRUE)
summary(fitCont, standardized=TRUE)

### 順序（順序カテゴリカル変数にカテゴリカル因子分析）
fitCat2 <- cfa(modelFA, dataCat2,
   std.lv=TRUE, ordered=paste("x", 1:8, sep=""))
summary(fitCat2, standardized=TRUE)


### 第7.2節
dataCat2 <- read.csv('dat/cat2.csv') # 本文でのデータ読み込み箇所
modelFA <- '                         # 本文でのモデル指定箇所
   f1 =~ x1 + x2 + x3 + x4
   f2 =~ x5 + x6 + x7 + x8
'
fitCat2 <- cfa(modelFA, dataCat2, std.lv=TRUE, 
   ordered=paste("x", 1:8, sep=""))
summary(fitCat2)


### 表7.3
### 異なる10個のデータに同一の分析を実行するが，
### 分析のスクリプト自体は表7.1と同一なので省略

### 図7.2
dataCat3 <- read.csv('dat/cat3.csv')
fitCat3 <- cfa(modelFA, dataCat3, std.lv=TRUE, 
   ordered=paste("x", 1:8, sep=""))

library(semPlot)
semPaths(fitCat3, style="lisrel", whatLabels="std",
  layout="tree", sizeLat=6, sizeLat2=4, sizeInt=3,
  thresholdSize=1.0, edge.label.cex=0.7)

### 表7.4
### 表7.3と同様の理由により省略

### 表7.5
### 表7.3と同様の理由により省略


### 第7.5節
library(semTools)
dataNN <- read.csv('dat/nonNormal.csv')

### 各変数の歪度と尖度
lapply(dataNN, skew)
lapply(dataNN, kurtosis)

### 多変量歪度と多変量尖度
mardiaSkew(dataNN)
mardiaKurtosis(dataNN)

### 表7.7
fitNN <- cfa(modelFA, dataNN, std.lv=TRUE,
   estimator="MLR")
summary(fitNN, fit.measures=TRUE, standardized=TRUE)

### 非正規分布の母集団
NNlower <- '
1.4951
0.4923 1.4907
0.4232 0.4177 1.3640
0.4177 0.4154 0.3603 1.3563
0.1459 0.1440 0.1219 0.1229 1.4833
0.1510 0.1498 0.1297 0.1248 0.4853 1.4941
0.1207 0.1241 0.1024 0.0992 0.4177 0.4220 1.3647
0.1279 0.1247 0.1076 0.1052 0.4180 0.4220 0.3584 1.3555
'

NNpopCov <- getCov(NNlower, names=paste("x",1:8,sep=""))
fitFA_NNpop <- cfa(modelFA, sample.cov=NNpopCov, sample.nobs=300000, std.lv=TRUE)
summary(fitFA_NNpop, fit.measures=TRUE, standardized=TRUE)



