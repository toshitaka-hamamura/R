
### 第8.3節
library(lavaan)
library(semTools)
dataEx <- read.csv("dat/extra.csv")
modelEx <- '
   Extra =~ x1 + x2 + x3 + x4 + x5
'
fitEx <- cfa(modelEx, dataEx, std.lv=TRUE)

### 信頼性係数の推定
reliability(fitEx)

### 2次因子分析
dataIntel <- read.csv('dat/intelligence.csv')
model2ndFA <- '
   視覚f1 =~ x1 + x2 + x3
   言語f2 =~ x4 + x5 + x6
   速度f3 =~ x7 + x8 + x9
   知能   =~ 視覚f1 + 言語f2 + 速度f3
'
fit2ndFA <- cfa(model2ndFA, dataIntel,
               std.lv=TRUE)

### 第8.4節
### 同族テストモデル
modelCon <- '
 Extra =~ a1*x1 + a2*x2 + a3*x3 + a4*x4 + a5*x5
    x1 ~~ Ve1 * x1
    x2 ~~ Ve2 * x2
    x3 ~~ Ve3 * x3
    x4 ~~ Ve4 * x4
    x5 ~~ Ve5 * x5
 rho := ( (a1+a2+a3+a4+a5)^2 / 
         ((a1+a2+a3+a4+a5)^2 + (Ve1+Ve2+Ve3+Ve4+Ve5)) )
'

fitCon <- cfa(modelCon, dataEx, std.lv=TRUE)
summary(fitCon, fit.measures=TRUE, standardized=TRUE)

### タウ等価テストモデル
modelTau <- '
 Extra =~ a*x1 + a*x2 + a*x3 + a*x4 + a*x5
    x1 ~~ Ve1 * x1
    x2 ~~ Ve2 * x2
    x3 ~~ Ve3 * x3
    x4 ~~ Ve4 * x4
    x5 ~~ Ve5 * x5
 rho := ( (a+a+a+a+a)^2 / 
         ((a+a+a+a+a)^2 + (Ve1+Ve2+Ve3+Ve4+Ve5)) )
'

fitTau <- cfa(modelTau, dataEx, std.lv=TRUE)
summary(fitTau, fit.measures=TRUE, standardized=TRUE)

### 平行テストモデル
modelPara <- '
 Extra =~ a*x1 + a*x2 + a*x3 + a*x4 + a*x5
    x1 ~~ Ve * x1
    x2 ~~ Ve * x2
    x3 ~~ Ve * x3
    x4 ~~ Ve * x4
    x5 ~~ Ve * x5
 rho := ( (a+a+a+a+a)^2 / 
         ((a+a+a+a+a)^2 + (Ve+Ve+Ve+Ve+Ve)) )
'

fitPara <- cfa(modelPara, dataEx, std.lv=TRUE)
summary(fitPara, fit.measures=TRUE, standardized=TRUE)


### 第8.5節
### カテゴリカル因子分析による項目反応理論の母数推定
modelFAtoIRT <- '
  f =~ a1*u1+a2*u2+a3*u3+a4*u4+a5*u5

  u1 | c1*t1
  u2 | c2*t1
  u3 | c3*t1
  u4 | c4*t1
  u5 | c5*t1

  alpha1:=a1/(1.0-(a1^2))^0.5; beta1:=c1/a1
  alpha2:=a2/(1.0-(a2^2))^0.5; beta2:=c2/a2
  alpha3:=a3/(1.0-(a3^2))^0.5; beta3:=c3/a3
  alpha4:=a4/(1.0-(a4^2))^0.5; beta4:=c4/a4
  alpha5:=a5/(1.0-(a5^2))^0.5; beta5:=c5/a5
'

dataIRT <- read.csv('dat/irt5.csv")
fitFAtoIRT <- cfa(modelFAtoIRT, dataIRT, std.lv=TRUE,
 ordered=c("u1","u2","u3","u4","u5"))
summary(fitFAtoIRT, standardized=TRUE)

### 図8.1
library(semPlot)
semPaths(fitFAtoIRT,whatLabels="std",
 thresholdSize=1.0,sizeInt=3)

