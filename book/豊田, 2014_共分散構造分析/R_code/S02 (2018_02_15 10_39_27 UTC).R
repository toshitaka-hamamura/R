###ライブラリーの読み込み
library(lavaan)
library(semTools)
library(semPlot)
library(GPArotation)
library(psych)

###データの読み込み
X <- read.csv("dat/seminar.csv",row.names=1)
#列名の変更
colnames(X) <- c("x1","x2","x3","x4","x5","x6","x7")

###探索的因子分析
unrotated <- efaUnrotate(data=X,nf=2)
rotated <- oblqRotate(object=unrotated,method="geomin")
summary(rotated)

###共分散行列の読み込み
lower <- '
 1.049 
 0.204 0.816 
 0.321 0.374 1.417 
 0.254 0.461 0.212  1.273 
 0.202 0.296 0.165  0.227 1.993 
 0.030 0.238 0.307  0.243 0.464 1.744 
-0.066 0.123 0.099 -0.045 0.371 0.154 0.844'

X.cov <- getCov(x=lower, 
   names=c("x1","x2","x3","x4","x5","x6","x7"))


###関数semによる分析
##図2.6のパス図
#モデルの表現
model1 <- 'f1 =~ x1 + x2 + x3 + x4
		  f2 =~ x5 + x6 + x7
		  f2 ~ f1'

#生データを利用
fit <- sem(model=model1, data=X,
           estimator="ML")
#共分散行列を利用
fit.c <- sem(model=model1,sample.cov=X.cov,
            sample.nobs=118,estimator="ML")

#分析結果の確認
summary(object=fit,fit.measure=TRUE)
show(object=fit)
coef(object=fit)
#非標準化推定値
parameterEstimates(object=fit,ci=T)
parameterEstimates(object=fit,ci=T,standardized=T)
#標準化推定値
standardizedSolution(object=fit)

#構成概念スコア
factor_score <- predict(object=fit)
plot(factor_score, xlab="講師の質", ylab="充実感", las=1,xlim=c(-1.0,0.6),ylim=c(-2.0,1.6));par(new=T)


###semPathsによるパス図の描画
semPaths(fit, layout = "tree", whatLabels  = "stand", 
nDigits=3, shapeMan="square", sizeMan =8, 
sizeLat =8, sizeLat2 =8, style = "lisrel",
residScale=12, curve=0.7, optimizeLatRes=T,edge.color="black")


##図2.7のパス図
#モデルの表現
model2 <- 'f1 =~ x1 + x2 + x3 + x4
		  f2 =~ x5 + x6 + x7
		  f2 ~ f1
		  x2 ~~ x4'

#母数の推定
fit2 <- sem(model=model2, data=X, estimator="ML")

#分析結果の確認
summary(object=fit2,fit.measure=TRUE)
standardizedSolution(object=fit2)
coef(object=fit2)
#非標準化推定値
parameterEstimates(object=fit2,ci=FALSE)
parameterEstimates(object=fit2,ci=FALSE,standardized=TRUE)
#標準化推定値
standardizedSolution(object=fit2)



######################################
###練習問題の共分散行列

###練習問題解答
#共分散行列の読み込み
lower_A <- '
1.850 
0.787 1.428 
0.363 0.488 1.423 
1.181 0.832 1.495 10.491 
1.220 1.170 0.912  1.453 15.210 
0.980 0.965 0.759  1.427  6.734 7.393'
A.cov <- getCov(lower_A, names=paste("x",1:6,sep=""))
A.cov

#モデルの表現
model3 <- 'f1 =~ x1 + x2
		  f2 =~ x3 + x4
           f3 =~ x5 + x6
           f3  ~ f1 + f2'

#母数の推定
fit.A <- sem(model=model3, sample.cov=A.cov,
               sample.nobs=1800,estimator="ML")

#分析結果の確認
summary(object=fit.A,fit.measure=T)
standardizedSolution(object=fit.A)

semPaths(fit.A, whatLabels="stand",optimizeLatRes=TRUE)

parameterEstimates(object=fit.A,ci=FALSE)
parameterEstimates(object=fit.A,ci=FALSE,standardized=TRUE)
standardizedSolution(object=fit.A)

#信頼区間の計算
0.379 - 1.96*0.05
0.379 + 1.96*0.05

