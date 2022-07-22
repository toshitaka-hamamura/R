#################################################################
###第3章スクリプト
###本スクリプトの動作確認環境は以下の通りである
###R3.0.2(64bit版)
###パッケージlavaan0.5-15
###パッケージsemPlot0.3.3
#################################################################

#パッケージンの読み込み
library(lavaan)
library(semPlot)

###第1節　パス解析
#逐次モデル
#データの読み込み
#x1：学歴，x2：年収, x3：職業威信，x4：評価
data1 <- read.csv("dat/marriage.csv")
head(data1)
#モデルの指定
model1 <- 'x3 ~ x1 + x2
           x4 ~ x2 + x3'
#分析の実行
fit1 <- sem(model1, data=data1)
#分析結果の確認
summary(fit1, standardized=T, fit.measures=T)
#standardizedSolution(fit01)

#パス図の描画(第一段階)：図3.2
semPaths(fit1, whatLabels ="stand",
layout="tree", style="lisrel", nCharNodes=0,
sizeMan=15, edge.label.cex=2.0)
#パス図の描画(第二段階)：図3.3
semPaths(fit1, whatLabels="stand",
layout="tree",style="lisrel",sizeMan=15,
edge.label.cex=2.0,rotation=2,reorder=F, 
manifests=c("x2","x1","x4","x3"),
nodeLabels=c("職業","学歴","評価","年収"))

#非逐次モデル
data2 <- read.csv("dat/fan.csv")
model2 <- ' x3 ~ x1 + x4
            x4 ~ x2 + x3'
fit2 <- sem(model2, data=data2)
#fit02_2 <- sem(model02, data=data02, fixed.x=F,std.lv=T)
summary(fit2, standardized=T, fit.measures=T)
#summary(fit02_2, standardized=T, fit.measures=T)

#パス図の描画：図3.4
semPaths(fit2, whatLabels="stand", 
style= "lisrel",layout="spring",
sizeMan=15,edge.label.cex=2.0,
nCharNodes=0,curve=1.5,mar=c(7,7,7,3),
manifest=c("x1","x3","x2","x4"),
nodeLabels=c("夫の父","夫","妻の父","妻"))


###第3節　多変量回帰
#偏相関係数の理解
data3_1 <- read.csv("dat/stats.csv", row.names=1)
round(cor(data3_1),3) #相関行列の計算
mean(data3_1$x); mean(data3_1$y); mean(data3_1$z) #各変数の平均
sd(data3_1$x)*46/47; sd(data3_1$y)*46/47; sd(data3_1$z)*46/47 #各変数の標準偏差
round((0.648-0.695*0.885)/(sqrt(1-0.695^2)*sqrt(1-0.885^2)), 3) #偏相関の計算

#多変量回帰分析モデルによる偏相関係数の求め方
model3_1 <- ' x ~ z
              y ~ z  '
fit3_1<-sem(model3_1,data=data3_1)
summary(fit3_1,standardized=T,fit.measures=T)

#パス図の描画：図3.5
semPaths(fit3_1,whatLabels="stand",
layout="tree",style="lisrel",
edge.color="black",shapeMan="rectangle",
sizeMan=15,sizeMan2=10,residScale=10,
edge.label.cex=2.5,nDigits=3)

#「期末試験データ」の分析
#x：睡眠時間 y：成績 z1：知能 z2：勉強時間
lower <- '1.000
          0.020 1.000
          0.004 0.791 1.000
         -0.423 0.722 0.341 1.000'
full <- getCov(lower, 
             names=c("x","y","z1","z2"))
model3_2 <- ' x ~ z1 + z2
              y ~ z1 + z2'
fit3_2<-sem(model3_2,sample.cov=full,
      sample.nobs=280, fixed.x=FALSE)
summary(fit3_2, standardized=T, fit.measures=T)
semPaths(fit3_2, whatLabels="stand",
layout="tree2",style="lisrel",
edge.color="black",sizeMan=10,
edge.label.cex=2.0,nDigits=3)


###第4節　MMIC/PLS
data4 <- read.csv("dat/semidata.csv")
#MIMICモデル
model4_MIMIC <- ' f2 ~ x1 + x2 + x3 + x4
                  f2 =~ x5 + x6 + x7'
fit4_M <- sem(model4_MIMIC,data=data4,fixed.x=F)
summary(fit4_M, standardized=T, fit.measures=T)

#パス図の描画：図3.6
semPaths(fit4_M, whatLabels="stand",
layout="tree",style="lisrel",
shapeLat="ellipse",sizeMan=9,sizeLat=10,
sizeLat2=7,rotation=2,edge.label.cex=1.6,
curve=2,curvePivot=F,residScale=15,
manifests=c("x4","x3","x2","x1","x7","x6","x5"))

#PLSモデル
model4_PLS <- ' f1 ~ x1 + x2 + x3 + x4
                 f2 =~ x5 + x6 + x7
                 f1 =~ f2
                 f1 ~~ 0*f1
                 f2~~f2'
fit4_P <- sem(model4_PLS, data=data4, fixed.x=F)
summary(fit4_P, standardized=T, fit.measures=T)

#パス図の描画：図3.7
semPaths(fit4_P, whatLabels="stand",
layout="tree",style="lisrel",
shapeLat="ellipse",optimizeLatRes=T, 
sizeMan=7,sizeLat=9,sizeLat2=7,
rotation=2,edge.label.cex=1.3,
curve=2,curvePivot=F,residScale=12,
manifests=c("x4","x3","x2","x1","x7","x6","x5"))

###第5節　高次・階層因子分析モデル
data5 <- read.csv("dat/intelligence.csv")
#2次因子分析モデル
model5_S<- '視覚f1 =~ x1 + x2 + x3
            言語f2 =~ x4 + x5 + x6
            速度f3 =~ x7 + x8 + x9
            知能 =~ 視覚f1 + 言語f2 + 速度f3'
fit5_S <- sem(model5_S, std.lv=TRUE, data=data5)
summary(fit5_S, fit.measures=T, standardized=T)

#パス図の描画：図3.8
semPaths(fit5_S,layout="tree",whatLabels="stand", 
edge.color="black",sizeMan=7, edge.label.cex=0.9,
shapeLat="ellipse",sizeLat=8, sizeLat2=5,
style="lisrel", optimizeLatRes=T, nCharNodes=0)

#階層因子分析モデル
model5_H <- '視覚f1 =~ x1 + x2 + x3
             言語f2 =~ x4 + x5 + x6
             速度f3 =~ x7 + x8 + x9
             F =~ x1 + x2 + x3 + x4 + x5 + x6 +x7 + x8 + x9
             F ~~ 0*視覚f1 + 0*言語f2 + 0*速度f3
             x3 ~~ a*x3; a>0'
fit5_H <- sem(model5_H, std.lv=T, data=data5)
summary(fit5_H,fit.measures=T,standardized=T)

#パス図の描画：図3.9
semPaths(fit5_H,layout="spring",
whatLabels="stand",sizeMan=5,
shapeLat="ellipse",sizeLat=8,sizeLat2=5,
exoCov=F,residuals=F,nCharNodes=0,
edge.label.cex=0.9,edge.color="black")


###第6節　1因子分析モデルによる信頼性
data6 <- read.csv("dat/extra.csv")
model6 <- ' 外向性 =~ x1 + x2 + x3 + x4 + x5 '
fit6 <- sem(model6, data=data6, std.lv=T)
summary(fit6, fit.measures=T) 

#パス図の描画：図3.10
semPaths(fit6,layout="tree",style="ram",
whatLabels="est",nCharNodes=0,sizeMan=10,
shapeLat="ellipse",sizeLat=15,sizeLat2=10,
edge.label.cex=1.5,edge.color="black")

##ω係数
a <- (0.99+1.17+0.77+1.01+0.69)^2 #因子負荷の和の2乗
e <- 1.68+1.21+1.23+1.11+1.31 #誤差分散の和
round(a/(a+e), 3) #ω係数の算出
##α係数
(var.mat <- var(data6)*(nrow(data6)+1)/nrow(data6)) #共分散行列
#各項目の分散の合計
sum.var <- var.mat[1,1]+var.mat[2,2]+var.mat[3,3]+var.mat[4,4]+var.mat[5,5]
#合計点の分散
all.var <- sum.var + 2*(var.mat[1,2]+var.mat[1,3]+var.mat[1,4]+var.mat[1,5]
 +var.mat[2,3]+var.mat[2,4]+var.mat[2,5]+var.mat[3,4]+var.mat[3,5]+var.mat[4,5])
round(5/4*(1-(sum.var/all.var)), 3) #α係数の算出

##以下を実行することによってもω係数とα係数を求めることができる
#詳細については第9章参照のこと
library(semTools)
reliability(fit6)

###第7節　MTMM
lower.MTMM <- '1.000
               0.464 1.000
               0.340 0.223 1.000
               0.574 0.241 0.330 1.000
               0.349 0.762 0.195 0.300 1.000
               0.175 0.019 0.646 0.390 0.119 1.000
               0.639 0.346 0.309 0.630 0.326 0.281 1.000
               0.399 0.788 0.166 0.251 0.791 0.052 0.420 1.000
               0.276 0.121 0.657 0.282 0.143 0.692 0.445 0.237 1.000'
full.MTMM <- getCov(lower.MTMM, 
         names=c("x11","x21","x31","x12","x22","x32","x13","x23","x33"))
model7 <- ' T1 =~ x11 + x12 + x13
            T2 =~ x21 + x22 + x23
            T3 =~ x31 + x32 + x33
            M1 =~ x11 + x21 + x31
            M2 =~ x12 + x22 + x32
            M3 =~ x13 + x23 + x33 
            T1 ~~ 0*M1 + 0*M2 + 0*M3
            T2 ~~ 0*M1 + 0*M2 + 0*M3
            T3 ~~ 0*M1 + 0*M2 + 0*M3
            M1 ~~ 0*M2 + 0*M3
            M2 ~~ 0*M3'
fit7 <- sem(model7, sample.cov=full.MTMM, sample.nobs=406,std.lv=T)
summary(fit7, fit.measures=T, standardized=T)

#パス図の描画：図3.11
semPaths(fit7,what="path",whatLabels="no",
style="lisrel",layout="tree",sizeMan=6,
curve=1.5,curvePivot=F,freeStyle=c("black",1),
fixedStyle=c("white",0))

###第8節　構成概念間の因果モデル
lower.cov <- '0.54
              0.99 2.28
              0.82 1.81 1.98
              0.73 1.27 0.91 6.89
              0.62 1.49 1.17 6.25 15.58
              0.79 1.55 1.04 5.84 5.84 10.76
              1.08 2.06 1.58 5.06 5.60 4.94 6.83
              0.85 1.81 1.57 5.75 9.39 4.73 4.98 11.38
              0.94 2.00 1.63 5.81 7.54 7.01 5.82 6.75 10.80'
full.cov <- getCov(lower.cov, 
         names=c("x1","x2","x3","x4","x5","x6","x7","x8","x9"))

model8 <- ' f1 =~ x1+x2+x3
            f2 =~ x4+a*x5+b*x6
            f3 =~ x7+a*x8+b*x9
            f2 ~ f1
            f3 ~ f1+f2
            x4 ~~  c*x4 + x7
            x5 ~~  d*x5 + x8
            x6 ~~  e*x6 + x9
            x7 ~~  c*x7
            x8 ~~  d*x8
            x9 ~~  e*x9 '
fit8 <- sem(model8, sample.cov=full.cov, sample.nobs=75)
summary(fit8,fit.measures=T,standardized=T)

#パス図の描画：図3.12
semPaths(fit8,layout="tree",whatLabels="est", 
style="lisrel",sizeMan=7,sizeLat=10,sizeLat2=8,
edge.color="black",curve=1,curvePivot=F,edge.label.cex=1.2)


###実践問題の解答
##実践問題1
lower.ex1  <- '
1.00
 .56 1.00
 .06  .05 1.00
 .16  .21  .32 1.00
 .01 -.04  .10 -.06 1.00
-.07 -.05  .16 -.07  .42 1.00
-.02 -.01  .14  .08  .18  .31 1.00
 .17  .30  .29  .40  .01  .13  .21 1.00
 .16  .21  .28  .19  .12  .27  .27  .50 1.00
 .05  .04  .08  .13  .07  .15  .25  .28  .24 1.00
 .10  .10  .09  .17  .02  .08  .08  .23  .18  .59 1.00
 .10  .17  .14  .17  .08  .17  .33  .32  .40  .55  .49 1.00
'
full.ex1 <- getCov(lower.ex1, 
names=c( "x01","x02","x03","x04","x05","x06","x07","x08","x09","x10","x11","x12"))

#モデルの指定（図3.13）
model.ex1 <- ' f1 =~  x01 + x02
               f2 =~  x03 + x04
               f3 =~  x05 + x06 + x07
               f4 =~  x08 + x09
               f5 =~  x10 + x11 + x12
               f4 ~ f1 + f2 + f5
               f5 ~ f3 + f4'
#分析の実行と結果の出力（図3.13）
fit.ex1 <- sem(model.ex1,
  sample.cov=full.ex1,sample.nobs=249)
summary(fit.ex1,standardized=T)

#パス図描画のためのスクリプトの例その1（図3.13）
semPaths(fit.ex1,layout="circle",what="path",
whatlabels="no",sizeLat=5, style="lisrel",
curve=1,residScale=6,edge.label.cex=0.7)
#パス図描画のためのスクリプトの例その2（図3.13）
semPaths(fit.ex1,layout="tree",
what="path", whatlabels="no",sizeLat=5,
style="lisrel",curvePivot=F, curve=1.5, rotation=2,
reorder=F, latents=c("f3","f2","f1","f5","f4"),
manifests=c("x07","x06","x05","x04","x03","x02","x01","x12","x11","x10","x09","x08"),
residScale=6,edge.label.cex=0.7)

##実践問題2
lower.ex2  <- '
1.000
0.610 1.000
0.422 0.297 1.000
0.268 0.419 0.660 1.000
0.466 0.223 0.400 0.228 1.000
0.221 0.458 0.241 0.412 0.585 1.00
'
full.ex2 <- getCov(lower.ex2, names=c( "x1","x2","x3","x4","x5","x6"))
#モデルの指定（図3.14）
model.ex2_1 <- ' f1 =~ x1 + x2
                 f2 =~ x3 + x4
                 f3 =~ x5 + x6
                 f2 ~ f1
                 f3 ~ f2'
#分析の実行と結果の出力（図3.14）
fit.ex2_1 <- sem(model.ex2_1,
  sample.cov=full.ex2, sample.nobs=100)
summary(fit.ex2_1,standardized=T)
#パス図描画のためのスクリプトの例（図3.14）
semPaths(fit.ex2_1,layout="tree",
whatLabels="stand",style="lisrel",
residScale=10)


#モデルの指定（図3.15）
model.ex2_2 <- ' f1 =~ x1 + x3 + x5
                 f2 =~ x2 + x4 + x6
                 f3 =~ a*x1 + a*x2
                 f4 =~ b*x3 + b*x4
                 f5 =~ c*x5 + c*x6
                 f4 ~ f3
                 f5 ~ f4
                 f3 ~~ 0*f1+0*f2
                 f1 ~~ 0*f2'
#分析の実行と結果の出力（図3.15）
fit.ex2_2 <- sem(model.ex2_2,
  sample.cov=full.ex2, sample.nobs=100)
summary(fit.ex2_2,standardized=T)
#パス図描画のためのスクリプトの例（図3.15）
semPaths(fit.ex2_2,layout="tree2",
whatLabels="stand",style="lisrel",
exoCov=F,reorder=F,
manifests=c("x1","x2","x5","x3","x4","x6"))
