library(semTools)
int.data.0<-read.csv("dat/chapter13.csv")
#積の変数作成
int.data.1<-indProd(int.data.0,
 var1=c("x4","x5","x6"),
 var2=c("x7","x8","x9"))
head(int.data.1)

#交互作用モデルの作成
int.model<-'
 f1 =~ x4 + x5 + x6; f2 =~ x7 + x8 + x9
 f12 =~ x4.x7 + x5.x8 + x6.x9
 f3 =~ x1 + x2 + x3; f3 ~ f1 + f2 + f12
 f12 ~~0*f1; f12 ~~ 0*f2
 x1 ~ 0*1; x4 ~ 0*1; x7 ~ 0*1
 x4.x7 ~ 0*1; f1 ~ NA*1; f2 ~ NA*1
 f3 ~ NA*1; f12 ~ NA*1'
fit.int<-sem(int.model, data=int.data.1,
 meanstructure=TRUE)
summary(fit.int,fit.measures=TRUE)

#調整変数の値ごとの切片，傾きの変化
fit.probe <- probe2WayMC(fit.int,
 c("f1", "f2", "f12"), "f3", "f1",
 c(1, 2, 3, 4))
plotProbe(fit.probe, xlim=c(1, 4))