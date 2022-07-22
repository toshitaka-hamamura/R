#####共分散構造分析R編-第18章
source('ADF3var2Analysis.R')

#18.1節　1因子共通変動モデル
branddat1<-read.csv('dat/brand1.csv')
#図18.1, 図18.2　ヒストグラム
{
	par(mfrow=c(1,2))
	hist(branddat1$brandA,main="",ylab="",br=seq(60,150,5),xlab="ブランドAの値段")
	axis(1, at=seq(60,150,10))
	hist(branddat1$brandB,main="",ylab="",br=seq(60,150,5),xlab="ブランドBの値段")
	axis(1, at=seq(60,150,10))
	par(mfrow=c(1,1))
}
#図18.3 散布図
plot(branddat1$brandA,branddat1$brandB,xlab="ブランドAの値段",ylab="ブランドBの値段")
#モデル比較
Res1<-ADF3var2(v1=scale(branddat1$brandA),v2=scale(branddat1$brandB))#正常な解が得られないモデルに関して警告がでます。
Res1#表18.1
#推定結果(表18.2)
round(Res1[[7]]$推定値,3)



###18.2節　2因子共通変動モデル
geyserdat<-read.csv("dat/geyser.csv")
#図18.5, 18.6　ヒストグラム
{
	par(mfrow=c(1,2))
	hist(geyserdat$eruptions,xaxt="n",xlab="噴出時間",ylim=c(0,80),main="",ylab="")
	axis(1, at=seq(1.5,5.5,0.5))
	hist(geyserdat$waiting,xaxt="n",xlab="待ち時間",ylim=c(0,60),main="",ylab="")
	axis(1, at=seq(40,100,10))
	par(mfrow=c(1,1))
}
#図18.4　散布図(軸なし)
plot(geyserdat$eruptions,geyserdat$waiting,xlab="噴出時間",ylab="待ち時間")
#モデル比較
Res2<-ADF3var2(scale(geyserdat$eruptions),scale(geyserdat$waiting))#正常な解が得られないモデルに関して警告がでます。
Res2#表18.3
#推定結果(表18.4)
round(Res2[[11]]$推定値,3)
#図18.4　散布図(軸あり)
{
	plot(scale(geyserdat$eruptions), scale(geyserdat$waiting),xlim=c(-2,2),ylim=c(-2,2),xlab="噴出時間",ylab="待ち時間")
	abline(a=0,b=0.5423554/0.4907025)
	abline(a=0,b=2.2727273/-2.2727273,lty="dashed")
}




###18.3節　双方向モデル
branddat2<-read.csv("dat/brand2.csv")
#図18.7, 18.8　ヒストグラム
{
	par(mfrow=c(1,2))
	hist(branddat2$brandC,main="",xlab="ブランドCの値段",ylab="",br=seq(85,150,5),ylim=c(0,250))
	axis(1, at=seq(85,150,5))
	hist(branddat2$brandD,main="",xlab="ブランドDの値段",ylab="",br=seq(85,150,5),ylim=c(0,250))
	axis(1, at=seq(85,150,5))
	par(mfrow=c(1,1))
}
#図18.9　散布図
plot(branddat2$brandC,branddat2$brandD,xlab="ブランドCの値段",ylab="ブランドDの値段")

#モデル比較
Res3<-ADF3var2(scale(branddat2$brandC),scale(branddat2$brandD))#正常な解が得られないモデルに関して警告がでます。
Res3#表18.5
#推定結果(表18.6)
round(Res3[[15]]$推定値,3)




###18.4節　因子＋単回帰モデル, 18.5節 分析の実際
galtondat<-read.csv("dat/galton.csv")
#図18.10, 18.11 ヒストグラム
{
	par(mfrow=c(1,2))
	hist(galtondat$parent,main="",xlab="両親の平均身長",ylab="",br=seq(61,74,1),ylim=c(0,250))
	hist(galtondat$child,main="",xlab="子の身長",ylab="",br=seq(61,74,1),ylim=c(0,250))
	par(mfrow=c(1,1))
}
#モデル比較
parent.Stand<-scale(galtondat$parent)
child.Stand <-scale(galtondat$child)
Res4<-ADF3var2(v1=parent.Stand,v2=child.Stand)#正常な解が得られないモデルに関して警告がでます。
Res4#表18.8, 表18.10
round(Res4[[16]]$推定値,3)#表18.9
modelA1<-Res4[[1]]
modelB2<-Res4[[6]]
modelC1<-Res4[[9]]
modelD1<-Res4[[13]]
modelE1<-Res4[[16]]
round(modelA1$推定値,3)#表18.11
round(modelB2$推定値,3)#表18.12
round(modelC1$推定値,3)#表18.13
round(modelD1$推定値,3)#表18.14
round(modelE1$推定値,3)#表18.15


