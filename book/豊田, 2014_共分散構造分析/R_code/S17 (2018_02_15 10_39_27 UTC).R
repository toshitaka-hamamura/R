#####共分散構造分析R編-第17章

###17.3節 3次積率単回帰モデルの推定
library(moments)#歪度を計算するために必要です
galtondat<-read.csv("dat/galton.csv")
#図17.3, 17.4 ヒストグラム
{
	par(mfrow=c(1,2))
	hist(galtondat$parent,main="",xlab="両親の平均身長",ylab="",br=seq(61,74,1),ylim=c(0,250))
	hist(galtondat$child,main="",xlab="子の身長",ylab="",br=seq(61,74,1),ylim=c(0,250))
	par(mfrow=c(1,1))
}
skewness(galtondat$parent)
skewness(galtondat$child)
#表17.1 
source('ADF3regAnalysis.R')
Res1<-ADF3reg(v1=galtondat$parent,v2=galtondat$child)
round(Res1[[1]]$推定値,3)




###17.4節 分析の実際1
Res1#表17.3(表17.2含む)
Optmodel<-Res1[[1]]#最適モデル（もっとも適合の良かったモデルの行番号を[[ ]]に入れる。）
round(Optmodel$推定値,3)
round(Optmodel$標準化係数,3)
#誤差の分布(図17.5)
{
	hist(galtondat$child-( 0.647001*galtondat$parent+ 23.908138),br=seq(-8,8,1),main="",xlab="誤差", ylab="", ylim=c(0,250))
	axis(side=1,at=seq(-8,8,1))
}




###17.5節 分析の実際2
#基本情報(表17.6)
round(Optmodel$基本情報,3)
#通常回帰(表17.7)
round(Optmodel$通常回帰,3)
#重み行列
round(Optmodel$重み行列,3)#行列全体
round(diag(Optmodel$重み行列),3)#対角要素のみ(表17.8)
#気温と消費電力データの分析
tempelecdat<-read.csv('dat/tempelec.csv')
#図17.6, 17.7 ヒストグラム
{
	par(mfrow=c(1,2))
	hist(tempelecdat$temp,main="",xlab="平均気温(℃)",ylab="", ylim=c(0,15))
	hist(tempelecdat$elec,main="",xlab="消費電力（キロワット時/日)",ylab="",br=seq(10,110,10), ylim=c(0,20))
	axis(side=1, at=seq(10,110,10))
	par(mfrow=c(1,1))
}
skewness(tempelecdat$temp)
skewness(tempelecdat$elec)
#モデル比較(表17.9)
Res2<-ADF3reg(tempelecdat$temp,tempelecdat$elec)
Res2
#推定値(表17.10)
round(Res2[[3]]$推定値,3)
#散布図+回帰直線(図17.8)
{
	plot(tempelecdat[,1],tempelecdat[,2], main="",xlab= "平均気温(℃)", ylab= "消費電力（キロワット時/日)")
	abline(64.624,-1.947)
}