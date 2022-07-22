library(lavaan)
testscore<-read.csv("dat/chapter11.csv")
#モデル設定
boot.model<-'
 f1 =~ eng1 + lit1 + mth1 + soc1
 f2 =~ eng2 + lit2 + mth2 + soc2
 f2 ~ f1; eng1 ~~ eng2; lit1 ~~ lit2
 mth1 ~~ mth2; soc1 ~~ soc2 '
#通常のモデルによる推定
boot.result.1<-sem(boot.model,
 data=testscore)
summary(boot.result.1,fit.measures=TRUE)

#ブートストラップ標準誤差の推定
set.seed(1234)
boot.result.2<-sem(boot.model,
 data=testscore, se="bootstrap",
 bootstrap = 1000)
summary(boot.result.2,fit.measures=TRUE)

#ブートストラップ信頼区間の推定(Efronのパーセンタイル法)
boot.result.3<-parameterEstimates(
 boot.result.2,boot.ci.type = "perc")
boot.result.3

#ブートストラップ信頼区間の推定(正規近似法)
boot.result.4<-parameterEstimates(
 boot.result.2,boot.ci.type = "norm")
boot.result.4

#ブートストラップ法による個々の推定値の算出
set.seed(1234)
boot.result.5<-bootstrapLavaan(
 boot.result.1)
summary(boot.result.5)

#修正されたブートストラップ法による適合度の評価
set.seed(1234)
boot.result.6<-bootstrapLavaan(
 boot.result.1,type="bollen.stine",
 FUN="fitMeasures")
summary(boot.result.6)