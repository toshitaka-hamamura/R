#######################################
## 第1章 共分散構造分析前夜
## 動作確認環境 R 3.0.2
#######################################

## 多変量データ
# データの読み込み
X <- read.csv("dat/seminar.csv", header=T, row.names=1)
# 表1.1
head(X,n=10) #オブジェクトXの上から10行目までを出力する。引数nを省略した場合、上から6行目までが出力される。

# データ行列の行数および列数を調べる
dim(X)

X[1, ] #「佐藤さん」の調査の結果を出力する。
X[ ,3] # 変数「ペース」に対する各人の評価を出力する。
X[1,3] #「佐藤さん」の「ペース」への満足度「4」を出力する。

######################################
## 図による要約
exampledat <- read.csv(file="dat/testdata.csv")
# 図1.1 「国語」に関するヒストグラム
hist(exampledat[,1],xlim=c(10,80),ylim=c(0,60),ylab="度数",xlab="国語")

# 図1.4 「国語」と「数学」についての散布図
plot(exampledat[,1],exampledat[,3])

######################################
## データの代表値
#表1.4，表1.5
# 「テキスト」について最頻値を確認する
table(X[,1])
rev(sort(table(X[,1])))[1:3] 

mean(X[,1]) #「テキスト」の平均値を出力
colMeans(X) # 行列各列の平均値を出力
median(X[,1]) #「テキスト」の中央値を出力
summary(X)

######################################
## 分散・標準偏差・共分散
# 「テキスト」についての標本分散
(var(X[,1])*(nrow(X)-1))/nrow(X)
# 不偏分散
var(X[,1])
# 標本分散を元にした「テキスト」の標準偏差
sqrt((var(X[,1])*(nrow(X)-1))/nrow(X))
# 不偏分散を元にした標準偏差
sd(X[,1])

# 表1.8 多変量データの標本分散共分散行列
(var(X)*(nrow(X)-1))/nrow(X)
# 不偏分散共分散行列
var(X)

######################################
## データの標準化・相関係数
# 表1.9 標準化「セミナー評価データ」
scale(X)

# 表1.10 相関行列の出力
cor(X)

######################################
## 探索的因子分析1
# パッケージの読み込み
library(psych)
library(GPArotation)

# 表1.12(uはu2の値の平方根をとることで算出します)
Xfa_rotate<-fa(r=X, nfactors=2,
　　　　rotate="promax",fm="ml")
Xfa_rotate

## 探索的因子分析2
# 表1.11 初期解
fa(r=X,nfactors=2,rotate="",fm="ml")

# 表1.13 因子スコアの出力
Xfa_rotate$scores

## 探索的因子分析3
# 図1.8 スクリープロット
plot(eigen(cor(X))$values,type="b",ylab="固有値",xlab="")

# 表1.14 1因子解
fa(r=X,nfactors=1,n.obs=118,rotate="",fm="ml")
# 表1.15 3因子解
fa(r=X,nfactors=3,n.obs=118,rotate="promax",fm="ml")


######################################
## 練習問題解答
hist(X[,1],ylim=c(0,70),breaks=c(0:5))
hist(X[,2],ylim=c(0,70),breaks=c(0:5))
hist(X[,3],ylim=c(0,70),breaks=c(0:5))
hist(X[,4],ylim=c(0,70),breaks=c(0:5))
hist(X[,5],ylim=c(0,70),breaks=c(0:5))
hist(X[,6],ylim=c(0,70),breaks=c(0:5))
hist(X[,7],ylim=c(0,70),breaks=c(0:5))

##########
#「テキスト」10名分のデータ
X[,1][1:10]
#平均
(3+4+4+2+1+3+4+4+5+4)/10

##########
#標本分散
((3-3.4)^2+(4-3.4)^2+(4-3.4)^2+(2-3.4)^2+(1-3.4)^2
+(3-3.4)^2+(4-3.4)^2+(4-3.4)^2+(5-3.4)^2+(4-3.4)^2)/10
#標本標準偏差
sqrt(1.24)

##########
#「プレゼン」10名分のデータ
X[,2][1:10]
#「テキスト」と「プレゼン」の10名分データの標本共分散
((3-3.4)*(5-3.8)+(4-3.4)*(4-3.8)+(4-3.4)*(4-3.8)+(2-3.4)*(3-3.8)+(1-3.4)*(1-3.8)
+(3-3.4)*(4-3.8)+(4-3.4)*(4-3.8)+(4-3.4)*(4-3.8)+(5-3.4)*(5-3.8)+(4-3.4)*(4-3.8))/10

##########
#変数「テキスト」に対する標準化
(3-3.4)/1.114
(4-3.4)/1.114
(4-3.4)/1.114
(2-3.4)/1.114
(1-3.4)/1.114
(3-3.4)/1.114
(4-3.4)/1.114
(4-3.4)/1.114
(5-3.4)/1.114
(4-3.4)/1.114

#「テキスト」10名分の標準化データの平均と標本分散
((-0.36)+0.54+0.54+(-1.26)+(-2.15)+(-0.36)+0.54+0.54+1.44+0.54)/10

(((-0.36)-0.00)^2+
(0.54-0.00)^2+
(0.54-0.00)^2+
((-1.26)-0.00)^2+
((-2.15)-0.00)^2+
((-0.36)-0.00)^2+
(0.54-0.00)^2+
(0.54-0.00)^2+
(1.44-0.00)^2+
(0.54-0.00)^2)/10

##########
#「プレゼン」10名分データの標本分散
((5-3.8)^2+(4-3.8)^2+(4-3.8)^2+(3-3.8)^2+(1-3.8)^2+(4-3.8)^2+(4-3.8)^2+(4-3.8)^2+(5-3.8)^2+(4-3.8)^2)/10
#「プレゼン」10名分データの標本標準偏差
sqrt(((5-3.8)^2+(4-3.8)^2+(4-3.8)^2+(3-3.8)^2+(1-3.8)^2+(4-3.8)^2+(4-3.8)^2+(4-3.8)^2+(5-3.8)^2+(4-3.8)^2)/10)
#「プレゼン」10名分データの標準化
round((5-3.8)/1.078,2)
round((4-3.8)/1.078,2)
round((4-3.8)/1.078,2)
round((3-3.8)/1.078,2)
round((1-3.8)/1.078,2)
round((4-3.8)/1.078,2)
round((4-3.8)/1.078,2)
round((4-3.8)/1.078,2)
round((5-3.8)/1.078,2)
round((4-3.8)/1.078,2)

#「テキスト」と「プレゼン」の10名分データの相関係数
((-0.36*1.11)+
(0.54*0.19)+
(0.54*0.19)+
(-1.26*-0.74)+
(-2.15*-2.60)+
(-0.36*0.19)+
(0.54*0.19)+
(0.54*0.19)+
(1.44*1.11)+
(0.54*0.19))/10

