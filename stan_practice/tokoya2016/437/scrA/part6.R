#第6章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#6.2 蕎麦の選好問題
x <- 220 ;  n <- 400           #正反応数とデータ数 
aaa<-Bi01(x,n, fi=NA)          #1つの2項分布に関する推測
print(aaa,3,pr1=0.5,cr1=243,cr2=1.4)
plot(density(aaa$theta))       #事後分布　図6.1左
hist(aaa$xaste,breaks=100)     #図6.1右
plot(density(aaa$Odds))        #図6.2

#6.3 相談相手問題
x <- c(26,40,8,2,23,8)
bbb<-Mu01(x, fi=NA)                   #カテゴリ数がkの比率の推測
print(bbb,3)
#連言命題が正しい確率
printIJ(bbb$U2,3,IJ=rbind(c(2,1),c(2,3),c(2,4),c(2,5),c(2,6)))
printIJ(bbb$U2,3,IJ=rbind(c(2,3),c(2,4),c(2,5),c(2,6)))
printIJ(bbb$U2,3,IJ=rbind(c(1,4),c(2,4),c(3,4),c(5,4),c(6,4)))
printIJ(bbb$U2,3,IJ=rbind(c(1,4),c(2,4),c(5,4),c(6,4)))
printIJ(bbb$U2,3,IJ=rbind(c(1,4),c(2,4),c(3,4),c(5,4)))
printIJ(bbb$U2,3,IJ=rbind(c(2,5),c(5,3),c(5,4),c(5,6)))

#6.4.1 法案賛否問題 1
x <- c(71,42); n <- c(120,125) #正反応数とデータ数 
ccc<-Bi02(x,n, fi=NA)                 #2つの2項分布に関する推測
print(ccc,3,cr1=0.1,cr2=1.5,cr3=2.0)

#6.4.3 デート経験問題
x <- c(38,51,66,79); n <- c(101,99,100,102) #正反応数とデータ数 
ddd<-Bi03(x,n, fi=NA)                       #g個の2項分布に関する推測
print(ddd,3)
#連言命題が正しい確率
printIJ(ddd$U2,3,IJ=rbind(c(2,1),c(3,2),c(4,3)))
printIJ(ddd$U2,3,IJ=rbind(c(2,1),c(3,2),c(4,2)))
printIJ(ddd$U2,3,IJ=rbind(c(2,1),c(3,1),c(4,1)))

#6.5.1 法案賛否問題2
x <- matrix(c(55,16,
              14,35),2,2,T)           #反応数 
apply(x,1,"sum");apply(x,2,"sum")     #周辺度数
eee<-Mu02(x, fi=NA)                  #対応ある2×2のクロス表の分析
print(eee,3)
plot(density(eee$V))          #連関係数の事後分布 図6.5

#6.5.2 トランプ
x <- matrix(c(20,20,
               6, 6),2,2,T)           #反応数 
apply(x,1,"sum");apply(x,2,"sum")     #周辺度数
out2<-Mu02(x, fi=eee$fit)     #対応ある2×2のクロス表の分析
print(out2,3)
plot(density(out2$V))          #連関係数の事後分布 図6.5

#6.5.4 ワイン選択問題
x <- matrix(c(19, 12,  6,
               8,  8,  4,
              15, 19, 18),3,3,T) ;       #反応数 
apply(x,1,"sum");apply(x,2,"sum");sum(x) #周辺度数
fff<-Mu02(x, fi=eee$fit)                #対応あるa×bのクロス表の分析
print(fff,3)
#連言命題が正しい確率(関数なし)
Up<-fff$Up; Um<-fff$Um
round(mean(Up[,1,1]*Up[,3,3]*Um[,3,1]*Um[,1,3]),3)
round(mean(Up[,1,1]*Up[,3,3]*Um[,3,1]),3)
round(mean(Up[,1,1]*Up[,3,3]),3)

### 第6章章末問題

## 1. コイン投げの確率 
kaijyo <-function(x){cumprod(x:1)[x]}　　#階乗計算を行う関数
(kaijyo(5)/(kaijyo(3)*kaijyo(2)))*0.5^3*(1-0.5)^2

## 2. ジャンケンの確率 
(kaijyo(5)/(kaijyo(2)*kaijyo(2)*kaijyo(1)))*0.3^2*0.3^2*0.4

## 3. コインの比率
x <- 55; n <- 100; aaa<-Bi01(x,n, fi=aaa$fit); print(aaa,3,pr1=0.5)

## 4. 高校生の相談相手 
x <- c(30,12,4,20,22,8); bbb<-Mu01(x, fi=bbb$fit); print(bbb,3)
printIJ(bbb$U2,3,IJ=rbind(c(1,2),c(1,3),c(1,4),c(1,5),c(1,6)))
printIJ(bbb$U2,3,IJ=rbind(c(1,2),c(1,3),c(1,4),c(1,6)))
printIJ(bbb$U2,3,IJ=rbind(c(1,2),c(1,3),c(1,6)))

## 4. 治療法の効果の比較
x <- c(90,78); n <- c(115,117); 
ccc<-Bi02(x,n, fi=ccc$fit); 
print(ccc,3,cr1=0.1)

## 5. 広告効果
x <- matrix(c(33,18,
              84,23),2,2,T)
apply(x,1,"sum");apply(x,2,"sum")
out<-Mu02(x, fi=eee$fit); print(out,3)

## 5. ２回来店した客が注文したワイン
x <- matrix(c(13,  6, 21,
               7, 17,  7,
              13,  6, 13),3,3,T)
apply(x,1,"sum");apply(x,2,"sum");sum(x)
out<-Mu02(x, fi=fff$fit); print(out,3)
