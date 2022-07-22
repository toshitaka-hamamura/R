#################################################
## 第19章 共分散構造によるモデルの特定
# OpenMx スクリプト
# 動作確認環境：R 3.0.2
#################################################
#パッケージのインストール（初回のみ）
source('http://openmx.psyc.virginia.edu/getOpenMx.R')
#パッケージの読み込み
library(OpenMx)

#################################################
# 19.2 直積モデル

#データの読み込み
X <- read.csv("dat/Cudeck1988.csv")
round(cor(X),3)

#共分散構造によるモデル特定
direct <- mxModel(model="direct",
  mxMatrix(type="Diag" , nrow=12, ncol=12, 
           free=T, lbound=0.0, name="D"),
  mxMatrix(type="Stand", nrow=3, ncol=3, 
           free=T, values=0.3, name="O"),
  mxMatrix(type="Stand", nrow=2, ncol=2, 
           free=T, values=0.3, name="M"),
  mxMatrix(type="Stand", nrow=2, ncol=2, 
           free=T, values=0.3, name="T"),
  mxMatrix(type="Diag" , nrow=12, ncol=12, 
           free=T, lbound=0.0, name="U"),
  mxAlgebra(expression= D %*%(O %x% M %x% T + U)%*% D,
            name="Sig"),
  mxMLObjective("Sig", dimnames=names(X)),
  mxData(cov(X),type="cov",numObs=2163)
)

#分析の実行
results<-mxRun(model=direct)

#結果の確認
summary(results) 
mxEval(list(O,T,M,diag(U)),model=results)


#################################################
# 19.3 PARAFACモデル
#データの読み込み
X <- read.csv("dat/MTMM186_raw.csv")
round(cor(X),3)

#共分散構造によるモデル特定
parafac <- mxModel(model="parafac",
 mxMatrix(type="Full", 4, 2,
          free=T,values=0.5,name="A"),
 mxMatrix(type="Full", 6, 2,
  free=  c(F,F,F,F,T  ,F,F,T  ,T  ,F,F,T), 
  values=c(1,0,0,1,.5,0,0,.5,.5,0,0,.5), 
  byrow=TRUE,name="V"),
 mxMatrix(type="Iden", 3, 3,
          free=F, name="I"),
 mxMatrix(type="Diag", 12, 12,
          free=T,values=0.5,name="U"),
 mxAlgebra(expression=
 (I %x% A) %*% V %*% t(V) %*% t(I %x% A)+U,
 name="Sig"),
 mxMLObjective("Sig", dimnames=names(X)),
 mxData(cor(X),type="cov", numObs=nrow(X))
)

#分析の実行
results<-mxRun(model=parafac)

#結果の確認
summary(results)
mxEval(list(A,V,diag(U)),model=results)

#################################################
# 19.4 イプサティブ2因子
#データの読み込み
X <- read.csv("dat/ipsative.csv")
round(cor(X),3)
round(cov(X),3) 
nov <- 8; nof <- 2; mov <- nov-1;
Gps<-(diag(rep(1,nov))-matrix(nov^(-1),nov,nov))[1:mov,]
ipsative08 <- mxModel("ipsative08",
   mxMatrix("Full", mov, nov, values=Gps,byrow=T,free=F, name="G"),
   mxMatrix("Full", nov, nof, values=0.0, 
      free=c(T,F,T,F,T,F,T,F,F,T,F,T,F,T,F,T),
      labels=c("a",NA,"b",NA,"c",NA,"d",NA,NA,"a",NA,"b",NA,"c",NA,"d"),
      byrow=TRUE,name="A"),
   mxMatrix("Stand", nof, nof, values=0.1,  free=T,name="P"),
   mxMatrix("Diag",  nov, nov, values=0.1,   free=T,
      labels=c("e1","e2","e3","e4","e1","e2","e3","e4"),
      name="U"),
   mxAlgebra(G %*% (A %*% P %*% t(A) + U) %*% t(G), name="Sig"),
   mxMLObjective("Sig", dimnames = names(X)),
   mxData(cov(X), type="cov", numObs=nrow(X))
)
results<-mxRun(ipsative08)
summary(results) 
mxEval(list(A,P,diag(U)) ,results)

#################################################
# 19.5 多変量ACEモデル
#データの読み込み
XM <- read.csv("dat/twinACEMZ104_raw.csv")
XD <- read.csv("dat/twinACEDZ104_raw.csv")
round(cor(XM),3); round(cor(XD),3);
nov<-4;
twinACEModel <- mxModel("twinACE",
   mxModel("ACE",
      mxAlgebra(expression=a %*% t(a),name="A"),
      mxAlgebra(expression=c %*% t(c),name="C"),
      mxAlgebra(expression=e %*% t(e),name="E"),
      mxAlgebra(expression=rbind (cbind(A + C + E , A + C),
         cbind(A + C , A + C + E)),name="expCovMZ"),
      mxAlgebra(expression=rbind (cbind(A + C + E , 0.5 %x% A + C),
         cbind(0.5 %x% A + C , A + C + E)),name="expCovDZ"),
      mxMatrix(type="Lower",nrow=nov,ncol=nov,free=T,values=0.6,name="a"),
      mxMatrix(type="Lower",nrow=nov,ncol=nov,free=T,values=0.5,name="c"),
      mxMatrix(type="Lower",nrow=nov,ncol=nov,free=T,values=0.4,name="e")
   ),
   mxModel("MZ",
      mxData(observed=cov(XM),type="cov", numObs=nrow(XM)),
      mxMLObjective("ACE.expCovMZ", dimnames = names(XM))
   ),
   mxModel("DZ",
      mxData(observed=cov(XD),type="cov", numObs=nrow(XD)),
      mxMLObjective("ACE.expCovDZ", dimnames = names(XD))
   ),
   mxAlgebra(expression=MZ.objective+DZ.objective, name="minus2loglikelihood"),
   mxAlgebraObjective("minus2loglikelihood")
)
results<-mxRun(twinACEModel)
summary(results) 
mxEval(list(ACE.A,ACE.C,ACE.E) ,results)



#################################################
#19.6 グラフィカルモデリング
library(lavaan)

#データの読み込み
lower <- '
 1.000
-0.221  1.000
 0.467 -0.119  1.000
 0.353 -0.247  0.465  1.000
-0.178 -0.052  0.205  0.031  1.000'
X.cor <- getCov(x=lower, names=paste0("x",1:5))
round(X.cor,2)


###制約なしモデル
gmodel00 <- mxModel(model="gmodel",
   mxMatrix(type="Symm" , nrow=5, ncol=5,
      free=c(T,
             T,T,
             T,T,T,
             T,T,T,T,
             T,T,T,T,T),
      values=c(1.51,
               0.23,  1.10,
              -0.68, -0.11,  1.65,
              -0.17,  0.24, -0.54,  1.37,
               0.43,  0.11, -0.45,  0.05,  1.17),
      labels=c("s11",
               "s21","s22",
               "s31","s32","s33",
               "s41","s42","s43","s44",
               "s51","s52","s53","s54","s55"),byrow=T,name="G"),
   mxMatrix("Iden" , 5, 5, free=F, name="I"),
   mxAlgebra(expression= solve(G), name="Sig"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[1,1]", name="c1"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[2,2]", name="c2"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[3,3]", name="c3"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[4,4]", name="c4"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[5,5]", name="c5"),
   mxAlgebra(expression= - solve(sqrt(I*G)) %*% G %*% solve(sqrt(I*G)), name="P"),
   mxMLObjective("Sig", dimnames = colnames(X.cor)),
   mxData(X.cor, type="cov", numObs=50)
)

results00<-mxRun(model=gmodel00)
summary(results00) 
matres <- mxEval(list(G,Sig,P) ,results00)
round(matres[[1]],3)
round(matres[[2]],3)
round(matres[[3]],3)


###s54を0に固定
gmodel01 <- mxModel(model="gmodel",
   mxMatrix(type="Symm" , nrow=5, ncol=5,
      free=c(T,
             T,T,
             T,T,T,
             T,T,T,T,
             T,T,T,F,T),
      values=c(1.51,
               0.23,  1.10,
              -0.68, -0.11,  1.65,
              -0.17,  0.24, -0.54,  1.37,
               0.43,  0.11, -0.45,  0.00,  1.17),
      labels=c("s11",
               "s21","s22",
               "s31","s32","s33",
               "s41","s42","s43","s44",
               "s51","s52","s53","s54","s55"),byrow=T,name="G"),
   mxMatrix("Iden" , 5, 5, free=F, name="I"),
   mxAlgebra(expression= solve(G), name="Sig"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[1,1]", name="c1"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[2,2]", name="c2"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[3,3]", name="c3"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[4,4]", name="c4"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[5,5]", name="c5"),
   mxAlgebra(expression= - solve(sqrt(I*G)) %*% G %*% solve(sqrt(I*G)), name="P"),
   mxMLObjective("Sig", dimnames = colnames(X.cor)),
   mxData(X.cor, type="cov", numObs=50)
)

results01<-mxRun(model=gmodel01)
summary(results01) 
matres01 <- mxEval(list(G,Sig,P) ,results01)
round(matres01[[1]],3)
round(matres01[[2]],3)
round(matres01[[3]],3)


###s32,s54を0に固定
gmodel02 <- mxModel(model="gmodel",
   mxMatrix(type="Symm" , nrow=5, ncol=5,
      free=c(T,
             T,T,
             T,F,T,
             T,T,T,T,
             T,T,T,F,T),
      values=c(1.51,
               0.23,  1.10,
              -0.68,  0.00,  1.65,
              -0.17,  0.24, -0.54,  1.37,
               0.43,  0.11, -0.45,  0.00,  1.17),
      labels=c("s11",
               "s21","s22",
               "s31","s32","s33",
               "s41","s42","s43","s44",
               "s51","s52","s53","s54","s55"),byrow=T,name="G"),
   mxMatrix("Iden" , 5, 5, free=F, name="I"),
   mxAlgebra(expression= solve(G), name="Sig"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[1,1]", name="c1"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[2,2]", name="c2"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[3,3]", name="c3"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[4,4]", name="c4"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[5,5]", name="c5"),
   mxAlgebra(expression= - solve(sqrt(I*G)) %*% G %*% solve(sqrt(I*G)), name="P"),
   mxMLObjective("Sig", dimnames = colnames(X.cor)),
   mxData(X.cor, type="cov", numObs=50)
)

results02<-mxRun(model=gmodel02)
summary(results02) 
matres02 <- mxEval(list(G,Sig,P) ,results02)
round(matres02[[1]],3)
round(matres02[[2]],3)
round(matres02[[3]],3)


###s52,s32,s54を0に固定
gmodel03 <- mxModel(model="gmodel",
   mxMatrix(type="Symm" , nrow=5, ncol=5,
      free=c(T,
             T,T,
             T,F,T,
             T,T,T,T,
             T,F,T,F,T),
      values=c(1.51,
               0.23,  1.10,
              -0.68,  0.00,  1.65,
              -0.17,  0.24, -0.54,  1.37,
               0.43,  0.00, -0.45,  0.00,  1.17),
      labels=c("s11",
               "s21","s22",
               "s31","s32","s33",
               "s41","s42","s43","s44",
               "s51","s52","s53","s54","s55"),byrow=T,name="G"),
   mxMatrix("Iden" , 5, 5, free=F, name="I"),
   mxAlgebra(expression= solve(G), name="Sig"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[1,1]", name="c1"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[2,2]", name="c2"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[3,3]", name="c3"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[4,4]", name="c4"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[5,5]", name="c5"),
   mxAlgebra(expression= - solve(sqrt(I*G)) %*% G %*% solve(sqrt(I*G)), name="P"),
   mxMLObjective("Sig", dimnames = colnames(X.cor)),
   mxData(X.cor, type="cov", numObs=50)
)

results03<-mxRun(model=gmodel03)
summary(results03) 
matres03 <- mxEval(list(G,Sig,P) ,results03)
round(matres03[[1]],3)
round(matres03[[2]],3)
round(matres03[[3]],3)


###s21,s52,s32,s54を0に固定
gmodel04 <- mxModel(model="gmodel",
   mxMatrix(type="Symm" , nrow=5, ncol=5,
      free=c(T,
             F,T,
             T,F,T,
             T,T,T,T,
             T,F,T,F,T),
      values=c(1.51,
               0.00,  1.10,
              -0.68,  0.00,  1.65,
              -0.17,  0.24, -0.54,  1.37,
               0.43,  0.00, -0.45,  0.00,  1.17),
      labels=c("s11",
               "s21","s22",
               "s31","s32","s33",
               "s41","s42","s43","s44",
               "s51","s52","s53","s54","s55"),byrow=T,name="G"),
   mxMatrix("Iden" , 5, 5, free=F, name="I"),
   mxAlgebra(expression= solve(G), name="Sig"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[1,1]", name="c1"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[2,2]", name="c2"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[3,3]", name="c3"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[4,4]", name="c4"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[5,5]", name="c5"),
   mxAlgebra(expression= - solve(sqrt(I*G)) %*% G %*% solve(sqrt(I*G)), name="P"),
   mxMLObjective("Sig", dimnames = colnames(X.cor)),
   mxData(X.cor, type="cov", numObs=50)
)

results04<-mxRun(model=gmodel04)
summary(results04) 
matres04 <- mxEval(list(G,Sig,P) ,results04)
round(matres04[[1]],3)
round(matres04[[2]],3)
round(matres04[[3]],3)


###s41,s21,s52,s32,s54を0に固定
gmodel05 <- mxModel(model="gmodel",
   mxMatrix(type="Symm" , nrow=5, ncol=5,
      free=c(T,
             F,T,
             T,F,T,
             F,T,T,T,
             T,F,T,F,T),
      values=c(1.51,
               0.00,  1.10,
              -0.68,  0.00,  1.65,
               0.00,  0.24, -0.54,  1.37,
               0.43,  0.00, -0.45,  0.00,  1.17),
      labels=c("s11",
               "s21","s22",
               "s31","s32","s33",
               "s41","s42","s43","s44",
               "s51","s52","s53","s54","s55"),byrow=T,name="G"),
   mxMatrix("Iden" , 5, 5, free=F, name="I"),
   mxAlgebra(expression= solve(G), name="Sig"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[1,1]", name="c1"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[2,2]", name="c2"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[3,3]", name="c3"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[4,4]", name="c4"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[5,5]", name="c5"),
   mxAlgebra(expression= - solve(sqrt(I*G)) %*% G %*% solve(sqrt(I*G)), name="P"),
   mxMLObjective("Sig", dimnames = colnames(X.cor)),
   mxData(X.cor, type="cov", numObs=50)
)

results05<-mxRun(model=gmodel05)
summary(results05) 
matres05 <- mxEval(list(G,Sig,P) ,results05)
round(matres05[[1]],3)
round(matres05[[2]],3)
round(matres05[[3]],3)


###s42,s41,s21,s52,s32,s54を0に固定
gmodel06 <- mxModel(model="gmodel",
   mxMatrix(type="Symm" , nrow=5, ncol=5,
      free=c(T,
             F,T,
             T,F,T,
             F,F,T,T,
             T,F,T,F,T),
      values=c(1.51,
               0.00,  1.10,
              -0.68,  0.00,  1.65,
               0.00,  0.00, -0.54,  1.37,
               0.43,  0.00, -0.45,  0.00,  1.17),
      labels=c("s11",
               "s21","s22",
               "s31","s32","s33",
               "s41","s42","s43","s44",
               "s51","s52","s53","s54","s55"),byrow=T,name="G"),
   mxMatrix("Iden" , 5, 5, free=F, name="I"),
   mxAlgebra(expression= solve(G), name="Sig"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[1,1]", name="c1"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[2,2]", name="c2"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[3,3]", name="c3"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[4,4]", name="c4"),
   mxMatrix("Full", 1, 1, free=FALSE, values=1, labels="Sig[5,5]", name="c5"),
   mxAlgebra(expression= - solve(sqrt(I*G)) %*% G %*% solve(sqrt(I*G)), name="P"),
   mxMLObjective("Sig", dimnames = colnames(X.cor)),
   mxData(X.cor, type="cov", numObs=50)
)

results06<-mxRun(model=gmodel06)
summary(results06) 
matres06 <- mxEval(list(G,Sig,P) ,results06)
round(matres06[[1]],3)
round(matres06[[2]],3)
round(matres06[[3]],3)


###分析結果(適合度)のまとめ
est_res <- data.frame(
ChiSq = round(c(
summary(results00)$Chi,
summary(results01)$Chi,
summary(results02)$Chi,
summary(results03)$Chi,
summary(results04)$Chi,
summary(results05)$Chi,
summary(results06)$Chi),3),
df = c(
summary(results00)$degreesOfFreedom,
summary(results01)$degreesOfFreedom,
summary(results02)$degreesOfFreedom,
summary(results03)$degreesOfFreedom,
summary(results04)$degreesOfFreedom,
summary(results05)$degreesOfFreedom,
summary(results06)$degreesOfFreedom),
AIC = round(c(
summary(results00)$AIC,
summary(results01)$AIC,
summary(results02)$AIC,
summary(results03)$AIC,
summary(results04)$AIC,
summary(results05)$AIC,
summary(results06)$AIC),3),
RMSEA = round(c(
summary(results00)$RMSEA,
summary(results01)$RMSEA,
summary(results02)$RMSEA,
summary(results03)$RMSEA,
summary(results04)$RMSEA,
summary(results05)$RMSEA,
summary(results06)$RMSEA),3))
est_res




