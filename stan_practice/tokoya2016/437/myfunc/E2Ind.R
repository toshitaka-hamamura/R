##########################################################################
#独立な2要因の実験データを分析
#■入力
#y: ベクトル形式の特性値
#A: ベクトル形式の水準(1からaまで、抜けなしで、整数で指定)
#B: ベクトル形式の水準(1からbまで、抜けなしで、整数で指定)
#prior=F : 論理値。Tなら事前分布の範囲を指定。Fなら指定せず。
#mL=-1000, mH=1000, sL=0, sH=100 : 事前分布のパラメタ
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): 事後分布で報告する確率点
#see=1234,cha=5,war=1000,ite=21000 : MCMC関連のパラメタ
#■出力
# fit:stanの出力, par:母数リスト, prob:確率ベクトル, 
#mu;全平均, [a]muA;A平均, [b]muB;B平均, [a,b]muAB;交互作用, 
#[a,b]cellmean;セル平均値, sigmaA;A標準偏差, sigmaB;B標準偏差,sigmaAB;AB標準偏差
#sigmaE;E標準偏差, eta2A;A説明率, eta2B;B説明率, eta2AB;AB説明率, eta2T;全説明率
#deltaA;A効果量, deltaB;B効果量, deltaAB;AB効果量, UbigA[a];A論理値0以上確率, 
#UsmaA[a];A論理値0以下確率, UbigB[b];B論理値0以上確率, UsmaB[b];B論理値0以下確率
#UbigAB[a,b];AB論理値0以上確率, UsmaAB[a,b];AB論理値0以下確率, 
#U2A[a,a];Aの2水準比較, U2B[b,b];Bの2水準比較, log_lik;対数尤度
##########################################################################
E2Ind <- function(y,A,B,prior=F,mL=-1000, mH=1000, sL=0, sH=100,
prob=c(0.025, 0.05, 0.5, 0.95, 0.975),
see=1234, cha=5, war=1000, ite=21000, fi=NA)
{
   library(rstan)
   if (prior) {
     dat <- list(n=length(y),a=length(unique(A)),b=length(unique(B)),
                 y=y,A=A,B=B,mL=mL,mH=mH,sL=sL,sH=sH)
     scr <- "stan/E2IndPT.stan"                           # Stan ファイル名
   } else {
     dat <- list(n=length(y),a=length(unique(A)),b=length(unique(B)),
                 y=y,A=A,B=B)
     scr <- "stan/E2IndPF.stan"                           # Stan ファイル名
   }
   par<-c("mu","muA","muB","muAB","cellmean","sigmaA","sigmaB","sigmaAB",
       "sigmaE","eta2A","eta2B","eta2AB","eta2T","deltaA","deltaB","deltaAB",
       "UbigA","UsmaA","UbigB","UsmaB","UbigAB","UsmaAB","U2A","U2B","log_lik")
   fit<-stan(file=scr,data=dat,pars=par,seed=see,chains=cha,warmup=war,iter=ite, fit=fi)
   ext<-extract(fit,par);
   out<-list(prior=prior,fit=fit,par=par,prob=prob,
      mu=ext$mu,muA=ext$muA,muB=ext$muB,muAB=ext$muAB,cellmean=ext$cellmean,
      sigmaA=ext$sigmaA,sigmaB=ext$sigmaB,sigmaAB=ext$sigmaAB,sigmaE=ext$sigmaE,
      eta2A=ext$eta2A,eta2B=ext$eta2B,eta2AB=ext$eta2AB,eta2T=ext$eta2T,
      deltaA=ext$deltaA,deltaB=ext$deltaB,deltaAB=ext$deltaAB,UbigA=ext$UbigA,
      UsmaA=ext$UsmaA,UbigB=ext$UbigB,UsmaB=ext$UsmaB,UbigAB=ext$UbigAB,
      UsmaAB=ext$UsmaAB,U2A=ext$U2A,U2B=ext$U2B,log_lik=ext$log_lik)
   class(out)<-'E2Ind'
   return(invisible(out))
}
##########################################################################
#印刷のメソッド
#■入力
#x:クラス 'E2Ind'のオブジェクト
#degits=3 : 小数の丸め
##########################################################################
print.E2Ind<-function(x,degits=3)
{
   prior<-x$prior;log_lik<-x$log_lik;
   if (prior) {print("******************事前分布指定******************")}
         else {print("******************事前分布デフォルト************")}
   waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik))
   print(x$fit,pars=x$par,digits_summary=degits,probs=x$prob)
   print(paste("waic=",round(waic,degits),sep=""))
   out<-list(waic=waic)
   return(invisible(out))
}  
##########################################################################
#２つのセルの比較
#■入力
#x:クラス 'E2Ind'のオブジェクト
#degits=3 : 小数の丸め
#H="A": 固定する因子　A or B
#F=1: 整数、固定する因子の水準
#I=1: 整数、比較する因子の平均値の大きい水準
#J=2: 整数、比較する因子の平均値の小さい
#cr1: 閾上率の基準値
##########################################################################
E2betw_level<-function(x,degits=3,H="A",F=1,I=1,J=2,cr1=F)
{
   if (H=='A') { big<-x$cellmean[,F,I];sma<-x$cellmean[,F,J];比較<-"B";}
          else { big<-x$cellmean[,I,F];sma<-x$cellmean[,J,F];比較<-"A";}
   G<-matrix(0,length(x$mu),5)
   G[,1] <- big-sma;
   G[,2] <- G[,1]/x$sigmaE;
   G[,3] <- pnorm(big,sma,x$sigmaE);
   G[,4] <- pnorm((G[,2]/sqrt(2)), 0.0, 1.0);
   lab<-c("平均値の差","効果量","非重複度","優越率")
   co<-4
   if(is.numeric(cr1)){  co<-co+1;
      G[,5] <- pnorm(((G[,1]-cr1)/(sqrt(2)*x$sigmaE)), 0.0, 1.0);
      lab<-c(lab,paste("閾上率(",cr1,")",sep=""));}
   Gc<-cbind(
      apply(G[,1:co],2,mean),
      apply(G[,1:co],2,sd),
      t(apply(G[,1:co],2,quantile, probs=c(0.025, 0.05, 0.5, 0.95, 0.975)))
   )
   colnames(Gc)<-c("EAP","post.sd","2.5%","5%","50%","95%","97.5%")
   rownames(Gc)<-lab
   cat("要因",H,"の水準",F,"を固定して、要因",比較,"の水準",I,J,"を比較する\n")
   round(Gc,degits)
}

