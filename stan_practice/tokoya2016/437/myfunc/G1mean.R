##########################################################################
#1群の正規分布に関する推測
#■入力
#x:ベクトル形式のデータ 
#prior=F : 論理値。Tなら事前分布の範囲を指定。Fなら指定せず。
#mL, mH, sL, sH : 事前分布のパラメタ
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): 事後分布で報告する確率点
#see=1234,cha=5,war=1000,ite=21000 : MCMC関連のパラメタ
#■出力
#fit:stanの出力, par:母数リスト, prob:確率ベクトル, 
#mu:平均, sigma:標準偏差, xaste:予測分布, log_lik:対数尤度
##########################################################################
G1mean <- function(x,prior=F, mL=-1000, mH=1000, sL=0, sH=100,
prob=c(0.025, 0.05, 0.5, 0.95, 0.975),see=1234,cha=5,war=1000,ite=21000,fi=NA)
{
   library(rstan)
   if (prior) {
     dat <- list(n=length(x),x=x,mL=mL,mH=mH,sL=sL,sH=sH)
     scr <- "stan/G1meanPT.stan"                           # Stan ファイル名
   } else {
     dat <- list(n=length(x),x=x)
     scr <- "stan/G1meanPF.stan"                           # Stan ファイル名
   }
   par<-c("mu","sigma","xaste","log_lik")        # sampling変数
   fit<-stan(file=scr,data=dat,pars=par,seed=see,chains=cha,warmup=war,iter=ite, fit=fi)
   ext<-extract(fit, c("mu","sigma","xaste","log_lik"))
   mu<-ext$mu; sigma<-ext$sigma; xaste<-ext$xaste; log_lik<-ext$log_lik
   out<-list(fit=fit,par=par,prob=prob,mu=mu,sigma=sigma,xaste=xaste,log_lik=log_lik)
   class(out)<-'G1mean'
   return(invisible(out))
}
##########################################################################
#印刷のメソッド
#x:クラス 'G1mean'のオブジェクト
#degits=3 : 小数の丸め
#cr1=F : 効果量の基準点      (測定値で指定)
#cr2a=F : 分布関数上限        (測定値で指定)
#cr2b=F : 分布関数下限        (測定値で指定)
#cr3=F : 測定値との比の基準点(測定値で指定)
#cr4=F : 平均値がcr4より小さい確率(測定値で指定)
#cr5=F : cr1による効果量がcr5より小さい確率(効果量で指定)
#pr1=F : ％点の基準点        (下からの確率で指定）
#pr2=F : cr2の領域が観測される確率が、pr2より大きい確率(確率で指定)
##########################################################################
print.G1mean<-function(x,degits=3,cr1=F,cr2a=F,cr2b=F,cr3=F,cr4=F,cr5=F,pr1=F,pr2=F)
{
   mu<-x$mu; sigma<-x$sigma; xaste<-x$xaste; log_lik<-x$log_lik
   prob<-x$prob
   G<-matrix(0,length(mu),6)
   G[,1]<- sigma^2;                  #分散
   G[,2]<- sigma/mu;                 #変動係数
   co<-2; lab<-c("分散","変動係数")
   if(is.numeric(cr1)){co<- co+1;G[,co]<- (mu-cr1)/sigma;
      lab<-c(lab,paste("効果量(",cr1,")",sep=""))}
   if(is.numeric(pr1)){co<- co+1;G[,co]<- mu+qnorm(pr1)*sigma;
      lab<-c(lab,paste("％点(",pr1,")",sep=""))}   
   if((is.numeric(cr2a))&(!is.numeric(cr2b)))
                      {co<- co+1;G[,co]<- pnorm(cr2a,mu,sigma);
      lab<-c(lab,paste("測定値が(",cr2a,")より小さい確率",sep=""))}
   if((is.numeric(cr2b))&(!is.numeric(cr2a)))
                      {co<- co+1;G[,co]<- 1-pnorm(cr2b,mu,sigma);
      lab<-c(lab,paste("測定値が(",cr2b,")より大きい確率",sep=""))}
   if((is.numeric(cr2a))&(is.numeric(cr2b)))
                      {co<- co+1;G[,co]<- pnorm(cr2a,mu,sigma)-pnorm(cr2b,mu,sigma);
      lab<-c(lab,paste("測定値が(",cr2b,")より大きく(",cr2a,")より小さい確率",sep=""))}
   if(is.numeric(cr3)){co<- co+1;G[,co]<- xaste/cr3;
      lab<-c(lab,paste("基準点との比(",cr3,")",sep=""))}
   Gc<-cbind(
      apply(G[,1:co],2,mean),
      apply(G[,1:co],2,sd),
      t(apply(G[,1:co],2,quantile, probs=prob))
   )
   colnames(Gc)<-c("EAP","post.sd",prob)
   rownames(Gc)<-lab
   U<-matrix(0,length(mu),4)
   co<-0;lab<-NULL
   if(is.numeric(cr4)){co<- co+1;U[,co]<- ifelse(mu< cr4,    1, 0);
         lab<-c(lab,paste("平均値が(",cr4,")より小さい確率",sep=""))}
   if((is.numeric(cr2a))&(!is.numeric(cr2b)))
                      {co<- co+1;U[,co]<- ifelse(xaste< cr2a,1,0);
      lab<-c(lab,paste("測定値が(",cr2a,")より小さい確率(点推定)",sep=""))}
   if((is.numeric(cr2b))&(!is.numeric(cr2a)))
                      {co<- co+1;U[,co]<- ifelse(xaste> cr2b,1,0);
      lab<-c(lab,paste("測定値が(",cr2b,")より大きい確率(点推定)",sep=""))}
   if((is.numeric(cr2a))&(is.numeric(cr2b)))
                      {co<- co+1;U[,co]<- ifelse((xaste< cr2a)&(xaste> cr2b),1,0);
      lab<-c(lab,paste("測定値が(",cr2b,")より大きく(",cr2a,")より小さい確率(点推定)",sep=""))}
   if(is.numeric(cr5)){co<- co+1;U[,co]<- ifelse(((mu-cr1)/sigma)< cr5, 1, 0);
         lab<-c(lab,paste("(",cr1,")による効果量が(",cr5,")より小さい確率",sep=""))}
   if((is.numeric(cr2a))&(!is.numeric(cr2b)))
                      {co<- co+1;U[,co]<- ifelse(pnorm(cr2a,mu,sigma)>pr2,1,0);
      lab<-c(lab,paste("測定値が(",cr2a,")より小さい確率が(",pr2,")より大きい確率",sep=""))}
   if((is.numeric(cr2b))&(!is.numeric(cr2a)))
                      {co<- co+1;U[,co]<- ifelse(1-pnorm(cr2b,mu,sigma)>pr2,1,0);
      lab<-c(lab,paste("測定値が(",cr2b,")より大きい確率が(",pr2,")より大きい確率",sep=""))}
   if((is.numeric(cr2a))&(is.numeric(cr2b)))
                      {co<- co+1;U[,co]<- ifelse(pnorm(cr2a,mu,sigma)-pnorm(cr2b,mu,sigma)>pr2,1,0);
      lab<-c(lab,paste("測定値が(",cr2b,")より大きく(",cr2a,")より小さい確率が(",pr2,")より大きい確率",sep=""))}
   if(co>0.9){
      Uc<-matrix(colMeans(as.matrix(U[,1:co])),co,1);
      rownames(Uc)<-lab
      }else{Uc<-0.0}
   print(x$fit,pars=x$par,digits_summary=degits,probs=x$prob)
   print(round(Gc,degits))
   print(round(Uc,degits))
   out<-list(G=G,Gc=Gc,Uc=Uc)
   return(invisible(out))
}
