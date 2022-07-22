##########################################################################
#比率の推測（1つの2項分布）
#■入力
#x: 正反応数
#n: ベルヌイ試行数
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): 事後分布で報告する確率点
#see=1234,cha=5,war=1000,ite=21000 : MCMC関連のパラメタ
#■出力
#fit:stanの出力, par:母数リスト, prob:確率ベクトル, 
#theta: 母比率, xaste: 予測分布, Odds:オッズ
#log_lik;対数尤度
##########################################################################
Bi01 <- function(x,n,prob=c(0.025, 0.05, 0.5, 0.95, 0.975),
                 see=1234, cha=5, war=1000, ite=21000, fi=NA)
{
   library(rstan)
   dat <- list(x=x,n=n)
   scr <- "stan/Bi01.stan"                                # Stan ファイル名
   par<-c("theta","xaste","Odds","log_lik")               # sampling変数
   fit<-stan(file=scr,data=dat,pars=par,
             seed=see,chains=cha,warmup=war,iter=ite, fit=fi)
   ext<-extract(fit,par);
   out<-list(fit=fit,par=par,prob=prob,
        theta=ext$theta,xaste=ext$xaste, Odds=ext$Odds,log_lik=ext$log_lik);
   class(out)<-'Bi01'
   return(invisible(out))
}
##########################################################################
#印刷のメソッド
#■入力
#x:クラス 'Bi01'のオブジェクト
#degits=3 : 小数の丸め
#pr1=F : thetaの最小値
#cr1=F : x*の最小値
#cr2=F : オッズの最小値
##########################################################################
print.Bi01<-function(x,degits=3,pr1=F,cr1=F,cr2=F)
{
   log_lik<-x$log_lik;theta<-x$theta;xaste<-x$xaste;Odds<-x$Odds;
   waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik))
   print(x$fit,pars=x$par,digits_summary=degits,probs=x$prob)
   U<-matrix(0,length(theta),3)
   co<-0;lab<-NULL
   if(is.numeric(pr1)){co<- co+1;U[,co]<- ifelse(theta> pr1, 1, 0);
         lab<-c(lab,paste("比率が(",pr1,")より大きい",sep=""))}
   if(is.numeric(cr1)){co<- co+1;U[,co]<- ifelse(xaste> cr1, 1, 0);
         lab<-c(lab,paste("x*が(",cr1,")より大きい",sep=""))}
   if(is.numeric(cr2)){co<- co+1;U[,co]<- ifelse(Odds>  cr2, 1, 0);
         lab<-c(lab,paste("オッズが(",cr2,")より大きい",sep=""))}
   if(co>0.9){
      Uc<-matrix(colMeans(as.matrix(U[,1:co])),co,1);
      rownames(Uc)<-lab
      }else{Uc<-0.0}
   print(round(Uc,degits))
   print(paste("waic=",round(waic,degits),sep=""))
   out<-list(waic=waic,Uc=Uc)
   return(invisible(out))
}  
