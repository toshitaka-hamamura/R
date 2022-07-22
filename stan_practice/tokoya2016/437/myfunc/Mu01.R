##########################################################################
#カテゴリ数がkの比率の統計的推測
#■入力
#x: 正反応数(長さkのベクトル)
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): 事後分布で報告する確率点
#see=1234,cha=5,war=1000,ite=21000 : MCMC関連のパラメタ
#■出力
#fit:stanの出力, par:母数リスト, prob:確率ベクトル, 
#pi: 母比率(k)
#U2: 行の母比率が列の母比率より大きいとき1を、それ以外は0 (,k*k)
#log_lik;対数尤度
##########################################################################
Mu01 <- function(x,prob=c(0.025, 0.05, 0.5, 0.95, 0.975),
                 see=1234, cha=5, war=1000, ite=21000, fi=NA)
{
   library(rstan)
   k <- length(x)
   dat <- list(k=k,x=x)
   scr <- "stan/Mu01.stan"                         # Stanファイル名
   par<-c("pi","U2","log_lik")                     # sampling変数
   fit<-stan(file=scr,data=dat,pars=par,
             seed=see,chains=cha,warmup=war,iter=ite, fit=fi)
   ext<-extract(fit,par);
   out<-list(fit=fit,par=par,prob=prob,
             pi=ext$p, U2=ext$U2, log_lik=ext$log_lik);
   class(out)<-'Mu01'
   return(invisible(out))
}
##########################################################################
#印刷のメソッド
#■入力
#x:クラス 'Mu01'のオブジェクト
#degits=3 : 小数の丸め
##########################################################################
print.Mu01<-function(x,degits=3)
{
   log_lik<-x$log_lik; 
   waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik))
   print(x$fit,pars=x$par,digits_summary=degits,probs=x$prob)
   print(paste("waic=",round(waic,degits),sep=""))
   out<-list(waic=waic)
   return(invisible(out))
}  
