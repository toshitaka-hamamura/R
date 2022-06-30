##########################################################################
#対応のないg群の比率に関する統計的推測（g個の2項分布）
#■入力
#x: 正反応数(長さgのベクトル)
#n: ベルヌイ試行数(長さgのベクトル)
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): 事後分布で報告する確率点
#see=1234,cha=5,war=1000,ite=21000 : MCMC関連のパラメタ
#■出力
#fit:stanの出力, par:母数リスト, prob:確率ベクトル, 
#p: 母比率(2)
#xaste: 予測分布のx*(2)
#U2: 行の母比率が列の母比率より大きいとき1を、それ以外は0 (,g*g)
#log_lik;対数尤度
##########################################################################
Bi03 <- function(x,n,prob=c(0.025, 0.05, 0.5, 0.95, 0.975),
                 see=1234, cha=5, war=1000, ite=21000, fi=NA)
{
   library(rstan)
   g <- length(x)
   dat <- list(g=g,x=x,n=n)
   scr <- "stan/Bi03.stan"                                # Stanファイル名
   par<-c("p","xaste","U2","log_lik")                     # sampling変数
   fit<-stan(file=scr,data=dat,pars=par,
             seed=see,chains=cha,warmup=war,iter=ite, fit=fi)
   ext<-extract(fit,par);
   out<-list(fit=fit,par=par,prob=prob,
             p=ext$p, xaste=ext$xaste, U2=ext$U2, log_lik=ext$log_lik);
   class(out)<-'Bi03'
   return(invisible(out))
}
##########################################################################
#印刷のメソッド
#■入力
#x:クラス 'Bi03'のオブジェクト
#degits=3 : 小数の丸め
##########################################################################
print.Bi03<-function(x,degits=3)
{
   log_lik<-x$log_lik; p<-x$p; xaste<-x$xaste; U2<-x$U2; 
   waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik))
   print(x$fit,pars=x$par,digits_summary=degits,probs=x$prob)
   print(paste("waic=",round(waic,degits),sep=""))
   out<-list(waic=waic)
   return(invisible(out))
}  
