##########################################################################
#対応のあるa×bのクロス表に関する統計的推測
#■入力
#x: クロス表（a ×　b)行列形式で指定）
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): 事後分布で報告する確率点
#see=1234,cha=5,war=1000,ite=21000 : MCMC関連のパラメタ
#■出力
#fit:stanの出力, par:母数リスト, prob:確率ベクトル, 
#pim: i行j列の母比率(a × b)
#xaste: 予測分布のx* (a × b)
#res: ピアソン残差(a × b)
#V: クラメルの連関係数
#pa: 第i行の周辺確率(a)
#pb: 第j列の周辺確率(b)
#Up: ピアソン残差が0より大きい確率(a × b)
#Um: ピアソン残差が0以下の確率(a × b)
#log_lik: 対数尤度
##########################################################################
Mu02 <- function(x,prob=c(0.025, 0.05, 0.5, 0.95, 0.975),
                 see=1234, cha=5, war=1000, ite=21000, fi=NA)
{
   library(rstan)
   dat <- list(a=nrow(x),b=ncol(x),x=x)
   scr <- "stan/Mu02.stan"                                    # Stanファイル名
   par<-c("pim","xaste","log_lik","res","V","pa","pb","Up","Um")# sampling変数
   fit<-stan(file=scr,data=dat,pars=par,
             seed=see,chains=cha,warmup=war,iter=ite, fit=fi)
   ext<-extract(fit,par);
   out<-list(fit=fit,par=par,prob=prob,
             pim=ext$pim, xaste=ext$xaste,log_lik=ext$log_lik,
             res=ext$res,V=ext$V,pa=ext$pa,pb=ext$pb,Up=ext$Up,Um=ext$Um);
   class(out)<-'Mu02'
   return(invisible(out))
}
##########################################################################
#印刷のメソッド
#■入力
#x:クラス 'Mu02'のオブジェクト
#degits=3 : 小数の丸め
##########################################################################
print.Mu02<-function(x,degits=3)
{
   log_lik<-x$log_lik; 
   waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik))
   print(x$fit,pars=x$par,digits_summary=degits,probs=x$prob)
   print(paste("waic=",round(waic,degits),sep=""))
   out<-list(waic=waic)
   return(invisible(out))
}  
