##########################################################################
#対応のない2群の比率に関する統計的推測（2つの2項分布）
#■入力
#x: 正反応数(長さ2のベクトル、標本比率の大きいものを先に指定する)
#n: ベルヌイ試行数(長さ2のベクトル)
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): 事後分布で報告する確率点
#see=1234,cha=5,war=1000,ite=21000 : MCMC関連のパラメタ
#■出力
#fit:stanの出力, par:母数リスト, prob:確率ベクトル, 
#p: 母比率(2)
#xaste: 予測分布のx*(2)
#p_sa: 比率の差
#p_hi: 比率の比
#Odds: オッズ(2)
#Odds_hi: オッズ比
#log_lik;対数尤度
##########################################################################
Bi02 <- function(x,n,prob=c(0.025, 0.05, 0.5, 0.95, 0.975),
                 see=1234, cha=5, war=1000, ite=21000, fi=NA)
{
   library(rstan)
   dat <- list(x=x,n=n)
   scr <- "stan/Bi02.stan"                                     # Stanファイル名
   par<-c("p","xaste","p_sa","p_hi","Odds","Odds_hi","log_lik")# sampling変数
   fit<-stan(file=scr,data=dat,pars=par,
             seed=see,chains=cha,warmup=war,iter=ite, fit=fi)
   ext<-extract(fit,par);
   out<-list(fit=fit,par=par,prob=prob,
             p=ext$p, xaste=ext$xaste, log_lik=ext$log_lik, p_sa=ext$p_sa, 
             p_hi=ext$p_hi,Odds=ext$Odds, Odds_hi=ext$Odds_hi);
   class(out)<-'Bi02'
   return(invisible(out))
}
##########################################################################
#印刷のメソッド
#■入力
#x:クラス 'Bi02'のオブジェクト
#degits=3 : 小数の丸め
#cr1=F : 比率の差の最小値
#cr2=F : 比率の比の最小値
#cr3=F : オッズ比の最小値
##########################################################################
print.Bi02<-function(x,degits=3,cr1=F,cr2=F,cr3=F)
{
   log_lik<-x$log_lik; p<-x$p; xaste<-x$xaste; p_sa<-x$p_sa; 
   p_hi<-x$p_hi; Odds<-x$Odds;  Odds_hi<-x$Odds_hi;
   waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik))
   print(x$fit,pars=x$par,digits_summary=degits,probs=x$prob)
   U<-matrix(0,length(p),3)
   co<-0;lab<-NULL
   if(is.numeric(cr1)){co<- co+1;U[,co]<- ifelse(p_sa> cr1, 1, 0);
         lab<-c(lab,paste("比率の差が(",cr1,")より大きい",sep=""))}
   if(is.numeric(cr2)){co<- co+1;U[,co]<- ifelse(p_hi>  cr2, 1, 0);
         lab<-c(lab,paste("比率の比が(",cr2,")より大きい",sep=""))}
   if(is.numeric(cr3)){co<- co+1;U[,co]<- ifelse(Odds_hi> cr3, 1, 0);
         lab<-c(lab,paste("オッズ比が(",cr3,")より大きい",sep=""))}
   if(co>0.9){
      Uc<-matrix(colMeans(as.matrix(U[,1:co])),co,1);
      rownames(Uc)<-lab
      }else{Uc<-0.0}
   print(round(Uc,degits))
   print(paste("waic=",round(waic,degits),sep=""))
   out<-list(waic=waic,Uc=Uc)
   return(invisible(out))
}  
