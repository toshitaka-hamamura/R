##########################################################################
#重回帰モデル
#■入力
#y: 基準変数(n)
#X: 予測変数行列(n*p)
#see=1234,cha=5,war=1000,ite=21000 : MCMC関連のパラメタ
#■出力
#fit:stanの出力, par:母数リスト,  
#b:  回帰係数ベクトル(p)
#sigma: 誤差標準偏差
#r2: 決定係数
#vyhat: yhatの分散
#log_lik;対数尤度
##########################################################################
Reg <- function(y,X,see=1234, cha=5, war=1000, ite=21000, fi=NA)
{
   library(rstan)
   y<-as.vector(y); X<-as.matrix(X);
   n <- length(y);   p <- ncol(X)
   X0<-cbind(X,1);   bi<-as.vector(solve(t(X0)%*%X0)%*%t(X0)%*%y);
   sigmai<- as.vector(sqrt((sum(y^2)-t(bi)%*%t(X0)%*%y)/n));
   initi<-function(){list(bb=bi,sigma=sigmai)}
   dat <- list(n=n, p=p, y=y, X=X0)
   scr <- "stan/Reg.stan"                             # Stanファイル名
   par<-c("a","b","sigma","r2","r","log_lik")     # sampling変数
   fit<-stan(file=scr,data=dat,pars=par,
             seed=see,chains=cha,warmup=war,iter=ite,init =initi, fit=fi)
   ext<-extract(fit,par);
   out<-list(fit=fit,par=par,a=ext$a,b=ext$b,sigma=ext$sigma,r2=ext$r2,
             r=ext$r, X=X, y=y, log_lik=ext$log_lik);
   class(out)<-'Reg'
   return(invisible(out))
}
##########################################################################
#印刷のメソッド
#■入力
#x:クラス 'Reg'のオブジェクト
#degits=3 : 小数の丸め
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): 事後分布で報告する確率点,
#Xnew=F   : (n*P)の行列、単回帰の場合はサイズnのベクトル　予測分布用データ
#■出力
#waic=waic、y=y、X=X
#yhat=予測値、resi=残差、（Xによる）
#Ga Gc   標準編回帰係数
#yhata yhatc　回帰式の事後分布　　（Xnewによる）
#yasta yastc　予測分布　（Xnewによる）
##########################################################################
print.Reg<-function(x,degits=3, prob=c(0.025, 0.05, 0.5, 0.95, 0.975),Xnew=F)
{
   Ga=NULL; Gc=NULL; yhata=NULL; yhatc=NULL; yasta=NULL; yastc=NULL;
   a<-x$a; b<-x$b; sigma<-x$sigma; r2<-x$r2; X<-x$X;
   y<-x$y; log_lik<-x$log_lik; 
   vari<-function(x){mean((x-mean(x))^2)};
   sdX<-sqrt(apply(X,2,vari))
   waic<- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik))
   print(x$fit,pars=x$par,digits_summary=degits,probs=prob)
 cat("標準偏回帰係数と重相関\n")
   Ga<-matrix(0,nrow(b),ncol(b))
   for (i in 1:ncol(b)){ Ga[,i] <- b[,i]*sdX[i]/sqrt(vari(y));}
   Gc<-cbind(apply(Ga,2,mean), apply(Ga,2,sd),
      t(apply(Ga,2,quantile, probs=prob)) )
      colnames(Gc)<-c("EAP","post.sd",prob)
      rownames(Gc)<-paste("b",1:ncol(b),sep="")
      print(round(Gc,degits))
#Xによるyhatと残差
   yhat <- apply((b%*%t(X))+a%*%matrix(1,1,nrow(X)),2,mean);
   names(yhat)<-rownames(X)
   resi<- y-yhat;
# cat("予測値\n")
#   print(round(yhat,degits))
# cat("残差\n")
#   print(round(resi,degits))
 cat("情報量規準\n")
   print(paste("waic=",round(waic,degits),sep=""))
#Xnewによるyhatと予測分布
 if (!(is.logical(Xnew))){
    if (length(dim(Xnew))<2){Xnew<-matrix(Xnew,,1)}
 cat("回帰式の事後分布\n")
   yhata<-matrix(0,nrow(b),nrow(Xnew))
   yhata <- (b%*%t(Xnew))+a%*%matrix(1,1,nrow(Xnew));
   yhatc<-cbind(apply(yhata,2,mean), apply(yhata,2,sd),
      t(apply(yhata,2,quantile, probs=prob)) )
      colnames(yhatc)<-c("EAP","post.sd",prob)
      rownames(yhatc)<-rownames(Xnew)
      print(round(yhatc,degits))
 cat("基準変数の予測分布\n")
   yasta<-matrix(0,nrow(b),nrow(Xnew))
   for (j in 1:nrow(Xnew))
      {yasta[,j]<-rnorm(nrow(b),yhata[,j],sigma)}
      yastc<-cbind(apply(yasta,2,mean), apply(yasta,2,sd),
      t(apply(yasta,2,quantile, probs=prob)) )
      colnames(yastc)<-c("EAP","sd",prob)
      rownames(yastc)<-rownames(Xnew)
      print(round(yastc,degits))
}
   out<-list(waic=waic,y=y,X=X,yhat=yhat,resi=resi,Ga=Ga, Gc=Gc,yhata=yhata,
 yhatc=yhatc, yasta= yasta,yastc=yastc)
   return(invisible(out))
}  

