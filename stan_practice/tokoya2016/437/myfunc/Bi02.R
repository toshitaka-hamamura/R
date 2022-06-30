##########################################################################
#�Ή��̂Ȃ�2�Q�̔䗦�Ɋւ��铝�v�I�����i2��2�����z�j
#������
#x: ��������(����2�̃x�N�g���A�W�{�䗦�̑傫�����̂��Ɏw�肷��)
#n: �x���k�C���s��(����2�̃x�N�g��)
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): ���㕪�z�ŕ񍐂���m���_
#see=1234,cha=5,war=1000,ite=21000 : MCMC�֘A�̃p�����^
#���o��
#fit:stan�̏o��, par:�ꐔ���X�g, prob:�m���x�N�g��, 
#p: ��䗦(2)
#xaste: �\�����z��x*(2)
#p_sa: �䗦�̍�
#p_hi: �䗦�̔�
#Odds: �I�b�Y(2)
#Odds_hi: �I�b�Y��
#log_lik;�ΐ��ޓx
##########################################################################
Bi02 <- function(x,n,prob=c(0.025, 0.05, 0.5, 0.95, 0.975),
                 see=1234, cha=5, war=1000, ite=21000, fi=NA)
{
   library(rstan)
   dat <- list(x=x,n=n)
   scr <- "stan/Bi02.stan"                                     # Stan�t�@�C����
   par<-c("p","xaste","p_sa","p_hi","Odds","Odds_hi","log_lik")# sampling�ϐ�
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
#����̃��\�b�h
#������
#x:�N���X 'Bi02'�̃I�u�W�F�N�g
#degits=3 : �����̊ۂ�
#cr1=F : �䗦�̍��̍ŏ��l
#cr2=F : �䗦�̔�̍ŏ��l
#cr3=F : �I�b�Y��̍ŏ��l
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
         lab<-c(lab,paste("�䗦�̍���(",cr1,")���傫��",sep=""))}
   if(is.numeric(cr2)){co<- co+1;U[,co]<- ifelse(p_hi>  cr2, 1, 0);
         lab<-c(lab,paste("�䗦�̔䂪(",cr2,")���傫��",sep=""))}
   if(is.numeric(cr3)){co<- co+1;U[,co]<- ifelse(Odds_hi> cr3, 1, 0);
         lab<-c(lab,paste("�I�b�Y�䂪(",cr3,")���傫��",sep=""))}
   if(co>0.9){
      Uc<-matrix(colMeans(as.matrix(U[,1:co])),co,1);
      rownames(Uc)<-lab
      }else{Uc<-0.0}
   print(round(Uc,degits))
   print(paste("waic=",round(waic,degits),sep=""))
   out<-list(waic=waic,Uc=Uc)
   return(invisible(out))
}  