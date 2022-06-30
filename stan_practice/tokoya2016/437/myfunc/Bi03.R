##########################################################################
#�Ή��̂Ȃ�g�Q�̔䗦�Ɋւ��铝�v�I�����ig��2�����z�j
#������
#x: ��������(����g�̃x�N�g��)
#n: �x���k�C���s��(����g�̃x�N�g��)
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): ���㕪�z�ŕ񍐂���m���_
#see=1234,cha=5,war=1000,ite=21000 : MCMC�֘A�̃p�����^
#���o��
#fit:stan�̏o��, par:�ꐔ���X�g, prob:�m���x�N�g��, 
#p: ��䗦(2)
#xaste: �\�����z��x*(2)
#U2: �s�̕�䗦����̕�䗦���傫���Ƃ�1���A����ȊO��0 (,g*g)
#log_lik;�ΐ��ޓx
##########################################################################
Bi03 <- function(x,n,prob=c(0.025, 0.05, 0.5, 0.95, 0.975),
                 see=1234, cha=5, war=1000, ite=21000, fi=NA)
{
   library(rstan)
   g <- length(x)
   dat <- list(g=g,x=x,n=n)
   scr <- "stan/Bi03.stan"                                # Stan�t�@�C����
   par<-c("p","xaste","U2","log_lik")                     # sampling�ϐ�
   fit<-stan(file=scr,data=dat,pars=par,
             seed=see,chains=cha,warmup=war,iter=ite, fit=fi)
   ext<-extract(fit,par);
   out<-list(fit=fit,par=par,prob=prob,
             p=ext$p, xaste=ext$xaste, U2=ext$U2, log_lik=ext$log_lik);
   class(out)<-'Bi03'
   return(invisible(out))
}
##########################################################################
#����̃��\�b�h
#������
#x:�N���X 'Bi03'�̃I�u�W�F�N�g
#degits=3 : �����̊ۂ�
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