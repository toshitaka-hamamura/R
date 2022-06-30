##########################################################################
#�Ή��̂���a�~b�̃N���X�\�Ɋւ��铝�v�I����
#������
#x: �N���X�\�ia �~�@b)�s��`���Ŏw��j
#prob=c(0.025, 0.05, 0.5, 0.95, 0.975): ���㕪�z�ŕ񍐂���m���_
#see=1234,cha=5,war=1000,ite=21000 : MCMC�֘A�̃p�����^
#���o��
#fit:stan�̏o��, par:�ꐔ���X�g, prob:�m���x�N�g��, 
#pim: i�sj��̕�䗦(a �~ b)
#xaste: �\�����z��x* (a �~ b)
#res: �s�A�\���c��(a �~ b)
#V: �N�������̘A�֌W��
#pa: ��i�s�̎��ӊm��(a)
#pb: ��j��̎��ӊm��(b)
#Up: �s�A�\���c����0���傫���m��(a �~ b)
#Um: �s�A�\���c����0�ȉ��̊m��(a �~ b)
#log_lik: �ΐ��ޓx
##########################################################################
Mu02 <- function(x,prob=c(0.025, 0.05, 0.5, 0.95, 0.975),
                 see=1234, cha=5, war=1000, ite=21000, fi=NA)
{
   library(rstan)
   dat <- list(a=nrow(x),b=ncol(x),x=x)
   scr <- "stan/Mu02.stan"                                    # Stan�t�@�C����
   par<-c("pim","xaste","log_lik","res","V","pa","pb","Up","Um")# sampling�ϐ�
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
#����̃��\�b�h
#������
#x:�N���X 'Mu02'�̃I�u�W�F�N�g
#degits=3 : �����̊ۂ�
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