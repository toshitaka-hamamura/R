con_hist<-function(z,•=0.5,‰¡Ž²="",digits=3,ã‹ó=300,‰¡‹ó=100)
{
   q0<-min(z);q1<-quantile(z,0.025);q2<-quantile(z,0.05);q3<-quantile(z,0.95)
   q4<-quantile(z,0.975);q5<-max(z);e<-(q5-q0)/‰¡‹ó
   ymax<-max(hist(z,breaks=seq(q0-e,q5+e,•))$counts)
   hist(z,breaks=seq(q0-e,q5+e,•),xlab=‰¡Ž²,cex.lab=1.5,cex.axis=1.5,main="",ylab="",ylim=c(0,ymax*1.1))
   eap<-mean(z)
   segments(eap,0,eap,ymax,lwd=2.0);text(eap,ymax+ã‹ó,paste("EAP",round(eap,digits)),cex=1.5)
   par(new=T)
   hist(z[((z<q2)|(q3<z))],breaks=seq(q0-e,q5+e,•),xlab='',cex.lab=1.5,cex.axis=1.5,main="",ylab="",ylim=c(0,ymax*1.1),col = gray(0.8))
   par(new=T)
   hist(z[((z<q1)|(q4<z))],breaks=seq(q0-e,q5+e,•),xlab='',cex.lab=1.5,cex.axis=1.5,main="",ylab="",ylim=c(0,ymax*1.1),col = gray(0.4))
}
