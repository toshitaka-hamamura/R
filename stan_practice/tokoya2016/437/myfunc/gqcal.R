#�����ʂ̃x�N�g��x�����ɁA�v�񓝌v�ʂ��v�Z����
gqcal<-function(x,digits =3,probs=c(0.025, 0.5, 0.975) ){
   y<-c(round(mean(x),digits),round(sd(x),digits),
     round(quantile(x,probs),digits))
   names(y)<-c("EAP","post.sd",probs)
   return(y)
}