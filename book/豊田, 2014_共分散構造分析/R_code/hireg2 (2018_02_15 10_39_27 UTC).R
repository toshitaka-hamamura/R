hireg2<-function(x,y,emethod='GLS',cÝ=T)
{
  if (cÝ) {q<-5;df<-2}     # q : êÌ
  else      {q<-4;df<-3}     # df: ©Rx
  #4ÜÅÌÏ¦ÌvZ;
  N<-length(x)
  mx<-mean(x); x<-x-mx; my<-mean(y); y<-y-my           #½ÏEÎ·
  x2<-x*x; x3<-x*x2; x4<-x*x3; x5<-x*x4; x6<-x*x5      #ÝæxNg;
  y2<-y*y; y3<-y*y2; y4<-y*y3; y5<-y*y4; y6<-y*y5 
  sx2<-mean(x2); sxy<-mean(x*y); sy2<-mean(y2)
  sx3<-mean(x3); sx2y<-mean(x2*y); sxy2<-mean(x*y2); sy3<-mean(y3)
  sx4<-mean(x4); sx3y<-mean(x3*y); sx2y2<-mean(x2*y2) 
  sxy3<-mean(x*y3); sy4<-mean(y4)
  iw<-dÝQÏÏ¦(x,y,emethod)
  #úl
  sê<-rep(0,q) 
  sê[1]<-sxy/sx2;                               #PCa;
  sê[2]<-sx2;                                   #QCxªU;
  sê[3]<-mean((y-(sxy/sx2)*x)^2);               #RCeªU;
  sê[4]<-sx3;                                   #SCx3Ï¦;
  if (cÝ) {
      sê[5]<-mean((y-(sxy/sx2)*x)^3);           #TCe3Ï¦;
  }
  #W{Ï¦ÌZbg
  s<-matrix(0,7,1); 
  s[1]<-sx2;  s[2]<-sxy; s[3]<-sy2;
  s[4]<-sx3; s[5]<-sx2y; s[6]<-sxy2; s[7]<-sy3;
  ÚI<-function(ê)
  {
    #VO}V[^ÌZbg
    sigthe<-matrix(0,7,1)
    sigthe[1]<-ê[2];
    sigthe[2]<-ê[1]*ê[2];
    sigthe[3]<-(ê[1]^2)*ê[2]+ê[3];
    sigthe[4]<-ê[4];
    sigthe[5]<-ê[1]*ê[4];
    sigthe[6]<-(ê[1]^2)*ê[4];
    if (cÝ) {
           sigthe[7]<-(ê[1]^3)*ê[4]+ê[5]
    }else{ sigthe[7]<-(ê[1]^3)*ê[4] }
    f<-N*( t(s-sigthe) %*% iw %*% (s-sigthe) )
    attr(f,'sigthe')<-sigthe
    return(f)
  }
  k<-nlm(ÚI,sê,hessian=T)
  ff<-ÚI(k$estimate)
  #oÍps¼Eñ¼ì¬;
  q11<-c('î{îñ')
  q12<-c('Kx','W{','©Rx','l','JÔµ','Maxùz','R[h')
  if (cÝ){ q21<-c('X«a','xªU','eªU','x3','e3')
  }else{ q21<-c('X«a','xªU','eªU','x3')}
  q22<-c('èl','Wë·','95ºÀ','95ãÀ','l','ªU','úl','ùz')
  q31<-c('X«','ØÐ')
  q32<-c('èl','Wë·','95ºÀ','95ãÀ','l')
  q41<-c('xªU','¤ªU','yªU','x3Ï¦','x2yÏ¦','xy2Ï¦','y3Ï¦')
  q42<-c('W{Ï¦','§ñÏ¦','Pc·','Wë·','Wc·','ë·ªU','dÝÎp')
  q51<-c('WñAW','xcx','ycx','ecx')
  q52<-c('W{l','§ñl')

  #oÍpsñ¶¬;
  p1<-matrix(0,1,7);  rownames(p1) <-q11;   colnames(p1) <-q12; 
  p2<-matrix(0,q,8);  rownames(p2) <-q21;   colnames(p2) <-q22; 
  p3<-matrix(0,2,5);  rownames(p3) <-q31;   colnames(p3) <-q32; 
  p4<-matrix(0,7,7);  rownames(p4) <-q41;   colnames(p4) <-q42; 
  p5<-matrix(0,4,2);  rownames(p5) <-q51;   colnames(p5) <-q52; 
  rownames(iw) <-q41;   colnames(iw) <-q41;  

  #oÍvZ
  p1[,1]<-k$minimum;p1[,2]<-N;p1[,3]<-df;
  p1[,4]<-pchisq(k$minimum,df,lower.tail=F);
  p1[,5]<-k$iterations;
  p1[,6]<-max(abs(k$gradient));p1[,7]<-k$code;
  p6<-k$hessian;
  p7<-solve(p6);
  p8<-diag(1/sqrt(diag(p7))) %*% p7  %*% diag(1/sqrt(diag(p7)));
  rownames(p6) <-q21;   colnames(p6) <-q21; 
  rownames(p7) <-q21;   colnames(p7) <-q21; 
  rownames(p8) <-q21;   colnames(p8) <-q21; 
  p2[,1]<-k$estimate;
  p2[,6]<-diag(p7);
  p2[,2]<-sqrt(p2[,6]);
  p2[,3]<-p2[,1]-(1.96*p2[,2]);
  p2[,4]<-p2[,1]+(1.96*p2[,2]);
  p2[,5]<-p2[,1]/p2[,2];
  p2[,7]<-sê;
  p2[,8]<-k$gradient;
  p3[1,1]<-sê[1];p3[2,1]<-my-(sxy/sx2)*mx;
  p3[1,2]<-sqrt(sê[3]/(N*sê[2]));
  p3[2,2]<-sqrt(((1/N)+(mx^2/(N*sê[2])))*sê[3]);
  p3[,3]<-p3[,1]-(1.96*p3[,2]);
  p3[,4]<-p3[,1]+(1.96*p3[,2]);
  p3[,5]<-p3[,1]/p3[,2];
  p4[,1]<-s; p4[,2]<-attr(ff,'sigthe');  p4[,3]<-p4[,1]-p4[,2];
  p4[,6]<-diag(solve(iw))/N;   p4[,4]<-sqrt(p4[,6]);
  p4[,5]<-p4[,3]/p4[,4];  p4[,7]<-diag(iw); 
  p5[1,1]<-s[2]/sqrt(s[1]*s[3]); 
  p5[2,1]<-s[4]/(sqrt(s[1])^3);
  p5[3,1]<-s[7]/(sqrt(s[3])^3);
  p5[4,1]<-(mean((y-(sxy/sx2)*x)^3))/(sqrt(mean((y-(sxy/sx2)*x)^2))^3); 
  p5[1,2]<-p4[2,2]/sqrt(p4[1,2]*p4[3,2]);
  p5[2,2]<-p4[4,2]/(sqrt(p4[1,2])^3);
  p5[3,2]<-p4[7,2]/(sqrt(p4[3,2])^3);
  if (cÝ){  p5[4,2]<-p2[5,1]/(sqrt(p2[3,1])^3)
  }else {p5[4,2]<-0}  ;

  #oÍønµ
  Ê<-list()
  Ê$î{îñ    <- p1
  Ê$èl      <- p2
  Ê$ÊíñA    <- p3
  Ê$c·Ìîñ  <- p4
  Ê$W»W  <- p5
  Ê$wVA    <- p6
  Ê$êÔ¤ªU<- p7
  Ê$êÔÖ  <- p8
  Ê$dÝsñ    <- iw
  return(Ê)
}
