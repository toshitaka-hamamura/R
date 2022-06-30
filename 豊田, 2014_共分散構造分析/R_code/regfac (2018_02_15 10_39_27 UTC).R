regfac<-function(x,y,emethod='GLS',歪み=TRUE)
{
  df=1
  if (歪み) {df<-0}
  q<- 7-df                       # df: 自由度 q: 母数の数
  #4次までの積率の計算;
  N<-length(x)
  mx<-mean(x); x<-x-mx; my<-mean(y); y<-y-my           #平均・偏差
  x2<-x*x; x3<-x*x2; x4<-x*x3; x5<-x*x4; x6<-x*x5      #累乗ベクトル;
  y2<-y*y; y3<-y*y2; y4<-y*y3; y5<-y*y4; y6<-y*y5 
  sx2<-mean(x2); sxy<-mean(x*y); sy2<-mean(y2)
  sx3<-mean(x3); sx2y<-mean(x2*y); sxy2<-mean(x*y2); sy3<-mean(y3)
  sx4<-mean(x4); sx3y<-mean(x3*y); sx2y2<-mean(x2*y2) 
  sxy3<-mean(x*y3); sy4<-mean(y4)
  iw<-重み２変数積率(x,y,emethod)
  #初期値(因子の3次の積率が正になるように)
  s母数<-rep(0,q) 
  s母数[1]<-sqrt(sx2y*sxy2/(sxy^3));               #１，因子3次積率a;
  s母数[2]<-sx2y/(sxy*s母数[1]);                   #２，x因子パタン;
  s母数[3]<-sxy2/(sxy*s母数[1]);                   #３，y因子パタン;
  s母数[4]<-sx2-s母数[2]^2;                        #４，x誤差分散;
  s母数[5]<-sy2-s母数[3]^2;                        #５，y誤差分散;
  s母数[6]<-0;                                     #６，x->y係数;
  if (歪み) {s母数[7]<- sy3-((s母数[3]+s母数[6]*s母数[2])^3)*s母数[1]}
  #標本積率のセット
  s<-matrix(0,7,1); 
  s[1]<-sx2;  s[2]<-sxy; s[3]<-sy2;
  s[4]<-sx3; s[5]<-sx2y; s[6]<-sxy2; s[7]<-sy3;
  目的<-function(母数)
  {
    #シグマシータのセット
    sigthe<-matrix(0,7,1)
    sigthe[1]<-母数[2]^2+母数[4];
    sigthe[2]<-母数[2]*(母数[3]+母数[6]*母数[2])+母数[6]*母数[4];
    sigthe[3]<-((母数[3]+母数[6]*母数[2])^2)+(母数[6]^2)*母数[4]+母数[5]
    sigthe[4]<-(母数[2]^3)*母数[1];
    sigthe[5]<-(母数[2]^2)*(母数[3]+母数[6]*母数[2])*母数[1];
    sigthe[6]<-母数[2]*((母数[3]+母数[6]*母数[2])^2)*母数[1];
    if (歪み) {sigthe[7]<-((母数[3]+母数[6]*母数[2])^3)*母数[1]+母数[7]}
       else {sigthe[7]<-((母数[3]+母数[6]*母数[2])^3)*母数[1]}
    f<-N*( t(s-sigthe) %*% iw %*% (s-sigthe) )
    attr(f,'sigthe')<-sigthe
    return(f)
  }
  k<-nlm(目的,s母数,hessian=T)
  ff<-目的(k$estimate)
  #出力用行名・列名作成;
  q11<-c('基本情報')
  q12<-c('適合度','標本数','自由度','ｐ値','繰返し','Max勾配','コード')
  if (歪み) {q21<-c('f:3次','f->x係数','f->y係数','x誤差2次','y誤差2次','x->y係数','y誤差3次')}
     else  {q21<-c('f:3次','f->x係数','f->y係数','x誤差2次','y誤差2次','x->y係数')}
  q22<-c('推定値','標準誤差','95％下限','95％上限','ｚ値','分散','初期値','勾配')
  q41<-c('x分散','共分散','y分散','x3積率','x2y積率','xy2積率','y3積率')
  q42<-c('標本積率','制約積率','単純残差','標準誤差','標準残差','誤差分散','重み対角')
  q31<-c('x歪度','y歪度')
  q32<-c('標本値','制約値')
  q51<-c('xパタン','yパタン','x->y係数')
  q52<-c('標準解')

  #出力用行列生成;
  p1<-matrix(0,1,7);  rownames(p1) <-q11;   colnames(p1) <-q12; 
  p2<-matrix(0,q,8);  rownames(p2) <-q21;   colnames(p2) <-q22; 
  p3<-matrix(0,2,2);  rownames(p3) <-q31;   colnames(p3) <-q32; 
  p4<-matrix(0,7,7);  rownames(p4) <-q41;   colnames(p4) <-q42; 
  p5<-matrix(0,3,1);  rownames(p5) <-q51;   colnames(p5) <-q52; 
  rownames(iw) <-q41;   colnames(iw) <-q41;  

  #出力計算
  p1[,1]<-k$minimum;p1[,2]<-N;p1[,3]<-df;
  if (df==0) p1[,4]<-1.0
  else p1[,4]<-pchisq(k$minimum,df,lower.tail=F)
  p1[,5]<-k$iterations;
  p1[,6]<-max(abs(k$gradient));p1[,7]<-k$code;
  p6<-k$hessian;
  p7<-try(solve(p6))
  if (class(p7)=='try-error')
  {
    library(MASS);p7<-ginv(p6) 
  }
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
  p2[,7]<-s母数;
  p2[,8]<-k$gradient;
  p4[,1]<-s; p4[,2]<-attr(ff,'sigthe');  p4[,3]<-p4[,1]-p4[,2];
  p4[,6]<-diag(solve(iw))/N;   p4[,4]<-sqrt(p4[,6]);
  p4[,5]<-p4[,3]/p4[,4];  p4[,7]<-diag(iw); 

  p3[1,1]<-p4[4,1]/(sqrt(p4[1,1])^3)
  p3[1,2]<-p4[4,2]/(sqrt(p4[1,2])^3)
  p3[2,1]<-p4[7,1]/(sqrt(p4[3,1])^3)
  p3[2,2]<-p4[7,2]/(sqrt(p4[3,2])^3)
 
  p5[1,1]<-p2[2,1]/sqrt(p4[1,2])
  p5[2,1]<-p2[3,1]/sqrt(p4[3,2])
  p5[3,1]<-p2[6,1]*sqrt(p4[1,2])/sqrt(p4[3,2])

  #出力引渡し
  結果<-list()
  結果$基本情報    <- p1
  結果$推定値      <- p2
  結果$観測変数歪度<- p3
  結果$残差の情報  <- p4
  結果$標準化係数  <- p5
  結果$ヘシアン    <- p6
  結果$母数間共分散<- p7
  結果$母数間相関  <- p8
  結果$重み行列    <- iw
  return(結果)
}
