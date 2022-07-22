#--------------------------------------------------
#   高次積率による単回帰分析を行うＲ関数
#   著者：豊田秀樹（早稲田大学）2006年6月13日 Ver.1.0
#--------------------------------------------------
#関数名:高次単回帰(v1,v2,emethod = c('GLS','ULS','SLS'),解析=F)
#　関数hiregを4回呼んで，高次単回帰のモデル比較を行い，分析結果を格納する．
#引数
#v1,v2：分析される２つの変数
#emethod = c('GLS','ULS','SLS')から選ぶ推定法
#解析：Tなら解析的な勾配とヘシアン，Fなら数値近似
#
#返り値
#v1v2T：v1からv2への誤差変数に歪みありの結果
#v1v2F：v1からv2への誤差変数に歪みなしの結果
#v2v1T：v2からv1への誤差変数に歪みありの結果
#v2v1F：v2からv1への誤差変数に歪みなしの結果
#
#結果の中身は関数hiregの返り値
#
#関数名:hireg(x,y,emethod='GLS',歪み=T,解析=F)
#　１次２次３次の積率を利用した単回帰分析
#引数　x:予測変数,　y:基準変数,　
#  emethod: 推定法　c('GLS','SLS','ULS')
#  歪み：誤差変数に歪みを仮定するか否か
#  解析：勾配とヘシアンを解析的に与えるか否か
#返り値
# $基本情報, 適合度,標本数,自由度,ｐ値,繰返し,Max勾配,コード
# $推定値, 推定値,標準誤差,95％下限,95％上限,ｚ値,分散,初期値,勾配
# $通常回帰   
# $残差の情報,標本積率,制約積率,単純残差,標準誤差,標準残差,誤差分散,重み対角
# $標準化係数, 標準回帰係数,x歪度,y歪度,e歪度
# $ヘシアン    
# $母数間共分散
# $母数間相関  
# $重み行列    

hireg<-function(x,y,emethod='GLS',歪み=T,解析=F)
{
  if (歪み) {q<-7;df<-2}     # q : 母数の数
  else      {q<-6;df<-3}     # df: 自由度
  #4次までの積率の計算;
  N<-length(x)
  mx<-mean(x); x<-x-mx; my<-mean(y); y<-y-my           #平均・偏差
  x2<-x*x; x3<-x*x2; x4<-x*x3; x5<-x*x4; x6<-x*x5      #累乗ベクトル;
  y2<-y*y; y3<-y*y2; y4<-y*y3; y5<-y*y4; y6<-y*y5 
  sx2<-mean(x2); sxy<-mean(x*y); sy2<-mean(y2)
  sx3<-mean(x3); sx2y<-mean(x2*y); sxy2<-mean(x*y2); sy3<-mean(y3)
  sx4<-mean(x4); sx3y<-mean(x3*y); sx2y2<-mean(x2*y2) 
  sxy3<-mean(x*y3); sy4<-mean(y4)
  #2変数x,yの３次までの積率の漸近共分散行列の逆行列を返す
  重み２変数平積<-function(x,y,emethod=c('GLS','ULS','SLS'))
  {
    if (emethod=='ULS') 
    {
      iw<-diag(9); return(iw) 
    }
    else
    {
      #重み行列の準備
      w<-matrix(0,9,9)
      w11<-matrix(0,2,2);w21<-matrix(0,3,2);w22<-matrix(0,3,3)
      w31<-matrix(0,4,2);w32<-matrix(0,4,3);w33<-matrix(0,4,4)
      #対角要素の代入
      w[1,1]<-w11[1,1]<-sx2;
      w[2,2]<-w11[2,2]<-sy2;
      w[3,3]<-w22[1,1]<-sx4 - sx2^2;
      w[4,4]<-w22[2,2]<-sx2y2 - sxy^2;
      w[5,5]<-w22[3,3]<-sy4 - sy2^2;
      w[6,6]<-w33[1,1]<-mean(x6) - 6*sx4*sx2 - sx3^2 + 9*(sx2^3);
      w[7,7]<-w33[2,2]<-mean(x4*y2) - 4*sx3y*sxy - 2*sx2y2*sx2 - sx2y^2 + 8*sx2*(sxy^2) + (sx2^2)*sy2;
      w[8,8]<-w33[3,3]<-mean(x2*y4) - 4*sxy3*sxy - 2*sx2y2*sy2 - sxy2^2 + 8*sy2*(sxy^2) + (sy2^2)*sx2;
      w[9,9]<-w33[4,4]<-mean(y6) - 6*sy4*sy2 - sy3^2 +  9*(sy2^3);
      #単純最小２乗法の重み
      if (emethod=='SLS')
      {
        iw<-solve(w); return(iw) 
      }
      #一般化最小２乗法の重み
      else
      {
        #ｗ１１;
        w11[2,1]<-sxy;
        w11[1,2]<-w11[2,1];
        #ｗ２１;
        w21[1,1]<-sx3;
        w21[2,1]<-sx2y;
        w21[3,1]<-sxy2;
        w21[1,2]<-sx2y;
        w21[2,2]<-sxy2;
        w21[3,2]<-sy3;
        #ｗ３１;
        w31[1,1]<-sx4 - 3*(sx2^2);
        w31[2,1]<-sx3y - 3*sx2*sxy;
        w31[3,1]<-sx2y2 - sx2*sy2 - 2*(sxy^2);
        w31[4,1]<-sxy3 - 3*sy2*sxy;
        w31[1,2]<-sx3y - 3*sx2*sxy;
        w31[2,2]<-sx2y2 - sx2*sy2 - 2*(sxy^2);
        w31[3,2]<-sxy3 - 3*sy2*sxy;
        w31[4,2]<-sy4 - 3*(sy2^2);
        #ｗ２２;
        w22[2,1]<-sx3y - sx2*sxy;
        w22[3,1]<-sx2y2 - sx2*sy2;
        w22[3,2]<-sxy3 - sxy*sy2;
        w22<-w22+t(w22)-diag(diag(w22));
        #ｗ３２;
        w32[1,1]<-mean(x5) - 4*sx3*sx2;
        w32[2,1]<-mean(x4*y) - 2*sx2y*sx2 - 2*sx3*sxy;
        w32[3,1]<-mean(x3*y2) - sxy2*sx2 - 2*sx2y*sxy - sx3*sy2;
        w32[4,1]<-mean(x2*y3) - sx2*sy3 - 3*sx2y*sy2;
        w32[1,2]<-mean(x4*y) - sx3*sxy - 3*sx2*sx2y;
        w32[2,2]<-mean(x3*y2) - sx2*sxy2 - 3*sx2y*sxy;
        w32[3,2]<-mean(x2*y3) - sy2*sx2y - 3*sxy2*sxy;
        w32[4,2]<-mean(x*y4) - sy3*sxy - 3*sy2*sxy2;
        w32[1,3]<-mean(x3*y2) - sy2*sx3 - 3*sxy2*sx2;
        w32[2,3]<-mean(x2*y3) - sx2y*sy2 - sy3*sx2 - 2*sxy2*sxy;
        w32[3,3]<-mean(x*y4) - 2*sxy2*sy2 - 2*sy3*sxy;
        w32[4,3]<-mean(y5) - 4*sy3*sy2;
        #ｗ３３;
        w33[2,1]<-mean(x5*y) - 4*sx2*sx3y - sx3*sx2y - 2*sx4*sxy + 9*(sx2^2)*sxy;
        w33[3,1]<-mean(x4*y2) - 3*sx2*sx2y2 - sx3*sxy2 - 2*sx3y*sxy - sx4*sy2 + 6*sx2*(sxy^2) + 3*(sx2^2)*sy2;
        w33[4,1]<-mean(x3*y3) - 3*sx3y*sy2 - sx3*sy3 - 3*sx2*sxy3 + 9*sx2*sy2*sxy;
        w33[3,2]<-mean(x3*y3) - sx2*sxy3 - 4*sx2y2*sxy - sy2*sx3y - sx2y*sxy2 + 5*sx2*sxy*sy2 + 4*(sxy^3);
        w33[4,2]<-mean(x2*y4) - 3*sy2*sx2y2 - sy3*sx2y - 2*sxy3*sxy - sx2*sy4 + 6*sy2*(sxy^2) + 3*(sy2^2)*sx2;
        w33[4,3]<-mean(x*y5) - 4*sy2*sxy3 - sy3*sxy2 - 2*sy4*sxy + 9*(sy2^2)*sxy;
        w33<-w33+t(w33)-diag(diag(w33));
        w<-rbind(cbind(w11,t(w21),t(w31)),cbind(w21,w22,t(w32)),cbind(w31,w32,w33))
        iw<-try(solve(w))
        if (class(iw)=='try-error')
        {
          library(MASS);iw<-ginv(w) 
        }
        return(iw)
      }
    }
  }
  iw<-重み２変数平積(x,y,emethod)
  #初期値
  s母数<-rep(0,q) 
  s母数[1]<-sxy/sx2;                               #１，a;
  s母数[2]<-my-(sxy/sx2)*mx;                       #２，b;
  s母数[3]<-mx;                                    #３，x平均;
  s母数[4]<-sx2;                                   #４，x分散;
  s母数[5]<-mean((y-(sxy/sx2)*x)^2);               #５，e分散;
  s母数[6]<-sx3;                                   #６，x3次積率;
  if (歪み) {
      s母数[7]<-mean((y-(sxy/sx2)*x)^3);           #７，e3次積率;
  }
  #標本積率のセット
  s<-matrix(0,9,1); 
  s[1]<-mx;  s[2]<-my;   s[3]<-sx2;  s[4]<-sxy; s[5]<-sy2;
  s[6]<-sx3; s[7]<-sx2y; s[8]<-sxy2; s[9]<-sy3;
  目的<-function(母数)
  {
    #シグマシータのセット
    sigthe<-matrix(0,9,1)
    sigthe[1]<-母数[3];
    sigthe[2]<-母数[1]*母数[3]+母数[2];
    sigthe[3]<-母数[4];
    sigthe[4]<-母数[1]*母数[4];
    sigthe[5]<-(母数[1]^2)*母数[4]+母数[5];
    sigthe[6]<-母数[6];
    sigthe[7]<-母数[1]*母数[6];
    sigthe[8]<-(母数[1]^2)*母数[6];
    if (歪み) {
           sigthe[9]<-(母数[1]^3)*母数[6]+母数[7]
    }else{ sigthe[9]<-(母数[1]^3)*母数[6] }
    f<-N*( t(s-sigthe) %*% iw %*% (s-sigthe) )
    if (解析) 
    {
      #ヤコビのセット
      yakobi=matrix(0,q,9);
      yakobi[1,2]<-母数[3];
      yakobi[1,4]<-母数[4];
      yakobi[1,5]<-2*母数[1]*母数[4];
      yakobi[1,7]<-母数[6];
      yakobi[1,8]<-2*母数[1]*母数[6];
      yakobi[1,9]<-3*(母数[1]^2)*母数[6];
      yakobi[2,2]<-1;
      yakobi[3,1]<-1;
      yakobi[3,2]<-母数[1];
      yakobi[4,3]<-1;
      yakobi[4,4]<-母数[1];
      yakobi[4,5]<-母数[1]^2;
      yakobi[5,5]<-1;
      yakobi[6,6]<-1;
      yakobi[6,7]<-母数[1];
      yakobi[6,8]<-母数[1]^2;
      yakobi[6,9]<-母数[1]^3;
      if (歪み) {yakobi[7,9]<-1}
      attr(f,'gradient')<-(-2*N)*(yakobi %*% iw %*% (s-sigthe))
      attr(f,'hessian')<-(yakobi %*% iw %*% t(yakobi))/(2*N)
    }
    attr(f,'sigthe')<-sigthe
    return(f)
  }
  k<-nlm(目的,s母数,hessian=T,check.analyticals=F)
  ff<-目的(k$estimate)
  #出力用行名・列名作成;
  q11<-c('基本情報')
  q12<-c('適合度','標本数','自由度','ｐ値','繰返し','Max勾配','コード')
  if (歪み){ q21<-c('傾きa','切片b','x平均','x分散','e分散','x3次','e3次')
  }else{ q21<-c('傾きa','切片b','x平均','x分散','e分散','x3次')}
  q22<-c('推定値','標準誤差','95％下限','95％上限','ｚ値','分散','初期値','勾配')
  q31<-c('傾きa','切片b')
  q32<-c('推定値','標準誤差','95％下限','95％上限','ｚ値')
  q41<-c('x平均','y平均','x分散','共分散','y分散','x3積率','x2y積率','xy2積率','y3積率')
  q42<-c('標本積率','制約積率','単純残差','標準誤差','標準残差','誤差分散','重み対角')
  q51<-c('標準回帰係数','x歪度','y歪度','e歪度')
  q52<-c('標本値','制約値')

  #出力用行列生成;
  p1<-matrix(0,1,7);  rownames(p1) <-q11;   colnames(p1) <-q12; 
  p2<-matrix(0,q,8);  rownames(p2) <-q21;   colnames(p2) <-q22; 
  p3<-matrix(0,2,5);  rownames(p3) <-q31;   colnames(p3) <-q32; 
  p4<-matrix(0,9,7);  rownames(p4) <-q41;   colnames(p4) <-q42; 
  p5<-matrix(0,4,2);  rownames(p5) <-q51;   colnames(p5) <-q52; 
  rownames(iw) <-q41;   colnames(iw) <-q41;  

  #出力計算
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
  p2[,7]<-s母数;
  p2[,8]<-k$gradient;
  p3[1,1]<-s母数[1];p3[2,1]<-s母数[2];
  p3[1,2]<-sqrt(s母数[5]/(N*s母数[4]));
  p3[2,2]<-sqrt(((1/N)+(s母数[3]^2/(N*s母数[4])))*s母数[5]);
  p3[,3]<-p3[,1]-(1.96*p3[,2]);
  p3[,4]<-p3[,1]+(1.96*p3[,2]);
  p3[,5]<-p3[,1]/p3[,2];
  p4[,1]<-s; p4[,2]<-attr(ff,'sigthe');  p4[,3]<-p4[,1]-p4[,2];
  p4[,6]<-diag(solve(iw))/N;   p4[,4]<-sqrt(p4[,6]);
  p4[,5]<-p4[,3]/p4[,4];  p4[,7]<-diag(iw); 
  p5[1,1]<-s[4]/sqrt(s[3]*s[5]); 
  p5[2,1]<-s[6]/(sqrt(s[3])^3);
  p5[3,1]<-s[9]/(sqrt(s[5])^3);
  p5[4,1]<-(mean((y-(sxy/sx2)*x)^3))/(sqrt(mean((y-(sxy/sx2)*x)^2))^3); 
  p5[1,2]<-p4[4,2]/sqrt(p4[3,2]*p4[5,2]);
  p5[2,2]<-p4[6,2]/(sqrt(p4[3,2])^3);
  p5[3,2]<-p4[9,2]/(sqrt(p4[5,2])^3);
  if (歪み){  p5[4,2]<-p2[7,1]/(sqrt(p2[5,1])^3)
  }else {p5[4,2]<-0}  ;

  #出力引渡し
  結果<-list()
  結果$基本情報    <- p1
  結果$推定値      <- p2
  結果$通常回帰    <- p3
  結果$残差の情報  <- p4
  結果$標準化係数  <- p5
  結果$ヘシアン    <- p6
  結果$母数間共分散<- p7
  結果$母数間相関  <- p8
  結果$重み行列    <- iw
  return(結果)
}

ADF3reg<-function(v1,v2,emethod='GLS',解析=F)
{
  stopifnot(emethod=='GLS'|emethod=='ULS'|emethod=='SLS') 
  stopifnot (length(v1)==length(v2))
  v1v2T<-hireg(v1,v2,emethod,歪み=TRUE,解析)
  v2v1T<-hireg(v2,v1,emethod,歪み=TRUE,解析)
  v1v2F<-hireg(v1,v2,emethod,歪み=FALSE,解析)
  v2v1F<-hireg(v2,v1,emethod,歪み=FALSE,解析)
  結果<-list(v1v2T,v2v1T,v1v2F,v2v1F)
  class(結果)<-'hireguni'
  return(結果)
}

print.hireguni<-function(x)
{
y<-rbind(x[[1]]$基本情報,x[[2]]$基本情報,x[[3]]$基本情報,x[[4]]$基本情報)
rownames(y) <-c('v1からv2へ誤差歪みあり','v2からv1へ誤差歪みあり',
                'v1からv2へ誤差歪みなし','v2からv1へ誤差歪みなし')
colnames(y) <-c('適合度','標本数','自由度','ｐ値','繰返し','Max勾配','コード')
print(round(y,3))
}



