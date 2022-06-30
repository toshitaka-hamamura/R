#--------------------------------------------------
#   高次積率による2変数分析を行うＲ関数
#   著者：豊田秀樹（早稲田大学）2006年9月10日 Ver.2.0
#--------------------------------------------------
#関数名:高次2変数(v1,v2,emethod = c('GLS','ULS','SLS'))
#それぞれのモデルの分析を司る５つの外部の関数
#　関数hireg2, 単回帰4種
#　関数hifac2, 1因子4種
#　関数hifac22,2因子4種
#　関数norec2, 双方向3種
#　関数regfac, 回帰＋因子分析4種
#を呼んで，合計19種類の高次2変数分析を行う．
#引数
#v1,v2：分析される２つの変数
#emethod = c('GLS','ULS','SLS')から選ぶ推定法
#
#返り値
#v1v2T：v1からv2への誤差変数に歪みありの結果
#v1v2F：v1からv2への誤差変数に歪みなしの結果
#v2v1T：v2からv1への誤差変数に歪みありの結果
#v2v1F：v2からv1への誤差変数に歪みなしの結果
#f1T2T：共通変動モデル(誤差非対称)
#f1F2T：共通変動モデル(v1の誤差対称)
#f1T2F：共通変動モデル(v2の誤差対称)
#f1F2F：共通変動モデル(誤差対称)
#f22FF：2因子    (eなしｇ対称)
#f22TF：2因子    (eありｇ対称)
#f22FT：2因子    (eなしｇ歪み)
#f22TT：2因子    (eありｇ歪み)
#n1T2T：双方向モデル(誤差非対称)
#n1F2T：双方向モデル(v1の誤差対称)
#n1T2F：双方向モデル(v2の誤差対称)
#rf12T：因子回帰v1→v2誤差歪み
#rf21T：因子回帰v2→v1誤差歪み
#rf12F：因子回帰v1→v2誤差対称
#rf21F：因子回帰v2→v1誤差対称
#
#５つの外部ファイルの関数のうちhireg2を例にあげて解説する
#関数名:hireg2(x,y,emethod='GLS',歪み=T)
#　２次３次の積率を利用した単回帰分析(１次は使用しない)
#引数　x:予測変数,　y:基準変数,　
#  emethod: 推定法　c('GLS','SLS','ULS')
#  歪み：誤差変数に歪みを仮定するか否か
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
#
#
#このファイルには関数が３つある
#
#関数名:高次2変数<-function(v1,v2,emethod='GLS')最上位の関数，上で説明済み
#
#関数名:重み２変数積率<-function(x,y,emethod=c('GLS','ULS','SLS'))
#  推定量の計算のための重み行列（GLS変数xyの2次と3次の積率の共分散行列
#  の逆行列)を返す
#
#関数名：print.hireguni<-function(x)
#　class　'hireguni'　の印刷のための関数


重み２変数積率<-function(x,y,emethod=c('GLS','ULS','SLS'))
{
  #4次までの積率の計算;
  N<-length(x)
  mx<-mean(x); x<-x-mx; my<-mean(y); y<-y-my           #平均・偏差
  x2<-x*x; x3<-x*x2; x4<-x*x3; x5<-x*x4; x6<-x*x5      #累乗ベクトル;
  y2<-y*y; y3<-y*y2; y4<-y*y3; y5<-y*y4; y6<-y*y5 
  sx2<-mean(x2); sxy<-mean(x*y); sy2<-mean(y2)
  sx3<-mean(x3); sx2y<-mean(x2*y); sxy2<-mean(x*y2); sy3<-mean(y3)
  sx4<-mean(x4); sx3y<-mean(x3*y); sx2y2<-mean(x2*y2) 
  sxy3<-mean(x*y3); sy4<-mean(y4)
  if (emethod=='ULS') 
  {
    iw<-diag(7); return(iw) 
  }
  else
  {
    #重み行列の準備
    w<-matrix(0,7,7)
    w22<-matrix(0,3,3)
    w32<-matrix(0,4,3);w33<-matrix(0,4,4)
    #対角要素の代入
    w[1,1]<-w22[1,1]<-sx4 - sx2^2;
    w[2,2]<-w22[2,2]<-sx2y2 - sxy^2;
    w[3,3]<-w22[3,3]<-sy4 - sy2^2;
    w[4,4]<-w33[1,1]<-mean(x6) - 6*sx4*sx2 - sx3^2 + 9*(sx2^3);
    w[5,5]<-w33[2,2]<-mean(x4*y2) - 4*sx3y*sxy - 2*sx2y2*sx2 - sx2y^2 + 8*sx2*(sxy^2) + (sx2^2)*sy2;
    w[6,6]<-w33[3,3]<-mean(x2*y4) - 4*sxy3*sxy - 2*sx2y2*sy2 - sxy2^2 + 8*sy2*(sxy^2) + (sy2^2)*sx2;
    w[7,7]<-w33[4,4]<-mean(y6) - 6*sy4*sy2 - sy3^2 +  9*(sy2^3);
    #単純最小２乗法の重み
    if (emethod=='SLS')
    {
      iw<-solve(w); return(iw) 
    }
    #一般化最小２乗法の重み
    else
    {
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
      w<-rbind(cbind(w22,t(w32)),cbind(w32,w33))
      iw<-try(solve(w))
      if (class(iw)=='try-error')
      {
        library(MASS);iw<-ginv(w) 
      }
      return(iw)
    }
  }
}

source('hireg2.R')
source('hifac2.R')
source('hifac22.R')
source('norec2.R')
source('regfac.R')

# 高次2変数<-function(v1,v2,emethod='GLS')
ADF3var2<-function(v1,v2,emethod='GLS')
{
  stopifnot(emethod=='GLS'|emethod=='ULS'|emethod=='SLS') 
  stopifnot (length(v1)==length(v2))
  v1v2T<-hireg2(v1,v2,emethod,歪み=TRUE)
  v2v1T<-hireg2(v2,v1,emethod,歪み=TRUE)
  v1v2F<-hireg2(v1,v2,emethod,歪み=FALSE)
  v2v1F<-hireg2(v2,v1,emethod,歪み=FALSE)

  f1T2T<-hifac2(v1,v2,emethod,歪み1=TRUE,歪み2=TRUE)
  f1F2T<-hifac2(v1,v2,emethod,歪み1=FALSE,歪み2=TRUE)
  f1T2F<-hifac2(v1,v2,emethod,歪み1=TRUE,歪み2=FALSE)
  f1F2F<-hifac2(v1,v2,emethod,歪み1=FALSE,歪み2=FALSE)

  f22FF<-hifac22(v1,v2,emethod,誤差=FALSE,因子=FALSE)
  f22TF<-hifac22(v1,v2,emethod,誤差=TRUE,因子=FALSE)
  f22FT<-hifac22(v1,v2,emethod,誤差=FALSE,因子=TRUE)
  f22TT<-hifac22(v1,v2,emethod,誤差=TRUE,因子=TRUE)

  n1T2T<-norec2(v1,v2,emethod,歪み1=TRUE,歪み2=TRUE)
  n1F2T<-norec2(v1,v2,emethod,歪み1=FALSE,歪み2=TRUE)
  n1T2F<-norec2(v1,v2,emethod,歪み1=TRUE,歪み2=FALSE)

  rf12T<-regfac(v1,v2,emethod,歪み=TRUE)
  rf21T<-regfac(v2,v1,emethod,歪み=TRUE)
  rf12F<-regfac(v1,v2,emethod,歪み=FALSE)
  rf21F<-regfac(v2,v1,emethod,歪み=FALSE)


  結果<-list(v1v2T,v2v1T,v1v2F,v2v1F,f1T2T,f1F2T,f1T2F,f1F2F,
             f22FF,f22TF,f22FT,f22TT,n1T2T,n1F2T,n1T2F,
             rf12T,rf21T,rf12F,rf21F
             )
  class(結果)<-'hireguni'
  return(結果)
}

print.hireguni<-function(x)
{
y<-rbind(x[[1]]$基本情報,x[[2]]$基本情報,x[[3]]$基本情報,x[[4]]$基本情報,
   x[[5]]$基本情報,x[[6]]$基本情報,x[[7]]$基本情報,x[[8]]$基本情報,
   x[[9]]$基本情報,x[[10]]$基本情報,x[[11]]$基本情報,x[[12]]$基本情報,
   x[[13]]$基本情報,x[[14]]$基本情報,x[[15]]$基本情報,
   x[[16]]$基本情報,x[[17]]$基本情報,x[[18]]$基本情報,x[[19]]$基本情報)
rownames(y) <-c(
    ' 1 A1 単回帰v1→v2 (誤差歪み)',
    ' 2 A2 単回帰v2→v1 (誤差歪み)',
    ' 3 A3 単回帰v1→v2 (誤差対称)',
    ' 4 A4 単回帰v2→v1 (誤差対称)',
    ' 5 B1 1因子共通変動 (誤差歪み)',
    ' 6 B2 1因子共通変動 (v1誤差対称)',
    ' 7 B3 1因子共通変動 (v2誤差対称)',
    ' 8 B4 1因子共通変動 (v1v2誤差対称)',
    ' 9 C1 2因子共通変動 (eなしf2対称)',
    '10 C2 2因子共通変動 (eありf2対称)',
    '11 C3 2因子共通変動 (eなしf2歪み)',
    '12 C4 2因子共通変動 (eありf2歪み)',
    '13 D1 双方向 (誤差歪み)',
    '14 D2 双方向 (v1誤差対称)',
    '15 D3 双方向 (v2誤差対称)',
    '16 E1 因子＋単回帰v1→v2 (誤差歪み)',
    '17 E2 因子＋単回帰v2→v1 (誤差歪み)',
    '18 E3 因子＋単回帰v1→v2 (誤差対称)',
    '19 E4 因子＋単回帰v2→v1 (誤差対称)'
     )
# rownames(y) <-c(
    # '１ A1 単回帰v1→v2  誤差歪み',
    # '２ A2 単回帰v2→v1  誤差歪み',
    # '３ A3 単回帰v1→v2  誤差対称',
    # '４ A4 単回帰v2→v1  誤差対称',
    # '５ B1 共通変動  (誤差非対称)',
    # '６ B2 共通変動  (v1誤差対称)',
    # '７ B3 共通変動  (v2誤差対称)',
    # '８ B4 共通変動  (12誤差対称)',
    # '９ C1 2因子    (eなしｇ対称)',
    # '10 C2 2因子    (eありｇ対称)',
    # '11 C3 2因子    (eなしｇ歪み)',
    # '12 C4 2因子    (eありｇ歪み)',
    # '13 D1 双方因果  (誤差非対称)',
    # '14 D2 双方因果  (v1誤差対称)',
    # '15 D3 双方因果  (v2誤差対称)',
    # '16 E1 因子回帰v1→v2誤差歪み',
    # '17 E2 因子回帰v2→v1誤差歪み',
    # '18 E3 因子回帰v1→v2誤差対称',
    # '19 E4 因子回帰v2→v1誤差対称',
     # )
colnames(y) <-c('適合度','標本数','自由度','ｐ値','繰返し','Max勾配','コード')
print(round(y,3))
}



