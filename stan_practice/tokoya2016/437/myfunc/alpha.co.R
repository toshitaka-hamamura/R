#α係数を計算する関数,
#引数 x は被験者かける項目の素点行列，行和がテスト得点となるデータ
alpha.co <- function(x){
   m <- ncol(x)
   alpha.co<-(m/(m-1))*(1-(sum(apply(x, 2,var))/var(apply(x, 1, sum))))
   return(round(alpha.co,3))
}
