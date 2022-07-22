#aがbよりc大きい(小さい)というphc曲線の描画
#seq01 横軸(差の基準点 c )を等差数列で表現する。たとえばseq01=seq(0,1.2,0.05)
#a, b,  事後分布を近似した乱数ベクトル、a - b >cまたはa - b <cのphcを求める
#aだけを扱いたい場合は b=0 とすればよい。
#cc="gtc"の場合はa - b >cであり、"rope"の場合はabs(a-b)<cであり、
#              それ以外は("ltc"が想定されるが)a - b <c
#byoga="yes"の場合はphc曲線を描く、それ以外の場合はphcを出力する
#xlab="",ylab="",cex.lab=2.2,cex.axis=1.5,lwd=1.5
#リターンはphcベクトル
phc01<-function(seq01, a, b=0
, cc="gtc", byoga="yes", dedits=2,
                 xlab="" ,ylab="",cex.lab=2.2, cex.axis=1.5){
  av<-numeric(length(seq01))
  j<-0
  if (cc == "gtc") {
     for(c in seq01){ j<-j+1; av[j]<-mean(a-b>c);}
  }else if(cc == "rope"){
     for(c in seq01){ j<-j+1; av[j]<-mean(abs(a-b)<c);}
  }else{
     for(c in seq01){ j<-j+1; av[j]<-mean(a-b<c);}
  }
  names(av)<-seq01
  if (byoga=="yes") {
     plot(seq01,av,lwd=2,type="l",ylab="",ylim=c(0,1),
       xlab=xlab,xlim=range(seq01),cex.lab=cex.lab,cex.axis=cex.axis)
     grid(lwd=1.5)
     return(invisible(round(av,dedits)))
  }
  return(round(av,dedits))
}
