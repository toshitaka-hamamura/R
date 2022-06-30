#項目分析とテスト得点の換算表作成を同時に行う
#第１引数は， x は被験者かける項目の素点行列，行和がテスト得点となるデータ
#第２引数は，項目名の文字ベクトル

test.table<-function(検査データ,項目名)
{
   stopifnot(ncol(検査データ) > 1, nrow(検査データ) > 1)
   stopifnot(all(is.element(項目名,colnames(検査データ))))
   if (length(項目名)==1) {
      素点行列 <- matrix(検査データ[,項目名],nrow(検査データ))
   } else {
      素点行列 <- 検査データ[,項目名]
   }
   問題数 <- ncol(素点行列)
   受験者数 <- nrow(素点行列)
   通過率 <- apply(素点行列,2,'mean')
   テスト得点 <-  apply(素点行列,1,'sum')
   識別力 <- disc.co(素点行列)
   if (is.na(識別力[1])) {識別力<-1.0}
   項目間相関行列 <- cor(素点行列)
   信頼性<-round(問題数/(問題数-1)*(1-sum(apply(素点行列, 2, 
      var))/var(apply(素点行列, 1, sum))),2)
   最高点 <- round(max(テスト得点),0)
   最低点 <- round(min(テスト得点),0)
   平均点 <- round(mean(テスト得点),2)
   ＳＤ <- round(sd(テスト得点),2)
   分位数 <- quantile(テスト得点)
   要約 <- cbind(受験者数,問題数,最高点,最低点,平均点,ＳＤ,信頼性)
   項目情報<-round(rbind(識別力,通過率),2)
   if (ncol(項目情報)>1) {項目情報<-項目情報[,rev(order(識別力))]}
   rownames(項目情報)<-c('識別力','項目平均')
   得点 <- 最高点:最低点 
   標準得点 <- round((得点-平均点)/ＳＤ,3)
   偏差値 <- round(((得点-平均点)/ＳＤ)*10+50,1)
   得点表<-data.frame(得点=得点,標準得点=標準得点,偏差値=偏差値)
   点数<- as.numeric(names(rev(table(テスト得点))))
   度数<-rev(table(テスト得点))
   相対度数<-度数/受験者数
   逆累積相対度数<-cumsum(相対度数)
   累積相対度数<-rev(cumsum(rev(相対度数)))
   par(mfrow=c(2,1))
   cumcurve(テスト得点,'累積相対度数')
   hist(テスト得点,xlab="テスト得点")
#   barplot(rev(度数),xlab="テスト得点")
   par(mfrow=c(1,1))
   度数表<-data.frame(点数=点数,度数=度数,相対度数=round(相対度数,4),
      累積相対度数=round(累積相対度数,4),
      逆累積相対度数=round(逆累積相対度数,4))
   fit<-(list(テスト情報=要約,項目情報=項目情報,素点行列=素点行列,
      テスト得点=テスト得点,度数表=度数表,
      得点表=得点表,項目間相関行列=項目間相関行列,分位数=分位数))
   class(fit)<-'test.table'
   return(invisible(fit))
}

print.test.table <- function(x)
{
   print(x$テスト情報)
   print(x$分位数)
   print(x$項目情報)
}
