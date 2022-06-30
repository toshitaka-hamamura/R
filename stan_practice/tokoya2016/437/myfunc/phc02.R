#差がある確率の行列
#ext 行に乱数、列に母数をもつ行列
#c   行の母数ー列の母数＞c の確率を与える
phc02<-function(ext,c=0, digits=5){
   J<-ncol(ext)
   pro_matrix<-matrix(0,J,J)
   for (i in 1:J){  for (j in 1:J){
       pro_matrix[i,j]<-mean(ext[,i]-ext[,j]>c)  }}
   round(pro_matrix,digits)
}
