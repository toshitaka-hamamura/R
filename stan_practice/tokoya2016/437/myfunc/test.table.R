#���ڕ��͂ƃe�X�g���_�̊��Z�\�쐬�𓯎��ɍs��
#��P�����́C x �͔팱�҂����鍀�ڂ̑f�_�s��C�s�a���e�X�g���_�ƂȂ�f�[�^
#��Q�����́C���ږ��̕����x�N�g��

test.table<-function(�����f�[�^,���ږ�)
{
   stopifnot(ncol(�����f�[�^) > 1, nrow(�����f�[�^) > 1)
   stopifnot(all(is.element(���ږ�,colnames(�����f�[�^))))
   if (length(���ږ�)==1) {
      �f�_�s�� <- matrix(�����f�[�^[,���ږ�],nrow(�����f�[�^))
   } else {
      �f�_�s�� <- �����f�[�^[,���ږ�]
   }
   ��萔 <- ncol(�f�_�s��)
   �󌱎Ґ� <- nrow(�f�_�s��)
   �ʉߗ� <- apply(�f�_�s��,2,'mean')
   �e�X�g���_ <-  apply(�f�_�s��,1,'sum')
   ���ʗ� <- disc.co(�f�_�s��)
   if (is.na(���ʗ�[1])) {���ʗ�<-1.0}
   ���ڊԑ��֍s�� <- cor(�f�_�s��)
   �M����<-round(��萔/(��萔-1)*(1-sum(apply(�f�_�s��, 2, 
      var))/var(apply(�f�_�s��, 1, sum))),2)
   �ō��_ <- round(max(�e�X�g���_),0)
   �Œ�_ <- round(min(�e�X�g���_),0)
   ���ϓ_ <- round(mean(�e�X�g���_),2)
   �r�c <- round(sd(�e�X�g���_),2)
   ���ʐ� <- quantile(�e�X�g���_)
   �v�� <- cbind(�󌱎Ґ�,��萔,�ō��_,�Œ�_,���ϓ_,�r�c,�M����)
   ���ڏ��<-round(rbind(���ʗ�,�ʉߗ�),2)
   if (ncol(���ڏ��)>1) {���ڏ��<-���ڏ��[,rev(order(���ʗ�))]}
   rownames(���ڏ��)<-c('���ʗ�','���ڕ���')
   ���_ <- �ō��_:�Œ�_ 
   �W�����_ <- round((���_-���ϓ_)/�r�c,3)
   �΍��l <- round(((���_-���ϓ_)/�r�c)*10+50,1)
   ���_�\<-data.frame(���_=���_,�W�����_=�W�����_,�΍��l=�΍��l)
   �_��<- as.numeric(names(rev(table(�e�X�g���_))))
   �x��<-rev(table(�e�X�g���_))
   ���Γx��<-�x��/�󌱎Ґ�
   �t�ݐϑ��Γx��<-cumsum(���Γx��)
   �ݐϑ��Γx��<-rev(cumsum(rev(���Γx��)))
   par(mfrow=c(2,1))
   cumcurve(�e�X�g���_,'�ݐϑ��Γx��')
   hist(�e�X�g���_,xlab="�e�X�g���_")
#   barplot(rev(�x��),xlab="�e�X�g���_")
   par(mfrow=c(1,1))
   �x���\<-data.frame(�_��=�_��,�x��=�x��,���Γx��=round(���Γx��,4),
      �ݐϑ��Γx��=round(�ݐϑ��Γx��,4),
      �t�ݐϑ��Γx��=round(�t�ݐϑ��Γx��,4))
   fit<-(list(�e�X�g���=�v��,���ڏ��=���ڏ��,�f�_�s��=�f�_�s��,
      �e�X�g���_=�e�X�g���_,�x���\=�x���\,
      ���_�\=���_�\,���ڊԑ��֍s��=���ڊԑ��֍s��,���ʐ�=���ʐ�))
   class(fit)<-'test.table'
   return(invisible(fit))
}

print.test.table <- function(x)
{
   print(x$�e�X�g���)
   print(x$���ʐ�)
   print(x$���ڏ��)
}