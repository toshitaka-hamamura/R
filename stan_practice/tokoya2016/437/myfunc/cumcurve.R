cumcurve<-function(x,ƒ‰ƒxƒ‹='')
{
   plot(as.numeric(rownames(table(x))),
      cumsum(table(x))/length(x),type='l',
      ylab='Šm—¦', xlab=ƒ‰ƒxƒ‹)
}