#http://rstudio-pubs-static.s3.amazonaws.com/3255_664d1bf83ad44b808fec0ad38a1f89ad.html
library(ggplot2)

example <- read.csv("C:/Users/toshi/Dropbox/2.Psychological Research/Statistics/example.csv",header = TRUE,sep = ",",fileEncoding = "latin1")
example <- read_csv("C:/Users/toshi/Dropbox/2.Psychological Research/Statistics/example.csv")
#step3
long$time<-factor(long$time,labels = c("pre-test","post-test"))

ggplot(data = long, aes(x = time, y = stai, color = factor(condition))) + 
  geom_line(linetype = 2) + 
  geom_point() + 
  ylim(0, 20) + 
  xlab("Time") + 
  ylab("Anxiety") + 
  scale_color_manual(values = c("black", "blue"), guide = FALSE) + 
  theme_bw()

?ggplot()

ggplot(long, aes(x = condition, y = stai))
