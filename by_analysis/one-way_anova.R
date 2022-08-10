library(compute.es)
library(car)
library(ggplot2)
library(multcomp)
library(pastecs)

#explore data 
by(stai_sum2,condition,stat.desc)
by(drink_most_total_quant2,condition,stat.desc)

#Levene's test
leveneTest(df$k6_sum1_sqrt,df$condition,center = median)
leveneTest(df$cesd_sum1_sqrt,df$condition,center = median)
leveneTest(df$stai_sum1,df$condition,center = median)
leveneTest(df$who5_sum1_sqrt,df$condition,center = median)
leveneTest(df$nmr_sum1,df$condition,center = median)
leveneTest(df_typdr$drink_typical_total_quant1,df_typdr$condition,center = median)
leveneTest(df_typdr$drink_typical_total_quant1_sqrt,df_typdr$condition,center = median)
leveneTest(df_typdr$drink_typical_total_quant1_inv,df_typdr$condition,center = median)
leveneTest(df_typdr$drink_most_total_quant1,df_typdr$condition,center = median)
leveneTest(df_typdr$drink_most_total_quant1_sqrt,df_typdr$condition,center = median)

#anova with planned contrast
contrast1<-c(-2,1,1)
contrast2<-c(0,-1,1)
contrasts(df$condition)<-cbind(contrast1,contrast2)
df$condition
cesdModel<-lm(df$cesd_sum2_sqrt~df$condition,data=df)#depression
summary.lm(cesdModel)
staiModel<-lm(df$stai_sum2~df$condition,data=df)#anxiety
summary.lm(staiModel)
drtpModel<-lm(df_typdr$drink_typical_total_quant2_sqrt~df_typdr$condition,data=df)
summary.lm(drtpModel)
drmsModel<-lm(df_typdr$drink_most_total_quant2_sqrt~df_typdr$condition,data=df)
summary.lm(drmsModel)

#post hoc tets
pairwise.t.test(df$cesd_sum2_sqrt,df$condition,p.adjustment.method="bonferroni")
pairwise.t.test(df$cesd_sum2_sqrt,df$condition,p.adjustment.method="BH")
pairwise.t.test(df$stai_sum2,df$condition,p.adjustment.method="bonferroni")
pairwise.t.test(df$stai_sum2,df$condition,p.adjustment.method="BH")
pairwise.t.test(df_typdr$drink_typical_total_quant2_sqrt,df_typdr$condition,p.adjustment.method="bonferroni")
pairwise.t.test(df_typdr$drink_typical_total_quant2_sqrt,df_typdr$condition,p.adjustment.method="BH")
pairwise.t.test(df_typdr$drink_most_total_quant2_sqrt,df_typdr$condition,p.adjustment.method="bonferroni")
pairwise.t.test(df_typdr$drink_most_total_quant2_sqrt,df_typdr$condition,p.adjustment.method="BH")
