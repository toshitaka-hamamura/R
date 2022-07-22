#######  第10章  ###########
#######################################################################
library(lavaan.survey)
#データの読み込み
mat <- read.csv("dat/mat.csv")
head(mat)

#抽出法の指定
svy.design <- svydesign(ids=~school + student, fpc=~M + N.m, 
weights=~w, data=mat)

library(lavaan)
#モデルの指定
model <-'test~1 + motive'

#lavaan の実行
lavaan.fit <- lavaan(model, data=mat, auto.var=TRUE,estimator="MLM")

survey.fit <- lavaan.survey(lavaan.fit=lavaan.fit, 
survey.design=svy.design)

#階層構造を考慮した場合
parameterEstimates(lavaan.fit)
#階層構造を無視した場合
parameterEstimates(survey.fit)
#######################################################################


