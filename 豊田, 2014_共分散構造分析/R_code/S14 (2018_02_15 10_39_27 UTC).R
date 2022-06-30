library(lavaan)

#分析の実行
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
fit <- sem(HS.model, data=HolzingerSwineford1939)

#通常の出力
summary(fit)

#適合度指標を追加した出力
summary(fit, fit.measures=TRUE)

#lavaanで計算できる全ての適合度指標の出力
fitMeasures(fit, fit.measures="all")

#尤度比検定
HS.model2 <- ' visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9
               x7 ~~ x8 '
fit2 <- sem(HS.model2, data=HolzingerSwineford1939)
anova(fit, fit2)

