library(lavaan)
library(semTools)
library(Amelia)

#®Sf[^ÌªÍ
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
fit.complete <- sem(HS.model, data=HolzingerSwineford1939)
summary(fit.complete)

#MCARªf[^ðLDÅªÍ
HS1939.MCAR <- read.csv("dat/HS1939MCAR.csv")
fit.MCAR.LW <- sem(HS.model, data=HS1939.MCAR)
summary(fit.MCAR.LW)

#MARªf[^ðLDÅªÍ
HS1939.MAR1 <- read.csv("dat/HS1939MAR1.csv")
fit.MAR1.LW <- sem(HS.model, data=HS1939.MAR1)
summary(fit.MAR1.LW)

#MARªf[^ðFIMLÅªÍ
fit.MAR1.FIML <- sem(HS.model, data=HS1939.MAR1, missing="fiml")
summary(fit.MAR1.FIML)

#MARªf[^ðMIÅªÍ
fit.MAR1.MI <- runMI(model=HS.model, data=HS1939.MAR1,
                     fun="sem", m=20, seed=6987)
summary(fit.MAR1.MI)
inspect(fit.MAR1.MI, "fit")
inspect(fit.MAR1.MI, "impute")


#âÏ³µÅFIML
HS1939.MAR2 <- read.csv("dat/HS1939MAR2.csv")
fit.MAR2.FIML <- sem(HS.model, data=HS1939.MAR2[,-c(10:12)],
                     missing="fiml")
summary(fit.MAR2.FIML)

#âÏðpµÄFIML
fit.MAR2.auxFI <- auxiliary(model=HS.model, data=HS1939.MAR2,
                            fun="sem", aux=c("sex", "ageyr", "agemo"))
summary(fit.MAR2.auxFI)


#âÏ³µÅMI
fit.MAR2.MI <- runMI(model=HS.model, data=HS1939.MAR2[,-c(10:12)],
                     m=20, seed=157, fun="sem")
summary(fit.MAR2.MI)

#âÏðpµÄMI
fit.MAR2.auxMI <- runMI(model=HS.model, data=HS1939.MAR2,
                        fun="sem", m=20, seed=97845)
summary(fit.MAR2.auxMI)

