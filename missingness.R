#----missingness----#
rm(list = ls())
Sys.setlocale("LC_ALL", ".932")
library(dplyr)
library(janitor)
library(mice)
# load_data ----
d1912s <- read.csv(file = "data/201912scr.csv")
d1912m <- read.csv(file = "data/201912main.csv")
d1912f <- read.csv(file = "data/201912fol.csv")
#d1912 <- read.csv(file = "data/201912scr_main_fol.csv")
d1912 <- read.csv(file = "data/201912scr_main_fol_cleaned.csv")
d2007s <- read.csv(file = "data/202007scr.csv")
d2012s <- read.csv(file = "data/202012scr.csv")
d2104s <- read.csv(file = "data/202104scr.csv")
d2108s <- read.csv(file = "data/202108scr.csv")
# select_data ----
d1912 <- d1912[, c("SEX", "AGE", "sas", "siats", "time_phone", "cius", "igds", "audit", "cesd", "lsas", "stais", "stait", "oci", "asrs", "aq", "bigfive_openness", "bigfive_conscientiousness", "bigfive_extroversion", "bigfive_agreeableness", "bigfive_neuroticism", "bis_att", "bis_nonp", "bis_mot")]
# mssingness ----
## 201912 ----
table_date <- data.frame(
  Study_name = c("2019Dec_1", "2019Dec_2", "2019Dec_3", "2020July", "2020Dec", "2021Apr", "2021Aug")
  , n = c(length(d1912s$ANSWERDATE), length(d1912m$ANSWERDATE), length(d1912f$ANSWERDATE), length(d2007s$ANSWERDATE), length(d2012s$ANSWERDATE), length(d2104s$ANSWERDATE), length(d2108s$ANSWERDATE))
  , Start_date = c(min(d1912s$ANSWERDATE), min(d1912m$ANSWERDATE), min(d1912f$ANSWERDATE), min(d2007s$ANSWERDATE), min(d2012s$ANSWERDATE), min(d2104s$ANSWERDATE), min(d2108s$ANSWERDATE))
  , End_date =   c(max(d1912s$ANSWERDATE), max(d1912m$ANSWERDATE), max(d1912f$ANSWERDATE), max(d2007s$ANSWERDATE), max(d2012s$ANSWERDATE), max(d2104s$ANSWERDATE), max(d2108s$ANSWERDATE))
)
knitr::kable(table_date, caption = "Dates of study participation")

## sas-sv ----
count_sas <- length(d1912s$sas) - length(na.omit(d1912s$sas))
percentage_sas <- (length(d1912s$sas) - length(na.omit(d1912s$sas))) / length(d1912s$sas)
table_miss_sas <- data.frame(Measure = "SAS-SV", Count = count_sas, Percentage = c(percentage_sas))
knitr::kable(table_miss_sas, digits = 4, caption = "Missing values of SAS-SV")
### phone time ----
count_phone <- length(d1912$time_phone) - length(na.omit(d1912$time_phone))
percentage_phone <- (length(d1912$time_phone) - length(na.omit(d1912$time_phone))) / length(d1912$time_phone) * 100
table_miss_phone <- data.frame(Measure = "Smartphone use time", Count = count_phone, Percentage = c(percentage_phone))
knitr::kable(table_miss_phone, digits = 2, caption = "Proportion of participants who responded 'I do not know' on their smartphone use time")
## audit ----
count_audit <- length(d1912$audit) - length(na.omit(d1912$audit))
percentage_audit <- (length(d1912$audit) - length(na.omit(d1912$audit))) / length(d1912$audit)
table_miss_audit <- data.frame(Measure = "AUDIT", Count = count_audit, Percentage = c(percentage_audit))
knitr::kable(table_miss_audit, digits = 2, caption = "Proportion of participants who responded 'I do not know' on their smartphone use time")
## 202007-09 ----
length(df_teiki$time_phone) - length(na.omit(df_teiki$time_phone))
(length(df_teiki$time_phone) - length(na.omit(df_teiki$time_phone))) / length(df_teiki$time_phone)

# multiple_imputation ----
## 201912 ----
df_pred <- matrix(1:ncol(d1912), nrow = ncol(d1912), ncol = ncol(d1912))
colnames(df_pred) <- names(d1912)
rownames(df_pred) <- names(d1912)  
diag(df_pred) <- 0
df_pred[which(df_pred > 0)] <- 1
df_pred
w.meth <- make.method(d1912)
set.seed(20220104)
d_imp <- mice(data = d1912, method = w.meth, predictorMatrix = df_pred, m = 10, maxit = 10)
d1912 <- complete(d_imp, 5)
## 202007-09 ----
df_pred <- matrix(1:ncol(df_teiki), nrow = ncol(df_teiki), ncol = ncol(df_teiki))
colnames(df_pred) <- names(df_teiki)
rownames(df_pred) <- names(df_teiki)  
diag(df_pred) <- 0
df_pred[which(df_pred > 0)] <- 1
df_pred
w.meth <- make.method(df_teiki)
d_imp <- mice(data = df_teiki, method = w.meth, predictorMatrix = df_pred, m = 10, maxit = 10)
df <- complete(d_imp, 5)
# save ----
write.table(x = d1912, file = "data/201912scr_main_fol_imp.csv", sep = ",", quote = FALSE)
write.table(x = df, file = "data/teiki_imp.csv", sep = ",", quote = FALSE)
