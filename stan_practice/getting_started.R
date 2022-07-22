#getting started
rm(list = ls())
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016")
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch7")
setwd("C:/Users/toshi/OneDrive/R/stan_practice/matsuura2016/ch6/exercise")
getwd()
library(rstan)
library(ggplot2)


remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
install.packages("pkgbuild")

pkgbuild::has_build_tools(debug = TRUE)

