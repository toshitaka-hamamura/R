#Q1
rm(list = ls())
d <- t(matrix(
  c(2, 2,
    3, 3,
    3, 5,
    4, 6,
    5, 3,
    6, 4,
    6, 6,
    6, 7,
    7, 8,
    8, 6),
    nrow = 2, ncol = 10
  ))
d <- as.data.frame(d)
colnames(d) <- c('x', 'y')


sum((d$x -5)*(d$y -5))
d$x*d$y-5*d$x-5*d$y+25
