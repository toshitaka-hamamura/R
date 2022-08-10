rm(list=ls())
#independent-samples t-test
#https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha#

t.test2 <- function(n1,m1,s1,n2,m2,s2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}
t.test2(492,2.35,1.69,185,1.17,1.39)#study 3: comparison of alcohol dependence with Sakurai (1998)
t.test2(500,9.75,2.55,143,8.26,3.51)#study 3: comparison of AE psychological with Sakurai (1998)
t.test2(500,3.82,1.13,135,4.62,1.20)#study 3: comparison of AE psychological with Sakurai (1998)
t.test2(500,3.36,1.06,135,2.80,1.09)#study 3: comparison of AE psychological with Sakurai (1998)
t.test2(500,2.65,1.01,135,2.94,1.19)#study 3: comparison of AE psychological with Sakurai (1998)
t.test2(500,2.08,1.03,135,1.83,0.86)#study 3: comparison of AE psychological with Sakurai (1998)
t.test2(477,126.11,19.69,106,137.64,16.70)#study 3: comparison of NMRE with Konno and Mearns (2013)

t.test2(500,126.70,19.84,191,119.51,19.45)#study 3: comparison of NMRE with Hamamura et al (2017)

t.test2(246,23.66,10.32,246,24.31,9.54)#study 3: comparison of relationship-related pos.
t.test2(246,26.41, 9.53,246,26.33,9.25)#study 3: comparison of alcohol-related pos.

t.test2(2436,1750, 89.93,3311,1612,69.05)#study 3: comparison of alcohol-related pos.
