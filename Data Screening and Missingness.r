install.packages("psych")
install.packages("Rcpp")
install.packages("BaylorEdPsych")
install.packages("sfsmisc")
install.packages("gdata")
install.packages("mvnmle")
install.packages("MASS")
install.packages("Amelia")
install.packages("mvoutlier")
install.packages("GGally")
install.packages("ggplot2")
install.packages("car")
install.packages("mice")

require(psych)
require(Rcpp)
require(BaylorEdPsych)
require(sfsmisc)
require(gdata)
require(mvnmle)
require(MASS)
require(Amelia)
require(mvoutlier)
require(GGally)
require(ggplot2)
require(car)
require(mice)



x <- read.delim(file="E:/PSYC467/4.Data Screening/screen.dat", header=TRUE, sep="\t",row.names=1)
attach(x) #can now refer to variables by name
names(x) #prints variable names


##1a and 1b. Inspect univariate descriptive statistics ##
#evaluate each variable appropriate minimum and maximum values
#evaluate each variable for appropriate mean
#evalaute each variable for appropriate standard deviation
#evaluate each variable for skew and kurtosis
#if values in the n column are not all equal, then deal with missingness
describe(x)   #prints useful descriptive statistics using the psych package
#Function Values (in column order)
#   Item Name
#   Item Number
#   Number of valid cases (used for identifying whether a variable contains missing data
#   Mean
#   Standard deviation
#   Median
#   Trimmed mean (with trim defaulting to .1 - trim means by dropping the top and bottom trim fraction)
#       Useful for identifying outliers and skew - want mean and trimmed mean to be almost identical
#   Median Absolute Deviation (from the median)
#       The median of the absolute deviations of each score from the median
#       MAD = median(|X - median(X)|)
#       A robust statistic that is more resilient to outliers than the standard deviation
#       In SD, the distances from the mean are squared, so outliers are weighted more heavily
#       In MAD, only absolute values of deviations are used, so a few outliers are irrelevant.
#       Useful in checking for univariate outliers - want to see SD and MAD to be roughly similar
#   Minimum
#   Maximum
#   Range
#   Skew
#      Useful for checking univariate normality (measures the asymmetry of a distribution)
#      Want to see values close to 0
#      Positive skew indicates the right tail is longer and the mass of the distribution is concentrated on the left
#      Negative skew indicates the left tail is longer and the mass of the distribution is concentrated on the right 
#   Kurtosis
#      Useful for checking univariate normality (measures the peakedness of a distribution)
#      Want to see values between -3 and +3
#      Negative kurtosis indicates distribution is platykurtic
#      Positive kurtosis indidates distribution is leptokurtic
#   Standard error
#      Typically standard error of the mean (standard deviation of the sample means over all possible samples of a given size
#      Estimate of the standard deviation of the sample means, computed from the sample being analyzed at the time
#      SE = SD/sqrt(n) - as described by the Central Limit Theorem
#      Useful in determining confidence intervals



#1c. Univariate Outliers#
#Produces Frequency Distribution Tables for each variable in the dataset
#Most useful for identifying outliers of discrete variables (rather than continuous)
#Can also be used to assess univariate normality 
#Note: only values represented in the dataset are provided (i.e. a value with a frequency of 0 is not included in the table)
freqtab <- NULL
for(k in 1:ncol(x)){
freqtab[[k]] <- table(x[,k])}
names(freqtab) <- as.list(names(x))
freqtab
 

###2.  MISSINGNESS  ###
#MCAR - missing completely at random
#    The pattern of missingness if completely unpredictable
#    The best kind of missingness
#    Can be ignored if less than 5% of the variable is missing
#MAR - missing at random
#    The pattern of missingness if predictable from other variables in the data
#    Frequently treated as MCAR
#MNAR - missing not at random
#    Missingness is related to the variable itself
#    CANNOT be ignored
#Little's MCAR test to assess for missing completely at random for multivariate data with missing values
#    Uses the information you have to test for patterns in the missing data
MCAR <- LittleMCAR(x) #run Little's MCAR test and saves results to MCAR

MCAR$amount.missing   #if Percent Missing for any variable is > .05 , then need to determine whether data are MCAR
# if < .05, then can use listwise deletion and move ahead with analyses 

MCAR$p.value #if p > .05, then data are MCAR
#if data are MCAR, then listwise deletion can be used without substantially altering results
#if data are MAR or MNAR (or a lot of missing data that is MCAR), then multiple imputation is most viable solution
#Always repeat any analysis with and without missing data to compare results
MCAR$missing.patterns #prints the number of missingness patterns (creates datasets for each pattern of missingness)
md.pattern(x) #diplays missing-data patterns
MCAR$chi.square  #need to report chi-square, df, and p-value in APA write-up
MCAR$df

LWx <- MCAR$data$DataSet1 #complete dataset using Listwise deletion 
#used when assessing other assumptions


###3. Pairwise plots to check for nonlinearity and heteroscedasticity  ###
windows(record=TRUE)  # This line does not work, can't find the fucntion windows.
ggpairs(na.omit(x)) #produces bivariate scatterplots for all variable

#take a closer look at the bivariate scatterplots with outliers
n.plot(x$timedrs,x$atthouse,nam=case.names(x),cex=.8)
n.plot(x$attdrug,x$atthouse,nam=case.names(x),cex=.8)
#in both scatterplots, it appears that two cases have values on atthouse that are < 5 but it is hard to see which they are
#these cases can also be seen in the frequency table printed earlier for atthouse
#These values not clear outliers until we plotted in the bivariate scatterplots
subset(x,atthouse < 5)  #prints the cases that are < 5 on atthouse

#notifies R that emplmnt, mstatus, and race are categorical variables     
xf <- x 
xf$emplmnt <- as.factor(x$emplmnt)     
xf$mstatus <- as.factor(x$mstatus)  
xf$race <- as.factor(x$race)  
#Produce bivariate plots for all combinations of variables
#   Includes box-and-whisker plots, and histograms that can be used to inform about normality and outliters
#   Plots are color-coded based on value of each categorical variable separately
ggpairs(na.omit(xf),upper = list(continuous = "cor", combo = "box",discrete="ratio"), 
            lower=list(continuous= "points",combo="facethist"),
            color="emplmnt",
            title="Employment")                                    
ggpairs(na.omit(xf),upper = list(continuous = "cor", combo = "box",discrete="ratio"), 
            lower=list(continuous= "points",combo="facethist"),
            color="mstatus",
            title="Marital Status")
ggpairs(na.omit(xf),upper = list(continuous = "cor", combo = "box",discrete="ratio"), 
            lower=list(continuous= "points",combo="facethist"),
            color="race",
            title="Race")

### 4. Normality ###
#Evaluate skew and kurtosis from the Descriptive Statistics sections
#4a. normality plots
for(i in 1:ncol(x)){
windows()
hist(x[,i],xlab=names(x[i]),main=paste("Histogram of",names(x[i])))
windows()
qqnorm(x[,i],col="red",main=names(x[i]))
qqline(x[,i],lwd=2)
}

#4b. Transformation variables
#Box-Cox 
nnvarcol <- 1 #idenfity the column of the non-normal variable in the dataset
nnvar <- (x[,nnvarcol]) 
m <- lm((nnvar+min(na.omit(nnvar))+1)~1)
bc <- boxcox(m)
bc.opt <- bc$x[which.max(bc$y)]
bc.opt
nnvar.bc <- bcPower((nnvar+min(na.omit(nnvar))+1), bc.opt)
hist(nnvar.bc)
hist(x[,nnvarcol])
x <- cbind(x,nnvar.bc)
x <- rename.vars(x,names(x),c(as.list(names(x[1:(ncol(x)-1)])),paste(names(x[nnvarcol]),".bc",sep="")))

#Logarithmic Transformation 
lttimedrs <- log(x$timedrs+1)
hist(lttimedrs)
plot(density(lttimedrs),col=1)
lines(density(nnvar.bc),col=2)

#Normalized dataset
normX <- x[,2:ncol(x)]

### 5. Univariate and Multivariate Outliers ###
#Inspect histograms printed in normality section above
#Standardized all variables
zX <- scale(na.omit(x))
#Print cases with z-scores > +/- 3.29
extreme <- NULL
for(k in 1:ncol(zX)){
extreme[[k]] <- subset(zX,zX[,k] >= 3.29) }
names(extreme) <- as.list(names(x))
extreme

#Multivariate outliers
#mahalanobis distance
D2 <- mahalanobis(na.omit(normX),colMeans(na.omit(normX)),cov(na.omit(normX))) 

plot(density(D2, bw = 0.5),
     main=paste("Squared Mahalanobis distances, n=",nrow(na.omit(normX)),", p=",ncol(normX))) ; rug(D2)
qqplot(qchisq(ppoints(nrow(na.omit(normX))), df = ncol(normX)), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 * " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')
sort(D2[D2 >= qchisq(.99, df = ncol(normX)) ],decreasing=TRUE)


#Winsorizing outliers
colMeans(x,na.rm=TRUE)
winsor.means(x)
WinsorX <- winsor(x)

#6. Multicollinearity and Singularity
#Variables are multicolinear if correlation > .8
#Variables are singular if correlation is > .95
round(cor(x,y=NULL,use="pairwise.complete.obs"),3)

### Single Imputation (only appropriate when data are MCAR)
MeanX <- mice(normX,m=1,meth="mean") #Mean imputation
#NOTE: In order to create a new imputed dataset you need to set YOUR OWN file name
write.table(complete(MeanX),file="E:/PSYC467/4.Data Screening/Mean Inputation.dat", row.names=FALSE, sep="/t")
RegX <- mice(normX,m=1,meth="norm") #Regression Imputation
write.table(complete(RegX),file="E:/PSYC467/4.Data Screening/screen Regression Inputation.dat", row.names=FALSE, sep="/t")

### Multiple Imputation (
#Amelia runs the bootstrap EM algorithm to incomplete data and creates imputed datasets
MIx <- amelia(normX,m=10,noms=c("race","mstatus","emplmnt"))

#similar function, but uses original x dataset, but normalize "timedrs" using log option
#MIx <- amelia(x,m=10,noms=c("race","mstatus","emplmnt"),logs="timedrs")

summary(MIx)
write.amelia(MIx, separate=TRUE,file.stem="E:/PSYC467/4.Data Screening/Amelia",format="csv")
#Use the imputed datasets in subsequent analyses.  Analyze each dataset as dictated by your analytic plan
#Collect and take the average of the resulting parameters (specific example will be demonstrated using Multiple Regression in a subsequent lecture
