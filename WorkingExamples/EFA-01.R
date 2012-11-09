##Factor analysis in R
## Alexander M. Schoemann
## 10/12/2011

## There are two fuctions to perform exploratory factor analysis in R: factanal (in the stats package) and fa (in the psych package)
##For very simple EFAs fatanal works fine, but fa is much more flexible with many more options


##Create some data to anlyze with different methods
##Note:
v1 <- rep(c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6),5)
v2 <- rep(c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5),5)
v3 <- rep(c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6),5)
v4 <- rep(c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4),5)
v5 <- rep(c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5),5)
v6 <- rep(c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4),5)
m1 <- cbind(v1,v2,v3,v4,v5,v6)

##factanal function
##Uses maximum liklihood (ML) factor analysis
##ML factor analysis assumes multivariate normality among indicators
##factanal only provides chisquare test of model fit
##No summary() or plot() methods available

##Run a 1, 2, and 3 factor model
##The first argument is the data, the second arguement is the number of factors to extract
##Defaults to varimax (orthogonal rotation)! Do not use this!
model1<-factanal(m1,1)
model2<-factanal(m1,2)
model3<-factanal(m1,3)

##Run a 1, 2, and 3 factor model with a promax (oblique) rotation
##MANY other rotations available in the GPArotation package. These rotations can be used with factanal
model1<-factanal(m1,1,rotation='promax')
model2<-factanal(m1,2,rotation='promax')
model3<-factanal(m1,3,rotation='promax')

##If you want to test the difference between model 2 and model3, extract the chisquare statistic and df from each model and compare
chisq.diff<-model1$STATISTIC-model2$STATISTIC
df.diff<-model1$dof-model2$dof
1-pchisq(chisq.diff,df.diff)

##############################################################################################
##fa function

##VERY flexible
##Uses OLS, WLS or ML for factor analysis
##Provides many indexs of model fit (chisquare RMSEA, TLI)
##Extensive output
##summary() only returns model fit information 
##fa also defaults to orthogonal rotation.
##Need to include rotate="promax" for oblque rotation (many other rotations available)
##fm option sets method of factor analysis
library(psych)

##OLS factor analysis
model1<-fa(m1,1,rotate='promax',fm='minres')
model2<-fa(m1,2,rotate='promax',fm='minres')
model3<-fa(m1,3,rotate='promax',fm='minres')

##ML factor analysis
model1<-fa(m1,1,rotate='promax',fm='ml')
model2<-fa(m1,2,rotate='promax',fm='ml')
model3<-fa(m1,3,rotate='promax',fm='ml')

##WLS factor analysis
model1<-fa(m1,1,rotate='promax',fm='wls')
model2<-fa(m1,2,rotate='promax',fm='wls')
model3<-fa(m1,3,rotate='promax',fm='wls')

##n.iter will perform the factor analysis on n bootstrap samples and return bootrap confidence intervals
model3.boot<-fa(m1,3,rotate='promax',fm='minres',n.iter=100)

##The function VSS.scree in the psych package will produce a scree plot directly from a covariance or data matrix
VSS.scree(m1)

##Parallel analysis. How do the eigne values from our data compare to eigen values from random data
##n.iter is the number of random data sets to average over

fa.parallel(m1, fm='minres', fa='fa', n.iter=50)
