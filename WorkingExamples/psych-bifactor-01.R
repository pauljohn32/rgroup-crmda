## Paul Johnson <pauljohn@ku.edu>
## 2011-10-14

## Aim is to learn to generate "bi-factor" structured indicators
## for a cfa analysis and then simulate repeatedly.

## As it stands now, I believe sim.hierarchical is generating
## the data correctly. Still wrestling to get the lavaan
## syntax to work properly (and match an equivalent MPlus specification).

set.seed(12345)

## First, set the sample size.
Nsample <- 500

## Using the psych package by William Revelle.

## In this code, we explore sim.hierarchical to generate data for
## "bi.factor" analysis

library(psych)

## Student wants to draw samples from a
##  bi-factor model, with loadings like this

##  0.4   0.2  0.0
##  0.4   0.4  0.0
##  0.5   0.4  0.0
##  0.3   0.4  0.0
##  0.5   0.0  0.3
##  0.4   0.0  0.5
##  0.2   0.0  0.3
##  0.5   0.0  0.4

## Call those factors f1, f2, f3
## f1 applies to all of the rows, f2 and f3 apply to particular
## sets of rows. Assume f1, f2, and f3
## are orthogonal. 


myf <- matrix( c( 0.4,0.4,0.5,0.3,0.5,0.4,0.2,0.5,
                 0.7,0.6,0.8,0.6,rep(0,4),
                 rep(0,4), 0.7,0.7,0.5,0.7), ncol=3)


###Caution: this gives data that leads to unstable cfa models
#myf <- matrix( c( 0.4,0.4,0.5,0.3,0.5,0.4,0.2,0.5,
#                 0.2,0.4,0.4,0.4,rep(0,4),
#                 rep(0,4), 0.3,0.5,0.3,0.50), ncol=3)



### We want the three factors are all completely separate
### from sim.hierarical's group loading, "gload"
myg <- c(0,0,0)

## Grab the data.
sim2 <- sim.hierarchical(gload=myg, fload=myf, n= Nsample, raw=T)
mydata <- as.data.frame(sim2$observed)

# write.table(mydata, file="bifactor.txt", row.names=F)

## the omega graph shows that the "3 factor bi.factor" model
## can be re-done as a general factor with 2 special factors.

omega(sim2$model)

##I can't understand this at all.
omega(sim2$model, sl=F)

## Analyze the data with CFA to see if we are getting what
## we ask for

## I think lavaan's method is more understandable than sem.

sim2cov <- cov(sim2$observed)

#analyze the "true" model
## mycov <- sim2$model

#Or pretend the observed correlation matrix is covar
#myrcov <- sim2$r

#######
### retrieve saved data that can be fit successfully, if you need
### a happy thought
## mydata <- read.table("http://pj.freefaculty.org/R/sem-testing/psych-bifactor.txt",header=TRUE)


library(lavaan)
mymod  <- '
             f1 =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8
             f2 =~ V1 + V2 + V3 + V4 
             f3 =~ V5 + V6 + V7 + V8
'

fit <- cfa(model.syntax=mymod, data=mydata, orthogonal=TRUE,
           std.lv=TRUE)
summary(fit, fit.measures=TRUE)








mymod2  <- '
             f1 =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8
             f2 =~ V1 + V2 + V3 + V4
             f3 =~ V5 + V6 + V7 + V8
             f1 ~~ 0*f2
             f1 ~~ 0*f3
             f2 ~~ 0*f3
'

fit2 <- cfa(model.syntax=mymod,
           data=mydata, orthogonal=TRUE,
           std.lv=TRUE)
summary(fit2, fit.measures=TRUE)



##manually fixing the variances and freeing the loadings of the first
## indicator
mymod3  <- '
             f1 =~ NA*V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8
             f2 =~ NA*V1 + V2 + V3 + V4
             f3 =~ NA*V5 + V6 + V7 + V8
             f1 ~~ 1*f1
             f2 ~~ 1*f2
             f3 ~~ 1*f3
             f1 ~~ 0*f2
             f1 ~~ 0*f3
             f2 ~~ 0*f3
'

fit3 <- cfa(mymod3, data=mydata, mimic="Mplus")
summary(fit3, fit.measures=TRUE)


#change to observed information matrix
fit4 <- cfa(model.syntax=mymod,
           mimic="Mplus",
           data=mydata, orthogonal=T,
           std.lv=T, information="observed")

summary(fit4, fit.measures=TRUE)
#didn't converge


##try to tune up, control. fails. cfa quits iterating anyway
fit5 <- cfa(model.syntax=mymod,
           mimic="Mplus",
           data=mydata, orthogonal=T,
           std.lv=T, information="observed",
            control=list(iter.max=100000, eval.max=100000))
summary(fit5, fit.measures=TRUE)


fit6 <- cfa(model.syntax=mymod,
           mimic="Mplus",
           data=mydata, orthogonal=T,
           std.lv=T, information="observed",
            control=list(optim.method="BFGS"))


summary(fit6, fit.measures=TRUE)





getData <- function(myg=NULL, myf=NULL, Nsample=NULL){
  sim2 <- sim.hierarchical(gload=myg, fload=myf, n= Nsample, raw=T)
  mydata <- as.data.frame(sim2$observed)
  fit <- cfa(model.syntax=mymod, data=mydata, orthogonal=TRUE,
             std.lv=TRUE)
  summary(fit, fit.measures=TRUE)
  list(conv=fit@Fit@converged, fit)
}


res <- list()
for(i in 1:10) res[[i]] <- getData(myg, myf, Nsample)
