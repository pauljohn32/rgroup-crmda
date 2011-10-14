## Paul Johnson <pauljohn@ku.edu>
## 2011-10-12

## Aim is to learn to generate "bi-factor" structured indicators
## for a cfa analysis and then simulate repeatedly.

## As it stands now, I believe sim.hierarchical is generating
## the data correctly, but lavaan has trouble analyzing it.
## I'm going to contact the authorities on the singularity in
## lavaan's standard error calculations.  I'm also going
## to see if an MPlus user will fit that model for me.

set.seed(12345)

## First, set the sample size.
Nsample <- 500

## Using the psych package by William Revelle. This is a widely known
## collection of routines intended for psychometrics and psychology
## more generally. I still don't understand the terminology
## "group" and "hierarchical" very well. Still.

## In this code, we explore
## sim.hierarchical to generate data for "bi.factor" analysis

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

## how should I set "myg" and "myf" so that they get what they
## want from sim.hierarchical?

myf <- matrix( c( 0.4,0.4,0.5,0.3,0.5, 0.4, 0.2, 0.5,
            0.2,0.4,0.4,0.4,rep(0,4),
            rep(0,4), 0.3,0.5,0.3,0.5), ncol=3)


### We want the three factors are all completely separate
### from sim.hierarical's group loading, "gload"
myg <- c(0,0,0)



sim2 <- sim.hierarchical(gload=myg, fload=myf, n= Nsample, raw=T)
sim2

##Note that although they do not print to the screen from sim2,
## the sample data is in there. It is sim2$observed
str(sim2)

## Grab the data so we can study it
mydata <- as.data.frame(sim2$observed)

## the omega graph shows that the "3 factor bi.factor" model
## can be re-done as a general factor with 2 special factors.

omega(sim2$model)

##I can't understand this at all.
##omega(sim2$model, sl=F)


## Analyze the data with CFA to see if we are getting what
## we ask for

## I think lavaan's method is more understandable than sem.

library(lavaan)
mymod  <- '
             f1 =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8
             f2 =~ V1 + V2 + V3 + V4 
             f3 =~ V5 + V6 + V7 + V8
             f1 ~~ 0*f2
             f1 ~~ 0*f3
             f2 ~~ 0*f3
'

## lavaan
##

sim2cov <- cov(sim2$observed)

#analyze the "true" model
mycov <- sim2$model

#Or pretend the observed correlation matrix is covar
#myrcov <- sim2$r

fit <- cfa(model.syntax=mymod,
           sample.cov= sim2cov,
           sample.nobs= Nsample,
           mimic="Mplus",
           missing="listwise",
           data=mydata, orthogonal=T,
           std.lv=T)


summary(fit, fit.measures=TRUE)


fit <- cfa(model.syntax=mymod,
           sample.cov= sim2cov,
           sample.nobs= Nsample,
           mimic="Mplus",
           missing="listwise",
           sample.mean=mean(sim2$observed),
           meanstructure=T,
           control=list(optim.method="BFGS"),
           data=mydata, orthogonal=T,
           std.lv=T)


summary(fit, fit.measures=TRUE)
