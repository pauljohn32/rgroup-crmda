## Paul Johnson <pauljohn@ku.edu>
## 2012-06-20

## mlm-1 Initial approach to problem

## Topic: create multi-level data and use regression and lmer to
## estimate it.  This creates 3 different dependent variables,
## y1: ordinary "homoskedastic" regression (no grouping effect0
## y2: clustered "random intercepts"
## y3: clustered "random intercepts" and "random slope" for 1 predictor (x1)

## Why do this? Mainly, I want to show how I "Think Through" a
## problem by building a series of example models.

## Substantively, what ispires this?  A Student wonders "how good is R
## for making plots for lots of different groups in a multilevel
## model?" Another Student wonders "what is your thought progression
## in designing a Monte Carlo simulation".  Lets try to answer both in
## same exercise. Along the way, I had a detour into a problem of
## calculating predicted values from lmer results, but after that was
## solved, then things came into focus.

library(MASS) ## for rmvnorm
set.seed(1234) ## for replicability

## Step 1. Create a Data Frame. We need ONE data set that works right!

## M respondents, N observations for each one.
## In a repeated measures context, this is called "longitudinal data".
## In cross sectional approach, this is M groups (classrooms) with
## several people in each)

M <- 100
N <- 10

## get 100 unique gray colors, will use later for plotting
mycolors <- gray.colors(M)
## With small M, brighter colors help. You choose
mycolors <- rainbow(M)

## Standard deviation of error term at individual level
STDE <- 48

## STDEb0: standard deviation of clustered intercepts.
### In a longitudinal "repeated measures" exercise, this is an
## individual-level effect In a cross section, this is a random
## classroom effect.
STDEb0 <- 30

## STEx1: standard deviation of slopes across cluster units
STDEb1 <- 5

## The true fixed effects of b0, b1, b2, b3 in
## y = b0 + b1*x1 + b2*x2 + b3*x3
bslopes <- c(0.2, 15.4, -0.2, 3)

## Now generate the data frame with x1, x2 and x3.
## Let's suppose the predictors are Multivariate Normal, with
## means (100,200,150), standard deviations (10, 20, 30), and
## intercorrelations of 0.4.  These can, of course, be adjusted
## for interesting variations.

## Mind, an "indicator" that says with which cluster an observation belongs.
Mind <- 1:M %x% rep(1,N)
## Iind, "I index" indexes cluster members.
Iind <- unlist(lapply(1:M, function(x) seq(1, N, by=1)))

Xmeans <- c(100, 200, 150)
Xsds <- c(10, 20, 30)
Xrho <- 0.4
Xcorr.mat <- matrix(c(1, Xrho, Xrho, Xrho, 1, Xrho, Xrho, Xrho, 1), nrow = 3)
sigma <- diag(Xsds) %*% Xcorr.mat %*% diag(Xsds)
X.mat <- mvrnorm(n = N * M, mu = Xmeans, Sigma = sigma)
dimnames(X.mat)[[2]] <- c("x1", "x2", "x3")
## Add an intercept on the front of X.mat
X.mat <- cbind("(Intercept)"= 1, X.mat)


## Create a dependent variable that has no clustering effects.  This
## is an "ordinary" regression with slopes and random error designated
## above random noise
y1 <- X.mat %*% bslopes + rnorm(M*N, m=0, s= STDE)
dat <- cbind(data.frame(id=1:(N*M), Mind, Iind, y1), as.data.frame(X.mat))
rm(Mind, y1, X.mat) ## cleanup workspace

## Layer on additive group level error in y2
## Add a group-level intercept in the data collection.

## m different amounts added each of M respondent (group) to
## each observation for that respondent (or group)
dat$y2 <- dat$y1 +  rnorm(M, 0, STDEb0) %x% rep(1, N)

## Confirm that linear algebra, study, run
## rnorm(5) %x% rep(1, 4)


## In y3, add in random slope effect
## Now insert a group level random slope for x1. Thus, we
## are changing the effect of x1 from 0.4 * x1 to
## b1 ~ N(0.4, STDEb1) * x1
##
## same as b1 ~ 0.4 + N(0, STDEb1) * x1
dat$y3 <- dat$y2 +  dat$x1 * rnorm(M, 0, STDEb1) %x% rep(1, N)


## Well, that's some data. Let's see how the fitted models work

plot(y1 ~ x1, data=dat)
## First, fit linear regression that ignores mixed effects
m1 <- lm(y1 ~ x1 + x2 + x3, data=dat)
summary(m1)
termplot(m1, terms="x1", partial.resid=T, se=T)
termplot(m1, terms="x2", partial.resid=T, se=T)
termplot(m1, terms="x3", partial.resid=T, se=T)

library(rockchalk)
plotPlane(m1, plotx1="x1", plotx2="x2")
plotPlane(m1, plotx1="x3", plotx2="x1", plwd = 0.1, pcex=0.8, drawArrows=TRUE)



plot(y2 ~ x1, data=dat)
## First, fit linear regression that ignores mixed effects
m2 <- lm(y2 ~ x1 + x2 + x3, data=dat)
summary(m2)




plot(y3 ~ x1, data=dat)
## First, fit linear regression that ignores mixed effects
m3 <- lm(y3 ~ x1 + x2 + x3, data=dat)
summary(m3)

## causes error in R-2.15, but shouldn't
## par(mfcol = c(1,3))
## termplot(m1, partial.resid=TRUE, se=TRUE)
## par(mfcol = c(1,1))

## In the lme4 package, there is an "easy" tool to run M separate lm regressions
library(lme4)
m31list <- lmList(y3 ~ x1 | Mind, data=dat, pool = FALSE)
##That is 100 separate regressions in a list

summary(m31list[[1]]) ## group 1
summary(m31list[[2]]) ## group 2
summary(m31list[[M-1]]) ## group M-1



plot(y3 ~ x1, data=dat, col= mycolors[Mind])

for( i in seq_along(m31list)){
    abline(m31list[[i]], col = mycolors[i])
}
## That graph is lacking somewhat. I wish the range of x1 were not
## the same for all groups. Will fix that in the next plot.



## M separate regressions, with 3 predictors
m3list <- lmList(y3 ~ x1 + x2 + x3 | Mind, data=dat, pool = FALSE)


## How calculate predicted values?  Let's set x2 and x3 at their
## individual-specific means.

predict( m3list[[1]] ) ##predicts for observed cases, group 1


plot(y3 ~ x1, data=dat, col=mycolors[Mind], cex=0.6)

## Now calculate predicted values for each model.
## How to do that? Here's one guess.
## Get the range of x1 from the 'whole sample' and
## then set x2 and x3 at within-cluster mean
x1range <- range(dat$x1)
## I have to figure this out 1 group at a time.
## Get the data set for group 1. Lets concentrate on them
mf1 <- model.frame(m3list[[1]])
pgroup <- predict( m3list[[1]], newdata = data.frame(x1 = x1range, x2 = mean(mf1$x2, na.rm = TRUE), x3 = mean(mf1$x3, na.rm = TRUE)))
lines(x1range, pgroup, col=mycolors[1])

mf1 <- model.frame(m3list[[4]]) ## extract data set for group 4
pgroup <- predict( m3list[[4]], newdata = data.frame(x1=x1range, x2=mean(mf1$x2), x3=mean(mf1$x3)))
lines(x1range, pgroup, col=mycolors[4])


## Now I see a pattern. Do same for all the groups.
## This time, I'll use the unique range on x1 for each cluster
## I think that makes the lines look more "in" the data.
plot(y3 ~ x1, data=dat, col=mycolors[Mind], main = "lm on clusters")
for( i in seq_along(m3list)){
    m3mf <- model.frame(m3list[[i]]) #data set for group i
    x1range <- range(m3mf$x1) ## use group-specific ranges this time
    pgroup <- predict( m3list[[i]], newdata = data.frame(x1=x1range, x2=mean(m3mf$x2), x3=mean(m3mf$x3)))
    lines(x1range, pgroup, col=mycolors[i])
}

## Hmm. That data looks like it might be handy for comparison later on.
## Lets try to build a data frame of it.

m3newdat <- lapply(m3list, function(x) {
    m3mf <- model.frame(x)
    ndf = data.frame(x1=range(m3mf$x1), x2=mean(m3mf$x2), x3=mean(m3mf$x3))
    ndf$m3pred <- predict(x, newdata = ndf)
    ndf} )
## Smash the list of data frames together
m3newdat <- do.call("rbind", m3newdat)
## Interesting!
m3newdat[1:20, ]
## Better add a variable Mind. This way seems like overkill, but probably is more
## robust than rep(1:M, each=2). But capitalizes on row names
m3newdat$Mind <-  as.integer(do.call("rbind", strsplit(row.names(m3newdat), split="\\.", perl=T))[ ,1])

## Draw new graphs on a new device, so we can compare
dev.new()



## Change gears.
##
## Now, estimate this as a mixed-effects model with lme4
require(lme4)  ##Make sure lme4 is loaded


## mm2: Just the random intercept, no random slope
mm2 <- lmer( y2 ~ x1 + x2 + x3 + (1 | Mind), data=dat)
summary(mm2) ### Wow. extremely accurate estimate of STDEb0
mm2VarCorr <- VarCorr(mm2)

## Yank out the standard deviation estimates
mm2reStdDev <- c(lapply(mm2VarCorr, attr, "stddev"), list(Residual = attr(mm2VarCorr, "sc")))
mm2reStdDev


## Both random intercept and random slope, not correlated with each other
mm3 <- lmer( y3 ~ x1 + x2 + x3 + (1|Mind) + (-1 + x1 | Mind), data=dat)
summary(mm3) ### Wow. Check estimates versus truth
mm3VarCorr <- VarCorr(mm3)
mm3VarCorr


## This allows the random effects to be correlated, even
## though they are not in the design.
mm4 <- lmer( y3 ~ x1 + x2 + x3 + (x1 | Mind), data=dat)
summary(mm4)
mm4VarCorr <- VarCorr(mm4)
mm4VarCorr
## Yank out the standard deviation estimates
mm4reStdDev <- c(lapply(mm4VarCorr, attr, "stddev"), list(Residual = attr(mm4VarCorr, "sc")))
mm4reStdDev

mm4ranef <- ranef(mm4, postVar = TRUE) ## a ranef.mer object, appears to be a list that includes one data frame

## Here's a fun one.
dotplot(mm4ranef)


### What do the predicted values mean from lmer?
### here they are. But what do they include?
head(mm4pred <- predict(mm4))


## So I'm going to go through the step-by-step to make sure I
## understand the process of calculating predicted values. These
## should follow a formula like this for model 4. j is the "cluster"
## index.
##
##  (b0 + raneff(intercept, j)) + (b1 + raneff(x1,j))*x1 + b2 x2 + b3 x3

mm4b <- fixef(mm4)
mm4vnames <- names(mm4b)
mm4mm <- model.matrix(mm4) ## grab predictors

## b0 + raneff(intercept, j)
mm4inteffect <- mm4b["(Intercept)"] +  mm4ranef[[1]][dat$Mind, 1]
mm4x1effect <- mm4mm[ , c("x1")] * (mm4b["x1"] + mm4ranef[[1]][dat$Mind, 2])
mm4pred2 <- mm4inteffect + mm4x1effect +  mm4mm[ ,c("x2","x3") ] %*%   mm4b[c("x2","x3")]
head(mm4pred2)

## Aha! Those exactly match predict for mm4. So I understand what
## "predict" does. It is the "conditional" mean, where the "best
## guess" for each sub unit is used to make a prediction about the
## modal outcome within each sub unit.


## Now, I want to run predict for some particular values of x1, x2, x3.
##
## Recall the "newdata" object we used with m3: m3newdat
## Get predictions for each row in that data
m3newdat$mm4pred <- predict(mm4, newdata = m3newdat)

plot(y3 ~ x1, data=dat, col=mycolors[Mind], main="lmer mixed model predictions")
by(m3newdat, m3newdat$Mind, function(x) { lines(x$x1, x$mm4pred, col=mycolors[x$Mind])})

## With a small N and M, it is especially obvious the predicted values are incorrect.


## Lets calculate predicted values manually, as before,
## just to be sure we are getting the same numbers
mm4b <- fixef(mm4)
mm4vnames <- names(mm4b)
mm4inteffect <- mm4b["(Intercept)"] +  mm4ranef[[1]][m3newdat$Mind, 1]
mm4x1effect <- m3newdat[ , c("x1")] * (mm4b["x1"] + mm4ranef[[1]][m3newdat$Mind, 2] )
mm4mmpred2 <- mm4inteffect + mm4x1effect +  as.matrix(m3newdat[ ,c("x2","x3") ]) %*%  mm4b[c("x2","x3")]
m3newdat$mm4manualpred <-  mm4inteffect + mm4x1effect +  as.matrix(m3newdat[ ,c("x2","x3") ]) %*%  mm4b[c("x2","x3")]


## Those values appear correct to me, here's how predictions should look

plot(y3 ~ x1, data=dat, col=mycolors[Mind], main="lmer mixed model predictions")
by(m3newdat, m3newdat$Mind, function(x) { lines(x$x1, x$mm4manualpred, col=mycolors[x$Mind])})


## Now grab estimates from lm and lmer for closer inspection.
## intercepts on M separate lm fits:
m3b0  <- sapply(m3list, function(m) coef(summary(m))["(Intercept)", "Estimate"])
sd(m3b0)

### The intercept "predictions" (blups) from mm4
mm4b0 <- mm4b["x1"] + mm4ranef[[1]][, "(Intercept)"]
sd(mm4b0)

###The slope estimates from M separate lm fits:
m3x1b  <- sapply(m3list, function(m) coef(summary(m))["x1", "Estimate"])
sd(m3x1b)

### The slope "predictions" (blups) from mm4
mm4x1b <- mm4b["x1"] + mm4ranef[[1]][, "x1"]
sd(mm4x1b)


## Are the mm4 b's better? Closer to expected values? Shrunken toward center?
op <- par(no.readonly = TRUE)
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(m3x1b, mm4x1b, xlab = expression(hat(b)[1] ~~from~~lm), ylab = expression(hat(b)[1] ~~from~~lmer~~mixed~~model),  main=expression(paste("Scatter of ",hat(b)[1]," from lm and lmer" )))
mx1b <- lm( mm4x1b ~ m3x1b)
abline(mx1b)
summary(mx1b)
options(digits=2)
legend("bottomright", as.expression(c(bquote(widehat(hat(b)[1*j]^{lmer}) == .(coef(mx1b)[1]) + .(coef(mx1b)[2])*hat(b)[1*j]))), lty=1, lwd=1)
par(op)


## Let's compare histograms of the slope estimates
par(mfcol=c(1,2))
par(mar=c(5.1, 4.1, 6.1, 2.1), xpd=T)

b1breaks <- plotSeq(range(c(m3x1b, mm4x1b)), 20)
m3x1bhist <- hist(m3x1b, breaks=b1breaks, plot = FALSE)
m4x1bhist <- hist(mm4x1b, breaks =b1breaks, plot = FALSE)
ylim <- magRange(c(0, m3x1bhist$density, m3x1bhist$density),  c(1, 1.2))
hist(m3x1b, breaks=b1breaks, prob = TRUE, ylim=ylim, xlab=expression(paste("100 lm estimates ", hat(b)[1])), main=paste("Separate OLS (lm)"))
legend("topright", legend = as.expression(c( bquote(mean(hat(b)[1])==.(round(mean(m3x1b),2))), bquote(sd(hat(b)[1])==.(round(sd(m3x1b),2))))))

hist(mm4x1b,  breaks=b1breaks, prob = TRUE, ylim=ylim, xlab=expression(paste("lmer 'blup' estimates ", hat(b)[1])), main="Mixed Model (lmer)")
legend("topleft", legend = as.expression(c( bquote(mean(hat(b)[1])==.(round(mean(mm4x1b),2))), bquote(sd(hat(b)[1])==.(round(sd(mm4x1b),2))))))
par(op)


## Plot the estimated slopes against the "true" random effects. I
## notice now I did not save the vectors of random effects Rather than
## fix that here, I stop and think this over for a while.

