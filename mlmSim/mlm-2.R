## Paul Johnson <pauljohn@ku.edu>
## 2012-06-24

## mlm-2. This is step 2 in the journey

## Topic: create multi-level data and use regression and lmer to
## estimate it.  This creates 3 different dependent variables,
## y1: ordinary "homoskedastic" regression (no grouping effect0
## y2: clustered "random intercepts"
## y3: clustered "random intercepts" and "random slope" for 1 predictor (x1)

## Change 1. I demonstrated mlm-1.R for James Selig, who suggested
## it was important to have clusters made up of various numbers of
## observations (unbalanced groups). Now there are M groups, but
## the variable N becomes a vector that represents the number of
## respondents in each cluster.

## Change 2. Do better book keeping on the random effects.  Now the
## random effects are drawn from a multivariate normal, allowing
## correlation between them. The random effects matrix is called
## Mreffects, its columns are (b0, b1), representing the intercept and
## slope random effects.  In my opinion, it is easier to specify the
## parameters with standard deviations and correlations.  The Covar
## matrix of the random effects "Msigma" is built up from those
## parameters.

##          | STDEb0  0   | |1  Mrho   | | STDEb0  0   |
## Msigma = |             |X|          |X|             |
##          |  0   STDEb1 | |Mrho    1 | |  0   STDEb1 |

library(MASS) ## for rmvnorm
set.seed(1234) ## for replicability

M <- 100  ## Clusters
## For equal-sized clusters with 10 observations, do this
## N <- rep(10, M)
## There are many ways to get varying cluster sizes.
##
N <- 134 + rpois(M, lambda=20)
## I add 4 because a cluster with fewer than 4 observations
## will cause a regression on the separate cluster to fail

## get M unique gray colors, will use later for plotting
## mycolors <- gray.colors(M)

## More colorful mycolors ?
mycolors <- rainbow(M)


## Create Mind and Iind indexes for book keeping.
## Mind, "M index" shows with which cluster an observation belongs.
Mind <- unlist(lapply(1:M, function(x) rep(x, each=N[x])))
## Iind, "I index" indexes cluster members.
Iind <- unlist(lapply(1:M, function(x) seq(1, N[x], by=1)))

##Create 3 variables from a multivariate Normal with these
## characteristics
Xmeans <- c(100, 200, 150)
Xsds <- c(20, 30, 40)
Xrho12 <- 0.4
Xrho13 <- 0.2
Xrho23 <- 0.0
Xcorr.mat <- diag(1, 3)
Xcorr.mat[1,2] <- Xcorr.mat[2,1] <- Xrho12
Xcorr.mat[1,3] <- Xcorr.mat[3,1] <- Xrho13
Xcorr.mat[2,3] <- Xcorr.mat[3,2] <- Xrho23
if(!isSymmetric(Xcorr.mat))stop("Xcorr.mat is not symmetric")

Xsigma <- diag(Xsds) %*% Xcorr.mat %*% diag(Xsds)
X.mat <- mvrnorm(n = sum(N), mu = Xmeans, Sigma = Xsigma)
dimnames(X.mat)[[2]] <- c("x1", "x2", "x3")
## Don't forget to insert an intercept later
X.mat <- cbind(X.mat)




## The true fixed effects of b0, b1, b2, b3 in
## y = b0 + b1*x1 + b2*x2 + b3*x3
bslopes <- c(1.5, 0.25, -0.2, 0.05)

## Standard deviation of error term at individual level
STDE <- 15

## Create a dependent variable that has no clustering effects.
##     FIXED Part                 +  RANDOM PART
y1 <- cbind(1, X.mat) %*% bslopes + rnorm(sum(N), m = 0, s = STDE)
dat <- cbind(data.frame(Mind, Iind, y1), as.data.frame(X.mat))
rownames(dat) <- paste(dat$Mind, dat$Iind, sep=".") ##may help bookkeeping later
rm(Mind, Iind, y1, X.mat) ## cleanup workspace

##Check the R-square on fitted model.
summary(lm(y1 ~ x1 + x2 + x3, data=dat))$r.square

## Layer on additive group level error in y2 and y3


## Parameters for random effects:
## STDEb0: standard deviation of clustered intercepts.
STDEb0 <- 0.5
## STDEb1: standard deviation of slopes across cluster units
STDEb1 <- 0.05
Mrho <- 0.2
## I'm tempted to get fancy here with a matrix for STDEb0, STDEb1, and
## Mrho. It would pay off, I expect. But be harder to teach.
Msigma <- diag(c(STDEb0, STDEb1)) %*% matrix(c(1, Mrho, Mrho, 1), 2, 2) %*% diag(c(STDEb0, STDEb1))
Mreffects <- mvrnorm(M, mu = c(0, 0), Sigma = Msigma)
colnames(Mreffects) <- c("b0","b1")

## In y2, include random intercept
dat$y2 <- dat$y1 + Mreffects[ ,"b0"][dat$Mind]

## In y3, add in random slope effect
dat$y3 <- dat$y2 +  dat$x1 * Mreffects[ ,"b1"][dat$Mind]

## Do some diagnostics on Mreffects
plot(Mreffects[, 1], Mreffects[,2], xlab = "Intercept Random Effect", ylab = "Slope Random Effect",  main = "True Random Effects")
##library(rockchalk)
##summarize(Mreffects)
summary(Mreffects)
apply(Mreffects, 2, sd)
cor(Mreffects)

plot(Mreffects[ ,"b0"][dat$Mind], dat$x1 * Mreffects[ ,"b1"][dat$Mind], col=dat$Mind, main = "Random Slopes and Intercepts", xlab="Intercept Random Effect (b0)", ylab="Slope Random Effect (b1*x)")

## The "grand" regression?
m1 <- lm(y3 ~ x1 + x2 + x3, data=dat)
summary(m1)

## The "dummy variable" regression?
m2 <- lm(y3 ~ x1 + x2 + x3 + as.factor(Mind), data=dat)
summary(m2)


library(lme4)
## M separate regressions, with 3 predictors
m3list <- lmList(y3 ~ x1 + x2 + x3 | Mind, data=dat, pool = FALSE)

## Predicted values set x2 and x3 at their cluster-specific means.

plot(y3 ~ x1, data=dat, col=mycolors[Mind], lwd = 0.6, main = "lm on clusters")
for( i in seq_along(m3list)){
    m3mf <- model.frame(m3list[[i]]) #data set for group i
    x1range <- range(m3mf$x1) ## use group-specific ranges this time
    pgroup <- predict( m3list[[i]], newdata = data.frame(x1=x1range, x2=mean(m3mf$x2), x3=mean(m3mf$x3)))
    lines(x1range, pgroup, col=mycolors[i])
}

## If a cluster has 3 or fewer cases, this warning will appear
## Warning message:
## In predict.lm(m3list[[i]], newdata = data.frame(x1 = x1range, x2 = mean(m3mf$x2),  :
##   prediction from a rank-deficient fit may be misleading


## Keep a record.
m3newdat <- lapply(m3list, function(x) {
    m3mf <- model.frame(x)
    ndf = data.frame(x1=range(m3mf$x1), x2=mean(m3mf$x2), x3=mean(m3mf$x3))
    ndf$m3pred <- predict(x, newdata = ndf)
    ndf} )
m3newdat <- do.call("rbind", m3newdat)
## Better add a variable Mind. This way seems like overkill, but probably is more
## robust than rep(1:M, each=2). It capitalizes on row names
m3newdat$Mind <-  as.integer(do.call("rbind", strsplit(row.names(m3newdat), split="\\.", perl=T))[ ,1])

## Draw new graphs on a new device, so we can compare
dev.new()


##
## Estimate this as a mixed-effects model with lme4

## mm2: Just the random intercept, no random slope
mm2 <- lmer( y2 ~ x1 + x2 + x3 + (1 | Mind), data=dat)
summary(mm2)
mm2VarCorr <- VarCorr(mm2)
mm2VarCorr

cor(fitted(mm2), dat$y2)

## Both random intercept and random slope, not correlated with each other
## Depending on Mrho, this may be a 'wrong model'.
mm3 <- lmer( y3 ~ x1 + x2 + x3 + (1|Mind) + (0 + x1 | Mind), data=dat, verbose=3)
summary(mm3)
mm3VarCorr <- VarCorr(mm3)
mm3VarCorr

cor(fitted(mm3), dat$y3)



## Yank out the standard deviation estimates. idiom stolen from formatVC in lmer code.
mm3reStdDev <- c(lapply(mm3VarCorr, attr, "stddev"), list(Residual = attr(mm3VarCorr, "sc")))
mm3reStdDev


mm3reStdDev[["Mind"]]["(Intercept)"] - STDEb0
##mm3reStdDev[["Mind"]]["x1"] - STDEb1
## Can't figure how to get STD(b1)
mm3reStdDev[["Residual"]] - STDE

## The "right" model allows the random effects to be correlated (Supposing
## Mrho not 0).
mm4 <- lmer( y3 ~ x1 + x2 + x3 + (x1 | Mind), data=dat)
summary(mm4)
mm4VarCorr <- VarCorr(mm4)
mm4VarCorr

cor(fitted(mm4), dat$y3)

plot(fitted(mm4), dat$y3, col=dat$Mind)



## Yank out the standard deviation estimates
mm4reStdDev <- c(lapply(mm4VarCorr, attr, "stddev"), list(Residual = attr(mm4VarCorr, "sc")))
mm4reStdDev


## Ever do summary stats on the predicted random effects?
mm4ranef <- ranef(mm4, postVar = TRUE) ## a ranef.mer object,  a list that includes one data frame
apply(mm4ranef[["Mind"]], 2, mean)
apply(mm4ranef[["Mind"]], 2, sd)
summarize(mm4ranef$Mind)
cor(mm4ranef$Mind)

dotplot(mm4ranef)

m3newdat$mm4pred <- predict(mm4, newdata = m3newdat)

plot(y3 ~ x1, data=dat, col=mycolors[Mind], main="lmer mixed model predictions")
by(m3newdat, m3newdat$Mind, function(x) { lines(x$x1, x$mm4pred, col=mycolors[x$Mind])})


## Double-check prediction values
mm4b <- fixef(mm4) ## coef(summary(mm4))[ ,1]
mm4vnames <- names(mm4b)
##  b0 + raneff(intercept, j)
mm4inteffect <- mm4b["(Intercept)"] +  mm4ranef[[1]][m3newdat$Mind, 1]
mm4x1effect <- m3newdat[ , c("x1")] * (mm4b["x1"] + mm4ranef[[1]][m3newdat$Mind, 2] )
mm4mmpred2 <- mm4inteffect + mm4x1effect +  as.matrix(m3newdat[ ,c("x2","x3") ]) %*%  mm4b[c("x2","x3")]
m3newdat$mm4manualpred <-  mm4inteffect + mm4x1effect +  as.matrix(m3newdat[ ,c("x2","x3") ]) %*%  mm4b[c("x2","x3")]

cbind(m3newdat$mm4manualpred, m3newdat$mm4pred)

## OK, looks good
plot(y3 ~ x1, data=dat, col=mycolors[Mind], main="lmer mixed model predictions")
by(m3newdat, m3newdat$Mind, function(x) { lines(x$x1, x$mm4manualpred, col=mycolors[x$Mind])})




## Now grab estimates from lm and lmer for closer inspection.
## intercepts on M separate lm fits:
m3b0  <- sapply(m3list, function(m) coef(m)["(Intercept)"])
sd(m3b0)

### The intercept "predictions" (blups) from mm4
mm4b0 <- mm4b["(Intercept)"] + mm4ranef[[1]][, "(Intercept)"]
sd(mm4b0)

###The slope estimates from M separate lm fits:
m3x1b  <- sapply(m3list, function(m) coef(m)["x1"])
sd(m3x1b)

### The slope "predictions" (blups) from mm4
mm4x1b <- mm4b["x1"] + mm4ranef[[1]][, "x1"]
sd(mm4x1b)


## Are the mm4 b's better than lm's? Closer to expected values? Shrunken toward center?
op <- par(no.readonly = TRUE)
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(m3x1b, mm4x1b, xlab = expression(hat(b)[1] ~~from~~lm), ylab = expression(hat(b)[1] ~~from~~lmer~~mixed~~model),  main=expression(paste("Shrinkage: Scatter of ",hat(b)[1]," from lm and lmer" )))
mx1b <- lm( mm4x1b ~ m3x1b)
abline(mx1b)
summary(mx1b)
options(digits=2)
legend("bottomright", as.expression(c(bquote(widehat(hat(b)[1*j]^{lmer}) == .(coef(mx1b)[1]) + .(coef(mx1b)[2])*hat(b)[1*j]))), lty=1, lwd=1)
options(digits=6)
par(op)


## Let's compare histograms of the slope estimates
par(mfcol=c(1,2))
par(mar=c(5.1, 4.1, 6.1, 2.1), xpd=T)

## Uses rockchalk: b1breaks <- plotSeq(range(c(m3x1b, mm4x1b)), 20)
b1breaks <- seq(range(c(m3x1b, mm4x1b))[1], range(c(m3x1b, mm4x1b))[2], length.out = 20)
m3x1bhist <- hist(m3x1b, breaks=b1breaks, plot = FALSE)
m4x1bhist <- hist(mm4x1b, breaks =b1breaks, plot = FALSE)
## Uses rockchalk: ylim <- magRange(c(0, m3x1bhist$density, m3x1bhist$density),  c(1, 1.2))

ylim <- range(c(0, m3x1bhist$density, m3x1bhist$density))*c(1, 1.2)
hist(m3x1b, breaks=b1breaks, prob = TRUE, ylim=ylim, xlab=expression(paste("100 lm estimates ", hat(b)[1])), main=paste("Separate OLS (lm)"))
legend("topright", legend = as.expression(c( bquote(mean(hat(b)[1])==.(round(mean(m3x1b),2))), bquote(sd(hat(b)[1])==.(round(sd(m3x1b),2))))))

hist(mm4x1b,  breaks=b1breaks, prob = TRUE, ylim=ylim, xlab=expression(paste("lmer 'blup' estimates ", hat(b)[1])), main="Mixed Model (lmer)")
legend("topleft", legend = as.expression(c( bquote(mean(hat(b)[1])==.(round(mean(mm4x1b),2))), bquote(sd(hat(b)[1])==.(round(sd(mm4x1b),2))))))
par(op)


## Plot the estimated slopes against the "true" random effects from Mreffects
plot(bslopes[2] + Mreffects[ ,2], m3x1b, xlab="Cluster Slopes (True Values)", ylab="Estimates of Slopes (across Clusters)", col="gray70", xlim= c(1.04,1.04)*range(c(bslopes[2] + Mreffects[ ,2], m3x1b)), ylim=c(1.04,1.04)*range((c(bslopes[2] + Mreffects[ ,2], m3x1b))))
points(bslopes[2] + Mreffects[ ,2], mm4x1b, , col="black", pch=13)
legend("topleft", c("OLS Slopes", "Mixed Model Slopes"), pch=c(1,13), col=c("gray70","black"))



## Plot the estimated intercept effects against the "true" random effects from Mreffects
plot(bslopes[1] + Mreffects[ ,1], m3b0, xlab="Cluster Intercepts (True Values)", ylab="Estimates of Intercepts", col="gray70")
points(bslopes[1] + Mreffects[ ,1], mm4b0 , col="black", pch=13)
legend("topleft", c("OLS Intercepts", "Mixed Model Intercepts"), pch=c(1,13), col=c("gray70","black"))


plot(Mreffects[, 1], m3b0)

plot(Mreffects[, 1], mm4b0)
