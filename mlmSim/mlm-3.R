## Paul Johnson <pauljohn@ku.edu>
## 2012-08-26

## mlm-3. This is step 3 the my R programming tutorial. I'm building
## up to a simulation of a lot of datasets and a lot of fitted models.

## In mlm-1 and mlm-2, I've mainly been interested in learing what a
## multi-level model is and what lmer does. Now I need to abstract
## that information and create some funtions to automate work. Here,
## the functions are at the top. Scroll down to "Demonstration starts
## here!" for the usage.

##  genData
##' Generate simulated regression data with various types of mixed
##' effects.  This creates a matrix X of MVN values, generate "random
##' effects" columns for each of the regression coefficients. The
##' dimensions of X are determined implicitly from the parameters that
##' the user supplies. The default settings provide 3 columns of
##' predictor variables.
##'
##' The user supplies the "true" beta coefficients, the slopes in
##' y = cbind(1, X) %*% beta =  beta[1] + beta[2]X[,1]...
##'
##' The function deduces the required width of X from beta: X is
##' (length(beta) - 1) columns wide. The number of rows in X is
##' randomly determined, it reflects M, Nmin, and Nvar. Thats M
##' clusters of observations, Nmin is minimum size of sample within
##' cluster, and Nvar, variation in cluster size (put to 0 for
##' balanced).
##'
##' The user can specify the "true" correlation matrix for the columns
##' of X, as well as a "true" correlation matrix for the columns of
##' the random effects that perturb the betas.
##'
##' The mixed effects are multivariate normal with expected value 0
##' and a covariance matrix that is consistent with the user specified
##' parameters STDEb (standard deviations of disturbances) and bcorr,
##' the correlations assumed among those random effects.
##'
##' @param M Optional. Integer, number of clusters, default = 100.
##' @param Nmin Optional. Integer, minimum number of observations per cluster, default = 20.
##' @param Nvar Optional.  Real number, variance of cluster size (implemented as
##' Poisson random), default = 0 (representing balanced data)
##' @param beta Required. Vector of real numbers, true beta for
##' fixed effects intercept and X's. d dimension
##' @param Xmeans Optional. Real number, possibly a vector of real numbers. Determines the means of the columns of the X.  One
##' number (common mean to all X columns) or a vector of means
##' (length(beta) - 1), default is 200.
##' @param Xsds Optional.  Real number, either single value or a vector. Standard deviations of X columns, length = (d-1)
##' @param Xcorr Correlation matrix of x columns, (d-1)x(x-1)
##' dimension. If Xcorr is not specified, the correlations between X
##' columns will default to 1. If Xcorr is specified as a single
##' number, then all X collumns are correlated with each other to same
##' value. Otherwise, Xcorr must be a symmetric matrix of correlation
##' coefficients of size (d-1)x(d-1)

##' @param STDEE standard deviation of the error term
##' @param STDEb standard deviation of random effects--perturbations
##' applied to b's within the clusters
##' @param bcorr symmetric matrix: correlation between random effects on b's
##' @param seed  Random generator starting point. If omitted, current R seed is used.
##' @return A list with:
##' 1. dat, a data frame with columns
##' Mind(cluster indicator), Iind (individual indicator within cluster),
##' X columns,  MVN predictors
##' y1: ordinary "homoskedastic" regression (no grouping effect)
##' y2: clustered "random intercepts"
##' y3: clustered "random intercepts" and "random slope" for 1 predictor (x1)
##' 2. Mreffects. Matrix of random effects, one row for each M element, one column for each beta
##' @author Paul Johnson <pauljohn@@ku.edu>
##'
##'
genData <- function(M = 100, Nmin = 20, Nvar = 5,
                    beta = c(1.5, 0.25, -0.2, 0.05),
                    Xmeans = 200,
                    Xsds = 40,
                    Xcorr = diag(1, ncol=length(beta)),
                    STDEE = 15, STDEb = rep(0, length(beta)),
                    bcorr = diag(1, nrow=length(beta)),
                    seed) {
    if (is.null(beta)) stop("a vector of beta coefficients must be suppiled in order to manufacture a model, such as y = beta[1] + beta[2] * x1 + beta[3] * x2 + ...")
    d <- length(beta)

    ## argument checking
    if (length(Xmeans) == 1) {
        Xmeans <- rep(Xmeans, d - 1) #assign same for all
    } else if (length(Xmeans) >= d || length(Xmeans) < d - 1)
        stop("Xmeans has too many elements. It should be ", d - 1, "elements")

    if (length(Xsds) == 1) {
        Xsds <- rep(Xsds, d-1) ## assign same for all
    } else if (length(Xsds) >= d || length(Xsds) < d-1)
        stop("Xsds should have ", d - 1, "elements")

    if(!missing(seed)) set.seed(seed) ## replication

    if (is.matrix(Xcorr)){
        stopifnot (isSymmetric(Xcorr))
        stopifnot (dim(Xcorr)[1] == d-1)
    } else if (length(Xcorr) == 1) {
        Xcorr <- diag(Xcorr, nrow = d-1)
    } else stop(paste("If you supply Xcorr as a matrix, it should be symmetric, with dimensions length(beta)-1)x(length(beta-1), which in this case would be: ",  (d-1), " x ",  (d-1)))


    if (missing(bcorr)) {
        bcorr <- diag(1, nrow = d)
    } else if (is.matrix(bcorr)) {
        stopifnot (isSymmetric(bcorr))
        stopifnot (dim(bcorr)[1] == d)
    } else if (length(bcorr) == 1) {
        bcorr <- diag(bcorr, nrow = d)
    } else stop(paste("If you supply bcorr as a matrix, it should be symmetric, with dimensions length(beta)xlength(beta), which in this case would be: ",  d, " x ", d))

    require(MASS) ## for rmvnorm

    N = Nmin + rpois(M, lambda = Nvar) ## Cluster sizes

    ## Create Mind and Iind indexes for book keeping.
    ## Mind, "M index" shows with which cluster an observation belongs.
    Mind <- unlist(lapply(1:M, function(x) rep(x, each=N[x])))
    ## Iind, "I index" indexes cluster members.
    Iind <- unlist(lapply(1:M, function(x) seq(1, N[x], by=1)))

    Xsigma <- diag(Xsds) %*% Xcorr %*% diag(Xsds)
    X.mat <- mvrnorm(n = sum(N), mu = Xmeans, Sigma = Xsigma)
    dimnames(X.mat)[[2]] <- paste("x", 1:(d-1), sep = "")

    ##     FIXED Part   +  RANDOM PART
    E <-  rnorm(sum(N), m = 0, s = STDEE)
    y1 <- cbind(1, X.mat) %*% beta + E

    ## Layer on additive group level error in y2 and y3
    Msigma <- diag(STDEb) %*% bcorr %*% diag(STDEb)
    Mreffects <- mvrnorm(M, mu = rep(0, (d)), Sigma = Msigma)
    colnames(Mreffects) <- paste("x", 0:(d-1), sep="")

    ## In y2, include random intercept
    y2 <- y1 + Mreffects[ ,"x0"][Mind]

    ## In y3, add in random slope effect, just for one variable
    y3 <- y2 +  X.mat[, "x1"] * Mreffects[ ,"x1"][Mind]
    ## In y4, add in all random slope effects
    y4 <- y2 + rowSums(X.mat * Mreffects[Mind, -1 ])

    dat <- data.frame(Mind, Iind, y1, y2, y3, as.data.frame(X.mat))
    rownames(dat) <- paste(dat$Mind, dat$Iind, sep=".") ##may help bookkeeping later
    invisible(list(dat = dat, Mreffects = Mreffects))
}


## This is termplot for multilevel clustered data. Choose your iv and dv names
## then watch the technicolor fun!
## This should run plot(dv ~ iv) with color=coding for list elements)
## For each fitted regression in "regs", draw a colored line
## to show predicted values.

spaghettiPlot.lmList <- function(regs, data, iv, cluster = "Mind", col = rainbow(length(regs))){
    ## argument check
    stopifnot(class(regs) == "lmList")
    dv <- as.character(formula(terms(regs[[1]]))[[2]]) ## get dv name
    if (sum(!c(dv, iv) %in% colnames(data)) > 0) stop("dv and iv must be column names in data")
    plot(data[, dv] ~ data[ , iv], col = col[data[ ,cluster]], lwd = 0.6, main = "Regression Within Clusters", xlab = iv, ylab = dv )
    newdat <- vector("list", length(regs))
    for( i in seq_along(regs) ){
        imod <- regs[[i]]
        imf <- model.frame(imod) #data set for cluster i
        ivrange <- range(imf[, iv]) ## use cluster-specific ranges this time
        newdf <- data.frame( iv = ivrange, lapply(imf[ , -which(colnames(imf) %in% c(dv,iv))], mean, na.rm = TRUE))
        colnames(newdf)[1] <- iv  ##pita!
        newdf$pred <- predict( imod, newdata = newdf)
        lines(ivrange, newdf$pred, col = col[i])
        newdf$Mind <- i
        newdat[[i]] <- newdf
    }
    newdata <- do.call("rbind", newdat)
    invisible(newdata)
}



Mrdiag <- function(RE, data, id = "Mind", col = rainbow(nrow(RE))){
    if(missing(RE) || missing(data)) stop("Must specify RE and data")
    par.orig <- par(no.readonly = TRUE)

    RE <- zapsmall(RE, digits = 5)
    ##purge columns that contain only one unique value
    RE <- RE[ , which(lapply(apply(RE, 2,  unique), length) > 1)]
    REnam <- colnames(RE)
    RE.ncol <- length(REnam)
    dev.new()
    par(mfrow = c( RE.ncol - 1, 2)) ##ignore intercept

    for (i in 2:RE.ncol){
        plot(RE[, 1], RE[ , REnam[i]], xlab = paste("Random Effect on: ", id), ylab = paste("Random Effect on: ", REnam[i], sep=""),  main = "True Random Effects", col = col)


        plot(RE[ , 1][dat$Mind], data[ , REnam[i]] * RE[ , REnam[i]][data[, id]], col = col[data[ ,id]], main = "Marginal Random Effects", xlab = paste("Random Effect on: ", id), ylab = paste("Marginal Slope Random Effect (b*", REnam[i], ")", sep=""))
    }
    par(par.orig)

    dev.new()
    require(lme4)
    fake.ranef.mer <- list(Mind = as.data.frame(RE))
    colnames(fake.ranef.mer$Mind) <- gsub( "x", "b", colnames(fake.ranef.mer$Mind))
    print(lme4:::dotplot.ranef.mer(fake.ranef.mer, main="True Random Effects"))

    require(rockchalk)
    list(summarize = rockchalk::summarizeNumerics(RE), cor = cor(RE))
}



###############################
### "Demonstration starts here!"
###############################

library(rockchalk) ## for summarize
library(lme4) ## for lmer and lmList
library(corpcor) ## needed for vec2sm





## Xcorr: Correlations among the X columns.
## Specify values of the lower triangular section of the correlation matrix.
## Just look, you'll see what I mean.
Xcorr <- vec2sm(c(0.4, 0.3, 0.0))
diag(Xcorr) <- 1
Xcorr ## just look what you got

## The true fixed effect values of the regression intercept and slopes
beta <- c(0.4, 0.3, -0.1, -1.1)
STDEE <- 5

## Correlations among random effects for columns, first is intercept.
STDEb <- rep(0, length(beta)) ##default effects 0
STDEb[1] <- 10 ## the cluster-level intercept random effect,
STDEb[2] <- .005  ## x1 slope disturbance
## Create a symmetric correlation matrix from the vech
bcorr <- vec2sm(c(0.2, 0, 0, 0, 0, 0))
diag(bcorr) <- 1

M <- 4
Nmin <- 1000
Nvar <- 0 ## balanced data

gd <- genData(M = M, Nmin = Nmin, Nvar = Nvar, beta = beta,
              Xmeans = c(10, 20, 100), Xcorr = Xcorr,
              STDEE = STDEE, STDEb = STDEb, bcorr = bcorr, seed = 441123)
dat <- gd$dat
Mreffects <- gd$Mreffects


## get M unique gray colors, will use later for plotting
## mycolors <- gray.colors(M)
## Want more colorful mycolors ?
mycolors <- rainbow(M)



## Fit separate linear models, one for each cluster
m3list <- lmList(y3 ~ x1 + x2 + x3 | Mind, data=dat, pool = FALSE)

## Plot those. Fun!
m3list.newdat <- spaghettiPlot.lmList(m3list,  iv="x1", cluster = "Mind",  dat = dat, col = mycolors)

## Look over the random effects, row by row.
Mrdiag(Mreffects, data = dat)

###masquerade Mreffects as a ranef.mer, in order to use dotplot from lme4
fake.ranef.mer <- list(Mind = as.data.frame(Mreffects[ ,c("b0","b1")]))
lme4:::dotplot.ranef.mer(fake.ranef.mer, main="True Random Effects")

fake.ranef.mer <- list(Mind = as.data.frame(Mreffects))
lme4:::dotplot.ranef.mer(fake.ranef.mer, main="True Random Effects")


## Run the following a few times, changing M and Nmin
gd2 = genData(M=50, Nmin = 20, Nvar = 0, Xmeans = c(10, 20, 100),
              Xcorr = Xcorr, beta = beta,
              STDEE = 159, STDEb = STDEb, bcorr = bcorr)
Mrdiag(gd2$Mreffects, data=gd2$dat)





beta <- c(0.4, 0.3, -0.1, -1.1)
STDEb <- rep(0, length(beta)) ##default effects 0
STDEb[1] <- 10 ## the intercept,
STDEb[2] <- .5  ## x1 slope disturbance
STDEb[3] <- .2
## Create a symmetric correlation matrix from the vech
bcorr <- vec2sm(c(0.2, 0.1, 0.5, 0, 0, 0))
diag(bcorr) <- 1

M <- 40
Nmin <- 20
Nvar <- 0 ## balanced data

gd <- genData(M = M, Nmin = Nmin, Nvar = Nvar, beta = beta,
              Xmeans = c(10, 20, 100), Xcorr = Xcorr,
              STDEE = 159, STDEb = STDEb, bcorr = bcorr, seed = 441123)
dat <- gd$dat
Mreffects <- gd$Mreffects

m3list <- lmList(y3 ~ x1 + x2 + x3 | Mind, data=gd$dat, pool = FALSE)

m3list.newdat <- spaghettiPlot.lmList(m3list,  iv="x1", cluster = "Mind",  dat = dat)
Mrdiag(gd$Mreffects, data=gd$dat)






## bcorr <- vec2sm(c(0.99,0,0,0,0, 0))
## diag(bcorr) <- 1
## ##leave more defaults at work
## gd <- genData( M=233, Nmin = 10, Nvar = 0, Xcorr = Xcorr, STDEE = 15,
##               STDEb = c(0.5, 0.1, 0, 0), bcorr = bcorr, seed=234)
## dat <- gd$dat
## Mreffects <- gd$Mreffects

## m3list <- lmList(y3 ~ x1 + x2 + x3 | Mind, data=dat, pool = FALSE)

## ## Plot those. Fun!
## spaghettiLMplot(m3list,  iv="x1", cluster = "Mind",  dat= dat)

## ## Look over the random effects, row by row.
## Mrdiag(Mreffects)

## Mrdiag(Mreffects, "b2")

## Start with regressions that ignore the clustering
## y1 has no random effects, m1.1 should be OK
m1.1 <- lm(y1 ~ x1 + x2 + x3, data=dat)
summary(m1.1)

## y2 has random intercepts
m1.2 <- lm(y2 ~ x1 + x2 + x3, data=dat)
summary(m1.2)

## y3 has random intercepts and random slopes
m1.3 <- lm(y3 ~ x1 + x2 + x3, data=dat)
summary(m1.3)

## The fixed effects "dummy variable" regression.
m2.1 <- lm(y3 ~ x1 + x2 + x3 + as.factor(Mind), data=dat)
summary(m2.1)
m3list.newdat$m2.1 <-predict(m2.1, newdata = m3list.newdat)


## The fixed effects "dummy variable" regression with ONLY THE
## "correct" interaction included
m2.2 <- lm(y3 ~  x2 + x3 + x1 * as.factor(Mind), data=dat)
summary(m2.2) ## cool
m3list.newdat$m2.2 <-predict(m2.2, newdata = m3list.newdat)

## The fixed effects "dummy variable" regression with interaction
## for all of the x's.
m2.3 <- lm(y3 ~  (x2 + x3 + x1) * as.factor(Mind), data=dat)
summary(m2.3) ## cool
m3list.newdat$m2.3 <-predict(m2.3, newdata = m3list.newdat)

## If a cluster has 3 or fewer cases, this warning will appear
## Warning message:
## In predict.lm(m3list[[i]], newdata = data.frame(x1 = x1range, x2 = mean(m3mf$x2),  :
##   prediction from a rank-deficient fit may be misleading


## Draw new graphs on a new device, so we can compare
dev.new()

##
## Estimate this as a mixed-effects model with lme4

## mm2: Just the random intercept, no random slope
mm2 <- lmer( y2 ~ x1 + x2 + x3 + (1 | Mind), data=dat)
summary(mm2)
m3list.newdat$mm2 <- predict(mm2, newdata = m3list.newdat)

mm2VarCorr <- VarCorr(mm2)
mm2VarCorr

cor(fitted(mm2), dat$y2)

## Both random intercept and random slope, not correlated with each other
## Depending on bcorr, this may be a 'wrong model'.
mm3 <- lmer( y3 ~ x1 + x2 + x3 + (1|Mind) + (0 + x1 | Mind), data=dat, verbose=1)
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
mm3reStdDev[["Residual"]] - STDEE

## The "right" model allows the random effects to be correlated (Supposing
## bcorr not 0).
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
#summarize(mm4ranef$Mind)
summary(mm4raner$Mind)
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
plot(beta[2] + Mreffects[ ,2], m3x1b, xlab="Cluster Slopes (True Values)", ylab="Estimates of Slopes (across Clusters)", col="gray70", xlim= c(1.04,1.04)*range(c(beta[2] + Mreffects[ ,2], m3x1b)), ylim=c(1.04,1.04)*range((c(beta[2] + Mreffects[ ,2], m3x1b))))
points(beta[2] + Mreffects[ ,2], mm4x1b, , col="black", pch=13)
legend("topleft", c("OLS Slopes", "Mixed Model Slopes"), pch=c(1,13), col=c("gray70","black"))



## Plot the estimated intercept effects against the "true" random effects from Mreffects
plot(beta[1] + Mreffects[ ,1], m3b0, xlab="Cluster Intercepts (True Values)", ylab="Estimates of Intercepts", col="gray70")
points(beta[1] + Mreffects[ ,1], mm4b0 , col="black", pch=13)
legend("topleft", c("OLS Intercepts", "Mixed Model Intercepts"), pch=c(1,13), col=c("gray70","black"))


plot(Mreffects[, 1], m3b0)

plot(Mreffects[, 1], mm4b0)



lme4:::dotplot.ranef.mer

